setwd("C:/Users/April/Desktop/MSc project/github/spacetime-vis")

library(maptools)
library(spdep)

IreMap <- readShapeSpatial(fn="Ireland choropleth")

plot(IreMap, border = "black", lwd = 1)

Ire.nb <- poly2nb(IreMap, queen = FALSE)
str(Ire.nb)

coordinates(IreMap)

text(coordinates(IreMap), labels=c(1:51), cex=0.8)

# generate distance based neighbourhoods of 50km and 100km and 120km
# Ire.nb.20km <- dnearneigh(coordinates(IreMap), d1 = 0, d2 = 20000, longlat=NULL)
# Ire.nb.50km <- dnearneigh(coordinates(IreMap), d1 = 0, d2 = 50000, longlat=NULL)
# Ire.nb.100km <- dnearneigh(coordinates(IreMap), d1 = 0, d2 = 100000, longlat=NULL)
# Ire.nb.120km <- dnearneigh(coordinates(IreMap), d1 = 0, d2 = 120000, longlat=NULL)

##### Analysis of disease clustering with regional data #####

IrePoint <- read.table("Ireland choropleth.txt", header=T)

names(IrePoint)
#[1] "ID_1"       "NAME_1"     "POP"        "CASES"      "DEATHS"     "Cases.Pop"  "Case_fatal"

my.dat <- as.data.frame(coordinates(IreMap))
names(my.dat) <- c("x", "y")

my.dat$POP <- IrePoint$POP
my.dat$CASES <- IrePoint$CASES
my.dat$raw <- my.dat$CASES / (my.dat$POP + 1)

#library(stats)
#na.fail(my.dat$raw)

moran.test(my.dat$raw, nb2listw(Ire.nb), alternative = "two.sided")

# Moran's I test under randomisation
#
# data:  my.dat$raw  
# weights: nb2listw(Ire.nb)  
#
# Moran I statistic standard deviate = 3.4118, p-value = 0.0006454
# alternative hypothesis: two.sided
# sample estimates:
# Moran I statistic       Expectation          Variance 
#      0.294231547      -0.019607843       0.008461517 

#### Smoothing #####

Cluster.smooth <- EBest(my.dat$CASES, my.dat$POP + 1, family="binomial")

my.dat$raw <- Cluster.smooth$raw
my.dat$estmm <- Cluster.smooth$estmm
head(my.dat)

moran.test(my.dat$estmm, nb2listw(Ire.nb), alternative = "two.sided")

# Moran's I test under randomisation
#
# data:  my.dat$estmm  
# weights: nb2listw(Ire.nb)  
#
# Moran I statistic standard deviate = 3.1921, p-value = 0.001412
# alternative hypothesis: two.sided
# sample estimates:
# Moran I statistic       Expectation          Variance 
#      0.273829622      -0.019607843       0.008450203 

geary.test(my.dat$raw, nb2listw(Ire.nb))
geary.test(my.dat$estmm, nb2listw(Ire.nb))

# Geary's C test under randomisation
#
# data:  my.dat$estmm 
# weights: nb2listw(Ire.nb) 
# 
# Geary C statistic standard deviate = 2.8428, p-value = 0.002236
# alternative hypothesis: Expectation greater than statistic
# sample estimates:
# Geary C statistic       Expectation          Variance 
#      0.716519425       1.000000000       0.009943568 

##### how far observations are dependent ####

Ire.acf <- sp.correlogram(Ire.nb, my.dat$estmm, order=6, method="I", zero.policy=TRUE)
print(Ire.acf)
plot(Ire.acf)

##### ANALYSIS OF DISEASE CLUSTERING WITH SPATIAL POINT PATTERN DATA ########

pts <- read.table("IRE-SPP.txt", header=T)
ptsp <- pts[pts$CASES >= 1,]
ptsn <- pts[pts$CASES == 0,]
case <- list (x=ptsp$x , y=ptsp$y)
cont <- list(x=ptsn$x, y=ptsn$y)

Nrep <- 999
k <- (1:15)
out <- TangoCNN.index(case, cont, s=k, Nrep)
out$p.value
# [1] 0.007

######## D-function #######

plot(runif(50, min=0, max=1), runif(50, min=0, max=1), pch=16, col="blue")

pts <- read.table("IRE-SPP.txt", header=T)
ptsp <- pts[pts$CASES >= 1,]
ptsn <- pts[pts$CASES == 0,]

library(maptools)

IreMap <- readShapeSpatial(fn="Ireland choropleth")
plot(IreMap)
points(ptsn$x , ptsn$y, cex=0.6, pch=16, col="blue")
points(ptsp$x , ptsp$y, cex=0.6, pch=16, col="red")

### Transform the data into points object ####

library(splancs)
pts.p <- as.points(ptsp$x, ptsp$y)
pts.n <- as.points(ptsn$x, ptsn$y)
pts.all <- as.points(pts$x, pts$y)
pts.poly <- read.table(file.choose(), header=T)
  
Ire.bdr <- read.table(file.choose(), header=T) # C:\Users\April\Desktop\MSc project\github\spacetime-vis\ireland_boundary_monster_out (2)

#### Estimate k-functions #####

s <- seq(from=0, to=150000, length=21)
pts.poly <- as.points(pts.poly)
Kpos <- khat(pts.p, pts.poly, s)