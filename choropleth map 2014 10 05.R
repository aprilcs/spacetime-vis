setwd("C:/Users/April/Desktop/MSc project/github/spacetime-vis")

library(lattice)
library(ggplot2)
library(latticeExtra)
library(gridBase)
library(gridExtra)

myTheme <- custom.theme.2(pch=19, cex=0.7,
                          region=rev(brewer.pal(9, 'YlOrRd')),
                          symbol = brewer.pal(n=8, name = "Dark2"))
myTheme$strip.background$col='transparent'
myTheme$strip.shingle$col='transparent'
myTheme$strip.border$col='transparent'

xscale.components.custom <- function(...){
  ans <- xscale.components.default(...)
  ans$top=FALSE
  ans}
yscale.components.custom <- function(...){
  ans <- yscale.components.default(...)
  ans$right=FALSE
  ans}
myArgs <- list(as.table=TRUE,
               between=list(x=0.5, y=0.2),
               xscale.components = xscale.components.custom,
               yscale.components = yscale.components.custom)
defaultArgs <- lattice.options()$default.args

lattice.options(default.theme = myTheme,
                default.args = modifyList(defaultArgs, myArgs))

##################################################################
## Choropleth maps
##################################################################


##################################################################
## Administrative boundaries
##################################################################

library(sp)
library(maptools)

#old <- setwd(tempdir())

IreMap <- readShapePoly(fn="Ireland choropleth")
Encoding(levels(IreMap$NAME_1)) <- "latin1"

IrePoint <- read.table("Ireland choropleth.txt", header=T)

##################################################################
## Map
##################################################################

library(colorspace)  

pdf(file="figs/population.pdf")
quantPal <- rev(heat_hcl(16))
spplot(IreMap["POP"], col='transparent', col.regions=quantPal, xlab="Total Population")
dev.off()

pdf(file="figs/cases.pdf")
quantPal <- rev(heat_hcl(16))
spplot(IreMap["CASES"], col='transparent', col.regions=quantPal, xlab="Total Cases")
dev.off()

pdf(file="figs/cases per pop.pdf")
quantPal <- rev(heat_hcl(16))
spplot(IreMap["Cases.Pop"], col='transparent', col.regions=quantPal, xlab="Cases per 100 000")
dev.off()

pdf(file="figs/case fatality.pdf")
quantPal <- rev(heat_hcl(16))
spplot(IreMap["Case.fatal"], col='transparent', col.regions=quantPal, xlab="Case Fatality Rate")
dev.off()

#### Propotional point mapping #####

pdf(file="figs/proportional mortality.pdf")
quantPal <- rev(terrain_hcl(1))
spplot(IreMap["POP"], col='transparent', col.regions=quantPal)
airPal <- colorRampPalette(c('springgreen1', 'sienna3', 'gray5'))(5)
spplot(IrePoint["Rate.of.mo"], col.regions=airPal, cex=sqrt(1:5), add=TRUE, edge.col='black', scales=list(draw=TRUE), key.space='right')
dev.off()

#### Olaf's style ######

library(maptools)
library(spdep)

head(IrePoint)

IrePoint$inc <- 100000*(IrePoint$CASES) / (IrePoint$POP + 1)
Bayes.IrePoint <- EBest((IrePoint$CASES), (IrePoint$POP + 1))
windows()
boxplot(100000*Bayes.IrePoint, col="blue")
IrePoint$sinc <- Bayes.IrePoint$estmm*100000

qtls <- quantile (IrePoint$sinc, probs=seq(0,1,0.20))
IrePoint$cols <- findInterval(IrePoint$sinc, qtls, all.inside=T)

colours <- rev(heat_hcl(5))
#colours <- c(colours[16:1])
windows()
plot(IreMap, axes=T, #xlab="Easting", ylab="Northing", 
     col=colours[IrePoint$cols])

brks <- round (qtls,1)
min(brks); max(brks); leglabs(brks)
#[1] 0
#[1] 8562.5
#[1] "under 0"         "0 - 1222"        "1222 - 2651.4"   "2651.4 - 3598.7" "over 3598.7"  
my.legend <- c("under 91.1","91.1 - 1311","1311 - 2720.2","2720.2 - 3592.6", "3592.6 - 8562.5")
legend(x= -10.8, y = 55.9, legend = my.legend[5:1], cex=0.8, title= " Cases of cholera per 100 000", fill=colours[5:1])

#pdf(file="figs/cases per pop.pdf")
#windows()
#quantPal <- rev(heat_hcl(16))
#spplot(IreMap["Cases.Pop"], col=colours[IreMap$cols], col.regions=quantPal)

dev.off()


###### Models ######

library(maptools)
library(spdep)

#IreTown <- readShapeSpatial(fn="Towns point data")

##### Model morbidity by first day of outbreak #####
#### Poisson count regression ####
First.day.glm <- glm(No..of.cas ~ Day.of.fir + offset(log(Total.popu)), family=poisson, data=IreTown)
summary(First.day.glm)

#Call:
#  glm(formula = No..of.cas ~ Day.of.fir + offset(log(Total.popu)), 
#      family = poisson, data = IreTown)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-24.310  -11.130   -6.794    2.993   59.228  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -3.999e+00  1.032e-02 -387.56   <2e-16 ***
#  Day.of.fir   4.002e-03  7.791e-05   51.36   <2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#(Dispersion parameter for poisson family taken to be 1)
#
#Null deviance: 27293  on 135  degrees of freedom
#Residual deviance: 24796  on 134  degrees of freedom
#AIC: 25485
#
#Number of Fisher Scoring iterations: 5

# 24796/134
# overdispersion is present

#### Spatial generalized linear mixed models #####
library(geoR)
test<- as.geodata(cbind(as.data.frame(coordinates(IreMap)), IrePoint$inc))
windows()
plot.geodata(test)

IrePoint$east <- coordinates(IreMap) [,1]
IrePoint$north <- coordinates(IreMap) [,2]

##### Set up a spatial correlation structure #####

attach(IrePoint)

library(MASS)
library(nlme)
spcor <- corSpatial (form= ~ east + north, type="exponential")
ispcor <- Initialize(spcor, data = IrePoint)

#### Spatial Generalized Linear Mixed Model ####

Mortality.glmm<- glmmPQL(CASES ~ north + east + offset(log(POP + 1)), random= ~1 | ID_1, correlation = ispcor, family=poisson)
summary(Mortality.glmm)



