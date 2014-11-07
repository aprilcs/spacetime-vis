##### THIS CODE WORKS #####

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

IreMap <- readShapePoly(fn="Ireland superfile1")
Encoding(levels(IreMap$NAME_1)) <- "latin1"

IrePoint <- read.table("Ireland county.txt", header=T)

##################################################################
## Map
##################################################################

#library(colorspace)  

#pdf(file="figs/population.pdf")
#quantPal <- rev(heat_hcl(16))
#spplot(IreMap["POP"], col='transparent', col.regions=quantPal, xlab="Total Population")
#dev.off()

#pdf(file="figs/cases.pdf")
#quantPal <- rev(heat_hcl(16))
#spplot(IreMap["CASES"], col='transparent', col.regions=quantPal, xlab="Total Cases")
#dev.off()

#pdf(file="figs/cases per pop.pdf")
#quantPal <- rev(heat_hcl(16))
#spplot(IreMap["Cases.Pop"], col='transparent', col.regions=quantPal, xlab="Cases per 100 000")
#dev.off()

#pdf(file="figs/case fatality.pdf")
#quantPal <- rev(heat_hcl(16))
#spplot(IreMap["Case.fatal"], col='transparent', col.regions=quantPal, xlab="Case Fatality Rate")
#dev.off()

#### Propotional point mapping #####

# pdf(file="figs/proportional mortality.pdf")
# quantPal <- rev(terrain_hcl(1))
# spplot(IreMap["POP"], col='transparent', col.regions=quantPal)
# airPal <- colorRampPalette(c('springgreen1', 'sienna3', 'gray5'))(5)
# spplot(IrePoint["Rate.of.mo"], col.regions=airPal, cex=sqrt(1:5), add=TRUE, edge.col='black', scales=list(draw=TRUE), key.space='right')
# dev.off()

#### Olaf's style ######
#### Smoothed cases per 100 000 people ######

library(maptools)
library(spdep)
library(colorspace) 

head(IrePoint)

IrePoint$inc <- 100000*(IrePoint$CASES) / (IrePoint$POP)
Bayes.IrePoint <- EBest((IrePoint$CASES), (IrePoint$POP))
windows()
boxplot(100000*Bayes.IrePoint, col="blue")
IrePoint$sinc <- Bayes.IrePoint$estmm*100000

qtls <- quantile (IrePoint$sinc, probs=seq(0,1,0.20))
IrePoint$cols <- findInterval(IrePoint$sinc, qtls, all.inside=T)

colours <- rev(heat_hcl(5))
#colours <- c(colours[16:1])
windows()
plot(IreMap, axes=T, xlab="Easting", ylab="Northing", 
     col=colours[IrePoint$cols])

brks <- round (qtls,1)
min(brks); max(brks); leglabs(brks)
# [1] 20.5
# [1] 8550.7
# [1] "under 1269.8"    "1269.8 - 2325.9" "2325.9 - 2844.4" "2844.4 - 3403.2"
# [5] "over 3403.2"  
my.legend <- c("20.5 - 1269.8", "1269.8 - 2325.9", "2325.9 - 2844.4", "2844.4 - 3403.2", "3403.2 - 8550.7")
legend(x= 300000, y = 6150000, legend = my.legend[5:1], cex=0.8, title= " Cases of cholera per 100 000", fill=colours[5:1])

##### Get coordinates #####

library(geoR)
test<- as.geodata(cbind(as.data.frame(coordinates(IreMap)), IrePoint$inc))
windows()
plot.geodata(test)

IrePoint$east <- coordinates(IreMap) [,1]
IrePoint$north <- coordinates(IreMap) [,2]

#### Figure 1 map of Ireland with counties and towns ####

windows()
colours <- rev(terrain.colors(1))
title(main=NULL, sub="Figure 1. County boundaries of Ireland during study period (1848 - 1850).")

plot(IreMap, axes=T, #xlab="Easting", ylab="Northing", 
     col=colours, lty=3)
Ire.bound <- readShapeLines(fn="Poly to line4UTM")
plot(Ire.bound, add=TRUE)
head(IrePoint)
text(IrePoint$east, IrePoint$north, IrePoint$NAME_1, col="white", cex=.4, font=1)
text(IrePoint$east, IrePoint$north, IrePoint$NAME_1, col="white", cex=.408, font=1)
text(IrePoint$east, IrePoint$north, IrePoint$NAME_1, col="black", cex=.4, font=2)

##### Not smoothed cholera per 100 000 ####

qtls <- quantile (IrePoint$inc, probs=seq(0,1,0.20))
IrePoint$cols <- findInterval(IrePoint$inc, qtls, all.inside=T)

colours <- rev(heat_hcl(5))
#colours <- c(colours[16:1])
windows()
plot(IreMap, axes=T, xlab="Easting", ylab="Northing", 
     col=colours[IrePoint$cols])

brks <- round (qtls,1)
min(brks); max(brks); leglabs(brks)
#[1] 0
#[1] 8562.5
#[1] "under 0"         "0 - 1222"        "1222 - 2651.4"   "2651.4 - 3598.7" "over 3598.7"  
my.legend <- c("under 450.1", "450.1 - 2064.3", "2064.3 - 2845.3", "2845.3 - 3406.6", "over 3406.6")
legend(x= 300000, y = 6150000, legend = my.legend[5:1], cex=0.8, title= " Cases of cholera per 100 000", fill=colours[5:1])

head(IrePoint)

##### Literacy ####

qtls <- quantile (IrePoint$X._illiterate, probs=seq(0,1,0.20))
IrePoint$cols <- findInterval(IrePoint$X._illiterate, qtls, all.inside=T)

colours <- rev(heat_hcl(5))
#colours <- c(colours[16:1])
windows()
plot(IreMap, axes=T, xlab="Easting", ylab="Northing", 
     col=colours[IrePoint$cols])

brks <- round (qtls,1)
min(brks); max(brks); leglabs(brks)

my.legend <- c("20 - 38" ,   "38 - 42.4" ,  "42.4 - 47.6", "47.6 - 59.8", "59.8 - 74")
legend(x= 300000, y = 6150000, legend = my.legend[5:1], cex=0.8, title= " Percent illiterate", fill=colours[5:1])

##### Number of families per 100 houses #####

qtls <- quantile (IrePoint$No_families_per_100_houses, probs=seq(0,1,0.20))
IrePoint$cols <- findInterval(IrePoint$No_families_per_100_houses, qtls, all.inside=T)

colours <- rev(heat_hcl(5))
#colours <- c(colours[16:1])
windows()
plot(IreMap, axes=T, xlab="Easting", ylab="Northing", 
     col=colours[IrePoint$cols])

brks <- round (qtls,1)
min(brks); max(brks); leglabs(brks)

my.legend <- c("105 - 107" ,  "107 - 107.4", "107.4 - 110", "110 - 113"  , "113 - 121")
legend(x= 300000, y = 6150000, legend = my.legend[5:1], cex=0.8, title= " No. of families per 100 houses", fill=colours[5:1])

##### Average number of persons to a family #####

qtls <- quantile (IrePoint$Av_no_persons_to_family, probs=seq(0,1,0.20))
IrePoint$cols <- findInterval(IrePoint$Av_no_persons_to_family, qtls, all.inside=T)

colours <- rev(heat_hcl(5))
#colours <- c(colours[16:1])
windows()
plot(IreMap, axes=T, xlab="Easting", ylab="Northing", 
     col=colours[IrePoint$cols])

brks <- round (qtls,1)
min(brks); max(brks); leglabs(brks)

my.legend <- c("5.1 - 5.3" ,"5.3 - 5.4" ,"5.4 - 5.5" ,"5.5 - 5.7", "5.7 - 6.2")
legend(x= 300000, y = 6150000, legend = my.legend[5:1], cex=0.8, title= " Average size of family", fill=colours[5:1])

#### Number of persons per square mile ####

qtls <- quantile (IrePoint$No_persons_sq_mile, probs=seq(0,1,0.20))
IrePoint$cols <- findInterval(IrePoint$No_persons_sq_mile, qtls, all.inside=T)

colours <- rev(heat_hcl(5))
#colours <- c(colours[16:1])
windows()
plot(IreMap, axes=T, xlab="Easting", ylab="Northing", 
     col=colours[IrePoint$cols])

brks <- round (qtls,1)
min(brks); max(brks); leglabs(brks)

my.legend <- c("122 - 147.8"   ,"147.8 - 170.8" ,"170.8 - 196.6", "196.6 - 229.6", "229.6 - 422")
legend(x= 300000, y = 6150000, legend = my.legend[5:1], cex=0.8, title= " No. of people per mile^2", fill=colours[5:1])

##### Percent families in 4th class houses ####

qtls <- quantile (IrePoint$X._families_in_4th_class_houses, probs=seq(0,1,0.20))
IrePoint$cols <- findInterval(IrePoint$X._families_in_4th_class_houses, qtls, all.inside=T)

colours <- rev(heat_hcl(5))
#colours <- c(colours[16:1])
windows()
plot(IreMap, axes=T, xlab="Easting", ylab="Northing", 
     col=colours[IrePoint$cols])

brks <- round (qtls,1)
min(brks); max(brks); leglabs(brks)

my.legend <- c("3.5 - 9"  ,   "9 - 11.2"   , "11.2 - 14.9" ,"14.9 - 17.1" ,"17.1 - 31.6")
legend(x= 300000, y = 6150000, legend = my.legend[5:1], cex=0.8, title= " Percent of families in 4th class", fill=colours[5:1])

##### Percent change in population from 1841 - 1851 ####

qtls <- quantile (IrePoint$X._decrease_pop_1941.51, probs=seq(0,1,0.20))
IrePoint$cols <- findInterval(IrePoint$X._decrease_pop_1941.51, qtls, all.inside=T)

colours <- (heat_hcl(5))
#colours <- c(colours[16:1])
windows()
plot(IreMap, axes=T, xlab="Easting", ylab="Northing", 
     col=colours[IrePoint$cols])

brks <- round (qtls,1)
min(brks); max(brks); leglabs(brks)

my.legend <- c("-27.9 - -31.6"  , "-24.1 - -27.9" ,"-20.3 - -24.1" ,"-14.3 - -20.3", "-14.3 - +29.4")
legend(x= 300000, y = 6150000, legend = my.legend[5:1], cex=0.8, title= "Percent population change 1941-51", fill=colours[5:1])
text(IrePoint$east, IrePoint$north, IrePoint$X._decrease_pop_1941.51, col="white", cex=.7, font=2)
text(IrePoint$east, IrePoint$north, IrePoint$X._decrease_pop_1941.51, col="white", cex=.708, font=2)
text(IrePoint$east, IrePoint$north, IrePoint$X._decrease_pop_1941.51, col="black", cex=.7, font=2)


##### Percent sick people ####

qtls <- quantile (IrePoint$X._sick, probs=seq(0,1,0.20))
IrePoint$cols <- findInterval(IrePoint$X._sick, qtls, all.inside=T)

colours <- rev(heat_hcl(5))
#colours <- c(colours[16:1])
windows()
plot(IreMap, axes=T, xlab="Easting", ylab="Northing", 
     col=colours[IrePoint$cols])

brks <- round (qtls,1)
min(brks); max(brks); leglabs(brks)

my.legend <- c("0.9 - 1.2" ,"1.2 - 1.4" ,"1.4 - 1.4" ,"1.4 - 2"  , "2 - 2.6")
legend(x= 300000, y = 6150000, legend = my.legend[5:1], cex=0.8, title= " Percent of families in 4th class", fill=colours[5:1])

##### Famine relief FOOD #####
head(IrePoint)
qtls <- quantile (IrePoint$Relief_food_tons_46.48, probs=seq(0,1,0.20))
IrePoint$cols <- findInterval(IrePoint$Relief_food_tons_46.48, qtls, all.inside=T)

colours <- rev(heat_hcl(5))
#colours <- c(colours[16:1])
windows()
plot(IreMap, axes=T, xlab="Easting", ylab="Northing", 
     col=colours[IrePoint$cols])

brks <- round (qtls,1)
min(brks); max(brks); leglabs(brks)

my.legend <- c("8 - 35"   ,   "35 - 56.2"    , "56.2 - 124.8" , "124.8 - 384.2","384.2 - 1866")
legend(x= 300000, y = 6150000, legend = my.legend[5:1], cex=0.8, title= " Famine relief in food (tons) 1846-48", fill=colours[5:1])

##### Total famine relief MONEY #####

head(IrePoint)
qtls <- quantile (IrePoint$Relief_money_sterling_46.48, probs=seq(0,1,0.20))
IrePoint$cols <- findInterval(IrePoint$Relief_money_sterling_46.48, qtls, all.inside=T)

colours <- rev(heat_hcl(5))
#colours <- c(colours[16:1])
windows()
plot(IreMap, axes=T, xlab="Easting", ylab="Northing", 
     col=colours[IrePoint$cols])

brks <- round (qtls,1)
min(brks); max(brks); leglabs(brks)

my.legend <- c("5 - 113.6"   ,"113.6 - 214.2", "214.2 - 407.8" ,"407.8 - 981.8"
               ,"981.8 - 4490")
legend(x= 300000, y = 6150000, legend = my.legend[5:1], cex=0.8, title= " Famine relief in money (sterling) 1846-48", fill=colours[5:1])

#### Famine relief in SEED #####

head(IrePoint)
qtls <- quantile (IrePoint$Relief_seed_pounds_46.48, probs=seq(0,1,0.20))
IrePoint$cols <- findInterval(IrePoint$Relief_seed_pounds_46.48, qtls, all.inside=T)

colours <- rev(heat_hcl(5))
#colours <- c(colours[16:1])
windows()
plot(IreMap, axes=T, xlab="Easting", ylab="Northing", 
     col=colours[IrePoint$cols])

brks <- round (qtls,1)
min(brks); max(brks); leglabs(brks)

my.legend <- c("0 - 12.4"  ,    "12.4 - 474"   ,   "474 - 1798.2"  ,  "1798.2 - 7726.6"
  ,"7726.6 - 54172")
legend(x= 300000, y = 6150000, legend = my.legend[5:1], cex=0.8, title= " Famine relief in seed (lbs) 1846-48", fill=colours[5:1])



