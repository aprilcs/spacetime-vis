###### Models ######

library(maptools)
library(spdep)

IreTown <- read.table("IRE-SPP1.txt", header=T)

# IreTown <- readShapeSpatial(fn="Towns point data")
# IreTown <-
##### Model morbidity by first day of outbreak #####
#### Poisson count regression ####
# First.day.glm <- glm(No..of.cas ~ Day.of.fir + offset(log(Total.popu)), family=poisson, data=IreTown)

IreTown$Cases.per.pop <- IreTown$CASES / IreTown$POP
Education.glm <- glm (IreTown$CASES ~ IreTown$Percent_illiterate_avg + offset(log(IreTown$POP)), family=poisson, data=IreTown)

summary(Education.glm)
#
# Call:
#  glm(formula = IreTown$CASES ~ IreTown$Percent_illiterate_avg + 
#        offset(log(IreTown$POP)), family = poisson, data = IreTown)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -25.546  -11.479   -6.321    3.863   50.033  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                    -4.6076050  0.0188025 -245.05   <2e-16 ***
#  IreTown$Percent_illiterate_avg  0.0270157  0.0004527   59.68   <2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
# (Dispersion parameter for poisson family taken to be 1)
#
# Null deviance: 27293  on 135  degrees of freedom
# Residual deviance: 23765  on 134  degrees of freedom
# AIC: 24454
#
#Number of Fisher Scoring iterations: 5

# 24454/134
# overdispersion is present

#### Spatial generalized linear mixed models #####
library(geoR)

IreMap <- readShapePoly(fn="Ireland superfile1")
Encoding(levels(IreMap$NAME_1)) <- "latin1"
nrow(IreMap)

IrePoint <- read.table("Ireland county.txt", header=T)
nrow(IrePoint)

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

head(IrePoint)
#### Spatial Generalized Linear Mixed Model ####

Literacy.morb.glmm<- glmmPQL(CASES ~ X._illiterate + north + east + offset(log(POP)), random= ~1 | ID_1, correlation = ispcor, family=poisson)
summary(Literacy.morb.glmm)
