setwd("C:/Users/April/Desktop/MSc project/github/spacetime-vis")

##### Cholera risk map ####

library(geoR)

IrePoint$Bayes.Cases <- EBest(IrePoint$CASES, IrePoint$POP + 1) [,"estmm"]

IreMap.geo <- as.geodata(cbind(IrePoint$east, IrePoint$north, IrePoint$Bayes.Cases))
windows()
plot.geodata(IreMap.geo)
win.graph(width = 6, height = 3, pointsize = 12)
par (mfrow=c(1,2), pty="s")
IreMap.svc <- variog(IreMap.geo, estimator.type = "modulus", op="cloud")
plot(IreMap.svc)
IreMap.esv <- variog(IreMap.geo, estimator.type = "modulus")
plot(IreMap.esv)

#### model-based kriging #####

IreMap.esv2 <- variog(IreMap.geo, estimator.type="modulus", max.dist = 2.5)
windows()
plot(IreMap.esv2)
IreMap.beta <- likfit(IreMap.geo, ini.cov.pars=c(0.05, 100000), trend = "cte", fix.nug=TRUE, cov.model="exponential")

lines.variomodel(IreMap.beta, lty=1, lwd=2, max.dist=2.5, col="red") 

IreMap.beta

# likfit: estimated model parameters:
# beta  sigmasq      phi 
# "0.0240" "0.0004" "0.2274" 
# Practical Range with cor=0.05 for asymptotic range: 0.6812445
#
# likfit: maximised log-likelihood = 130.3

library(rgdal)
library(gstat)

Ire.bdr <- read.table(file.choose(), header=T)

Ire.ply <- as.matrix(cbind(Ire.bdr$x, Ire.bdr$y))
plot(Ire.ply)
#Ire.bound1 <- readOGR(dsn="C:/Users/April/Desktop/MSc project/github/spacetime-vis", layer="Poly to line3")
#Ire.bound <- readOGR(dsn="C:/Users/April/Desktop/MSc project/github/spacetime-vis", layer="Ireland superfile")

#require(maptools)
krige.grid <- expand.grid(seq(min(Ire.bdr$x), max(Ire.bdr$x), l=100), seq(min(Ire.bdr$y), max(Ire.bdr$y), l=100))

#Ire.grid <- spsample(Ire.bound1, type="regular", n=1000, #cellsize=0.1,
                     #offset = c(0.5, 0.5))
windows()
plot(krige.grid)
 
Ire.uk <- krige.conv(IreMap.geo, krige = krige.control (type.krige = "ok", trend.d = "cte", obj.model = IreMap.beta), locations = krige.grid, borders = Ire.ply)

win.graph()
Ire.predict <- Ire.uk
Ire.predict$predict <- -Ire.predict$predict
image(Ire.predict, krige.grid, col = heat.colors(10))
Ire.100.predict <- Ire.uk
Ire.100.predict$predict <- 100*Ire.100.predict$predict
contour(Ire.100.predict, labcex=1.2, method = "edge", ad=T)

#my.legend <- c("1%","2%","3%","4%","5%", "6%", "7%", "8%", "9%", "10%")
#legend(x= -10.4, y = 55.6, legend = my.legend[10:1], ncol = 2, cex=0.8, title= " Predicted % cholera risk", fill= heat.colors(10))

##### Mortality risk map ######

library(geoR)

IrePoint$Bayes.Deaths <- EBest(IrePoint$DEATHS, IrePoint$POP + 1) [,"estmm"]

IreMap.geo <- as.geodata(cbind(IrePoint$east, IrePoint$north, IrePoint$Bayes.Deaths))
windows()
plot.geodata(IreMap.geo)
win.graph(width = 6, height = 3, pointsize = 12)
par (mfrow=c(1,2), pty="s")
IreMap.svc <- variog(IreMap.geo, estimator.type = "modulus", op="cloud")
plot(IreMap.svc)
IreMap.esv <- variog(IreMap.geo, estimator.type = "modulus")
plot(IreMap.esv)

#### model-based kriging #####

IreMap.esv2 <- variog(IreMap.geo, estimator.type="modulus", max.dist = 2.5)
windows()
plot(IreMap.esv2)
IreMap.beta <- likfit(IreMap.geo, ini.cov.pars=c(0.05, 100000), trend = "cte", fix.nug=TRUE, cov.model="exponential")
#IreMap.beta <- likfit(IreMap.geo, ini.cov.pars=c(1.0, 100000), trend = "cte", fix.nug=TRUE, cov.model="exponential")
lines.variomodel(IreMap.beta, lty=1, lwd=2, max.dist=2.5, col="red") 

IreMap.beta

#likfit: estimated model parameters:
#  beta  sigmasq      phi 
#"0.0112" "0.0001" "0.4801" 
#Practical Range with cor=0.05 for asymptotic range: 1.43824
#
#likfit: maximised log-likelihood = 183

library(rgdal)
library(gstat)

Ire.bdr <- read.table(file.choose(), header=T) # C:\Users\April\Desktop\MSc project\github\spacetime-vis\ireland_boundary_monster_out (2)

Ire.ply <- as.matrix(cbind(Ire.bdr$x, Ire.bdr$y))
plot(Ire.ply)
#Ire.bound1 <- readOGR(dsn="C:/Users/April/Desktop/MSc project/github/spacetime-vis", layer="Poly to line3")
#Ire.bound <- readOGR(dsn="C:/Users/April/Desktop/MSc project/github/spacetime-vis", layer="Ireland superfile")

#require(maptools)
krige.grid <- expand.grid(seq(min(Ire.bdr$x), max(Ire.bdr$x), l=100), seq(min(Ire.bdr$y), max(Ire.bdr$y), l=100))

#Ire.grid <- spsample(Ire.bound1, type="regular", n=1000, #cellsize=0.1,
#offset = c(0.5, 0.5))
windows()
plot(krige.grid)

Ire.uk <- krige.conv(IreMap.geo, krige = krige.control (type.krige = "ok", trend.d = "cte", obj.model = IreMap.beta), locations = krige.grid, borders = Ire.ply)

win.graph()
Ire.predict <- Ire.uk
Ire.predict$predict <- -Ire.predict$predict
image(Ire.predict, krige.grid, col = heat.colors(10))
Ire.100.predict <- Ire.uk
Ire.100.predict$predict <- 100*Ire.100.predict$predict
contour(Ire.100.predict, labcex=1.2, method = "edge", ad=T)

#my.legend <- c("0.5%","1.0%","1.5%","2.0%","2.5%", "3.0%", "3.5%", "4.0%", "4.5%", "5.0%")
#legend(x= -10.4, y = 55.6, legend = my.legend[10:1], ncol = 2, cex=0.8, title= " Predicted % mortality risk", fill= heat.colors(10))

###### Case fatality map #####

library(geoR)

IrePoint$Bayes.Casefat <- EBest(IrePoint$DEATHS, IrePoint$CASES) [,"estmm"]

IreMap.geo <- as.geodata(cbind(IrePoint$east, IrePoint$north, IrePoint$Bayes.Casefat))
windows()
plot.geodata(IreMap.geo)
win.graph(width = 6, height = 3, pointsize = 12)
par (mfrow=c(1,2), pty="s")
IreMap.svc <- variog(IreMap.geo, estimator.type = "modulus", op="cloud")
plot(IreMap.svc)
IreMap.esv <- variog(IreMap.geo, estimator.type = "modulus")
plot(IreMap.esv)

#### model-based kriging #####

IreMap.esv2 <- variog(IreMap.geo, estimator.type="modulus", max.dist = 2.5)
windows()
plot(IreMap.esv2)
IreMap.beta <- likfit(IreMap.geo, ini.cov.pars=c(0.05, 100000), trend = "cte", fix.nug=TRUE, cov.model="exponential")
#IreMap.beta <- likfit(IreMap.geo, ini.cov.pars=c(1.0, 100000), trend = "cte", fix.nug=TRUE, cov.model="exponential")
lines.variomodel(IreMap.beta, lty=1, lwd=2, max.dist=2.5, col="red") 

IreMap.beta

#likfit: estimated model parameters:
#  beta  sigmasq      phi 
#"0.4424" "0.0038" "0.2705" 
#Practical Range with cor=0.05 for asymptotic range: 0.8104928
#
#likfit: maximised log-likelihood = 75.32

library(rgdal)
library(gstat)

Ire.bdr <- read.table(file.choose(), header=T) # C:\Users\April\Desktop\MSc project\github\spacetime-vis\ireland_boundary_monster_out (2)

Ire.ply <- as.matrix(cbind(Ire.bdr$x, Ire.bdr$y))
plot(Ire.ply)
#Ire.bound1 <- readOGR(dsn="C:/Users/April/Desktop/MSc project/github/spacetime-vis", layer="Poly to line3")
#Ire.bound <- readOGR(dsn="C:/Users/April/Desktop/MSc project/github/spacetime-vis", layer="Ireland superfile")

#require(maptools)
krige.grid <- expand.grid(seq(min(Ire.bdr$x), max(Ire.bdr$x), l=100), seq(min(Ire.bdr$y), max(Ire.bdr$y), l=100))

#Ire.grid <- spsample(Ire.bound1, type="regular", n=1000, #cellsize=0.1,
#offset = c(0.5, 0.5))
windows()
plot(krige.grid)

Ire.uk <- krige.conv(IreMap.geo, krige = krige.control (type.krige = "ok", trend.d = "cte", obj.model = IreMap.beta), locations = krige.grid, borders = Ire.ply)

win.graph()
Ire.predict <- Ire.uk
Ire.predict$predict <- -Ire.predict$predict
image(Ire.predict, krige.grid, col = heat.colors(10))
Ire.100.predict <- Ire.uk
Ire.100.predict$predict <- 100*Ire.100.predict$predict
contour(Ire.100.predict, labcex=1.2, method = "edge", ad=T)

#my.legend <- c("35%","40%","45%","50%","55%", "60%", "65%", "70%", "75%", "80%")
#legend(x= -10.4, y = 55.6, legend = my.legend[10:1], ncol = 2, cex=0.8, title= " Predicted % case fatality risk", fill= heat.colors(10))
