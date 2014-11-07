setwd("C:/Users/April/Desktop/MSc project/github/spacetime-vis")

####### Point map #######
###### Presence / absence map ######

pts <- read.table("IRE-SPP1.txt", header=T)
ptsp <- pts[pts$CASES >= 1,]
ptsn <- pts[pts$CASES == 0,]

library(maptools)

IreMapBound <- readShapeSpatial(fn="Poly to line4UTM")

win.graph()

plot(IreMapBound, axes=T)
points(ptsn$x , ptsn$y, cex=0.6, pch=16, col="blue")
points(ptsp$x , ptsp$y, cex=0.6, pch=16, col="red")
colours <- c("red", "blue")

my.legend <- c("Cholera present","Cholera absent")
legend(x= 310000, y = 6145000, pch=16, legend = my.legend[1:2], cex=0.8, title= " Cholera presence or absence", col=colours[1:2])

#### Bubble map ######
#### Total Cases #####

win.graph()

plot(IreMapBound, axes=T)

radius <- sqrt(ptsp$CASES/ pi )
points(ptsn$x , ptsn$y, cex=0.6, pch=16, col="blue")
symbols(ptsp$x , ptsp$y, circles=radius, inches=0.35, fg="white", bg="red", add=TRUE)

#colours <- c("red", "blue")

# my.legend <- c("Cholera present","Cholera absent")
#legend(x= -10.6, y = 55.8, pch=16, legend = my.legend[1:2], cex=0.8, title= " Cholera presence or absence", col=colours[1:2])

#### Total Deaths #####

win.graph()

plot(IreMapBound, axes=T)

radius <- sqrt(ptsp$DEATHS / pi )
#points(ptsn$x , ptsn$y, cex=0.6, pch=16, col="blue")
symbols(ptsp$x , ptsp$y, circles=radius, inches=0.35, fg="white", bg="black", add=TRUE)


#### Cases per population #####

win.graph()

plot(IreMapBound, axes=T)

radius <- sqrt((ptsp$CASES / ptsp$POP)/ pi )
points(ptsn$x , ptsn$y, cex=0.6, pch=16, col="blue")
symbols(ptsp$x , ptsp$y, circles=radius, inches=0.35, fg="white", bg="red", add=TRUE)

#colours <- c("red", "blue")

# my.legend <- c("Cholera present","Cholera absent")
#legend(x= -10.6, y = 55.8, pch=16, legend = my.legend[1:2], cex=0.8, title= " Cholera presence or absence", col=colours[1:2])

#### Deaths per population #####

win.graph()

plot(IreMapBound, axes=T)

radius <- sqrt((ptsp$DEATHS / ptsp$POP) / pi )
#points(ptsn$x , ptsn$y, cex=0.6, pch=16, col="blue")
symbols(ptsp$x , ptsp$y, circles=radius, inches=0.35, fg="white", bg="black", add=TRUE)

#### Case fatality #####

win.graph()

plot(IreMapBound, axes=T)

radius <- sqrt((ptsp$DEATHS / ptsp$CASES) / pi )
#points(ptsn$x , ptsn$y, cex=0.6, pch=16, col="blue")
symbols(ptsp$x , ptsp$y, circles=radius, inches=0.20, fg="white", bg="black", add=TRUE)
