library(sp)
library(rgdal)

xy <- data.frame(ID = 1:2, X = c(118, 119), Y = c(10, 50))
coordinates(xy) <- c("X", "Y")
proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example

res <- spTransform(xy, CRS("+proj=utm +zone=51 ellps=WGS84"))
res
#            coordinates ID
# 1 (-48636.65, 1109577)  1
# 2    (213372, 5546301)  2

## For a SpatialPoints object rather than a SpatialPointsDataFrame, just do: 
as(res, "SpatialPoints")
# SpatialPoints:
#              x       y
# [1,] -48636.65 1109577
# [2,] 213372.05 5546301
# Coordinate Reference System (CRS) arguments: +proj=utm +zone=51
# +ellps=WGS84 