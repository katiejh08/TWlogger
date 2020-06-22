source('pt2pt_fxns.R')   
## Check out this document in notepad, it contains fxns to calculate speeds, directions, ... between two points on the globe
## I calculate statistics from each point to the next

## We must order the dataframe in order to ensure the correct application of our coming functions
data$name <- as.character(data$name)
data <- data[order(data$name,data$dttz),]
library(fossil)

calcdist <- function(x) pt2pt.distance(longitude=x$long,latitude=x$lat)
calcdur <- function(x) pt2pt.duration(datetime=x$dttz)
calcdir <- function(x) pt2pt.direction(longitude=x$long,latitude=x$lat)

## Specify functions to calculate FORWARD and BACKWARD dist, dur and spd over lists of id's. 
## NOTE: this will allow you to calculate trajectorie speeds and directions for a large number of birds at once
v1 <- lapply(split(data,data$name),"calcdist")
v2 <- lapply(split(data,data$name),"calcdur")
v3 <- lapply(split(data,data$name),"calcdir")

## this converts the distance from M to KM
data$dist <- as.numeric(unlist(v1))/1000
## this converts the distance from KM to MI
# data$dist <- data$dist * 0.621371
##converts duration from S to Hrs 
data$dur <- as.numeric(unlist(v2))/3600
##Speed is in Miles/Hr
data$spd <- data$dist/data$dur
data$dir <- as.numeric(unlist(v3))
