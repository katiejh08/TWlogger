
# create Spatial Points object of Sakhalvasho coordinates
crds <- cbind(long,lat)
data$crds <- SpatialPoints(crds,proj4string=CRS("+proj=longlat +datum=WGS84"))

# create Date object to pass to sunriset function
# dates <- gsub('-', '/', as.character(data$date))
# data$dates <- as.POSIXct(dates,tz=tzOffset)  

#first time need to install package
#install.packages("maptools")
library(maptools)
# calculate sunrise times
# data$srise <- sunriset(data$crds, data$dates, direction=c("sunrise"),POSIXct.out=TRUE)[,2]
data$srise <- sunriset(data$crds, data$dttz_down, direction=c("sunrise"),POSIXct.out=TRUE)[,2]

# Calculate Dawn,Sunrise, Sunset and Dusk times
data$sset <- sunriset(data$crds, data$dttz_down, direction=c("sunset"),POSIXct.out=TRUE)[,2]
data$dawn <- crepuscule(data$crds, data$dttz_down, solarDep=18, direction=c("dawn"),POSIXct.out=TRUE)[,2]
data$dusk <- crepuscule(data$crds, data$dttz_down, solarDep=18, direction=c("dusk"),POSIXct.out=TRUE)[,2]

# Calculate solar position (altitude)
data$solarpos <- solarpos(data$crds, data$dttz_down)[,2]

# determine if point is during night or day
data$night <- ifelse(data$dttz_down < data$srise | data$dttz > data$sset,'night','day')


# determine if day night dusk or dawn
data$astronomical <- ifelse(data$dttz > data$srise & data$dttz < data$sset,'day', 
                     ifelse(data$dttz < data$dawn & data$dttz < data$dusk ,'night',
                            ifelse(data$dttz < data$srise,'dawn','dusk')))

# drops <- c("dates","crds","season")
drops <-c("crds")
data <- data[ , !(names(data) %in% drops)]

