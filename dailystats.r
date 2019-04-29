#first time need to install package
#install.packages("fossil")
library(fossil)

### determine first and last obs for each travel day
################################################################
starts <- aggregate(data$dttz, by=list(data$indday), FUN= function(x)min(as.character(x)))
colnames(starts)[1:2]  <- c("indday","daily.start")
starts$daily.start <- as.POSIXct(strptime(starts$daily.start,format="%Y-%m-%d %H:%M:%S"),tz=tzOffset)

ends <- aggregate(data$dttz, by=list(data$indday), FUN= function(x)max(as.character(x)))
colnames(ends)[1:2]  <- c("indday","daily.end")
ends$daily.end <- as.POSIXct(strptime(ends$daily.end,format="%Y-%m-%d %H:%M:%S"),tz=tzOffset)

# merge daily starts and ends with original dataframe
new <- merge(starts,ends)
data2 <- merge(data,new)

# extract locations at start and end of each day
startofdays <- which(data2$dttz == data2$daily.start)
endofdays <- which(data2$dttz == data2$daily.end)

starts <- data2[startofdays,c("indday","long","lat","dttz")]
ends <- data2[endofdays,c("indday","long","lat","dttz")]
colnames(starts)[1:4] <-c("indday","stlon","stlat","stdttz")
colnames(ends)[1:4] <-c("indday","endlon","endlat","enddttz")
daylocs <- merge(starts,ends,by="indday")

### CALCULATE DAILY DISTANCE AND DIRECTON 
##################################################
library(fossil)
# calculate daily distance
daylocs$daily.dist <- deg.dist(daylocs$stlon,daylocs$stlat,daylocs$endlon,daylocs$endlat)
## this converts the distance from KM to MI
# daylocs$daily.dist <- daylocs$daily.dist * 0.621371
daylocs$daily.dir <- earth.bear(daylocs$stlon,daylocs$stlat,daylocs$endlon,daylocs$endlat)
daylocs$daily.dir  <- ifelse(daylocs$daily.dir > 180, daylocs$daily.dir-360, daylocs$daily.dir)

# append some basic info from original dttz
##daylocs <- merge(daylocs,unique(data[,c("indday","name","migr","yr","mth")]),all.x=T)
daylocs <- merge(daylocs,unique(data[,c("indday","name","yr","mth")]),all.x=T)

# calculate daily cumulative distance
daily.c.dist <- aggregate(data$dist,by=list(data$indday),FUN='sum')
colnames(daily.c.dist)[1:2] <- c("indday","daily.c.dist")

daylocs <- merge(daylocs,daily.c.dist,all.x=T)
daylocs$straightness <- daylocs$daily.dist/daylocs$daily.c.dist

# To quantify consistency of flight direction I calculate turning angle as 
# the difference between current flight direction and flight direction at the previous point

pt2pt.ta <- function(direction){
	ta <- c(NA,direction[2:length(direction)] - direction[1:length(direction)-1])
	ta <- ifelse(ta < -180, ta + 360, ta)
	ta <- ifelse(ta > 180 , ta - 360, ta)
return(ta)
}

calc.ta <- function(x) pt2pt.ta(direction=x$daily.dir)

## Specify functions to calculate FORWARD and BACKWARD dist, dur and spd over lists of id's. 
## NOTE: this will allow you to calculate trajectorie speeds and directions for a large number of birds at once
daylocs <- daylocs[order(daylocs$name,daylocs$indday),]

v1 <- lapply(split(daylocs,daylocs$name),"calc.ta")

daylocs$ta <- as.numeric(unlist(v1))


