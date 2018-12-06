##########################################
####        Combine csv files        #####
####           Version 3.X           #####
##########################################

## For GPS/LSM303 data (for all TWLogger Version 3 units)
# This allows the user to choose the folder containing the multiple CSV files
#setwd("C:/Users/Musculus/Documents/R/TWlogger/00Data")
setwd("/Users/jamesfahlbusch/Documents/Projects/R/TWlogger/00Data")
tzOffset <- "Etc/GMT+3" # Falklands time in the winter

# Specify the start and end time of deployment (NOTE: will be specific to each deployment, use local deployment time)
startTime <- as.POSIXct(strptime("2018-07-08 17:47:00",format="%Y-%m-%d %H:%M:%S"),tz=tzOffset)
# Always use logger retrieval time (regardless if retrieved before battery died)
endTime <- as.POSIXct(strptime("2018-07-10 09:42:00",format="%Y-%m-%d %H:%M:%S"),tz=tzOffset)


# Select the first CSV of the group you want to combine
filename <- file.choose("Select the first CSV of the group")
pathChoice <- dirname(filename)

# This imports all CSV files in the directory chosen
filenames <- list.files(path = pathChoice, pattern = "*.csv", all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = TRUE)
library(plyr)
import.list <- llply(paste(pathChoice,"/",filenames,sep = ""), read.csv)
accdata <- do.call("rbind", sapply(paste(pathChoice,"/",filenames,sep = ""), read.csv, simplify = FALSE))
#rm(import.list)
#this creates a name column containing the date and tag number
accdata$name <- row.names(accdata)
deploymentName <- strsplit(accdata$name[1],'/')
deploymentName <- deploymentName[[1]][(length(deploymentName[[1]]))-1] #pulls name of deployment from row names (n-1 element of split string)
accdata$name <- deploymentName
#check to see if worked
str(accdata)
#rm(pathChoice)
#rm(filenames)

# reorders columns to prepare to change column names
accdata <- accdata[,c("name", "Date.MM.DD.YYYY.","Time.hh.mm.ss.","Timestamp.Ms.","Temp.Raw.",
                      "ACCELX","ACCELY","ACCELZ","MAGX","MAGY","MAGZ","Sats","HDOP","Latitude","Longitude","FixAge","DateUTC","TimeUTC","DateAge","Altitude","Course","Speed")]
# change column names to more practical shorter names
colnames(accdata)[1:22] <- c("name","date","time","ts","temp","ax","ay","az","mx","my","mz","Sats","HDOP","Lat","Long","FixAge","DateUTC","TimeUTC","DateAge","Altitude","Course","Speed")
rownames(accdata) <- c()

# Don't do this for July 2018
# create proper datetime objects (convert GMT to local)
accdata$dt <- as.POSIXct(paste(accdata$date, accdata$time), format="%m/%d/%Y %H:%M:%S", tz='GMT')
attr(accdata$dt, "tzone") # check that dt is in GMT
accdata$dttz <- accdata$dt # set dttz to dt
attr(accdata$dttz, "tzone") <- tzOffset # change the timezone to tzOffset
attr(accdata$dttz, "tzone") # check that dttz is in local time
str(accdata)

##########################################################
## NOTE this section is for tag data set to CA time!!! 
##########################################################

# create proper datetime objects
CA_tzOffset <- "Etc/GMT+7"
accdata$dt <- as.POSIXct(paste(accdata$date, accdata$time), format="%m/%d/%Y %H:%M:%S", tz=CA_tzOffset) # initial data is in CA time
attr(accdata$dt, "tzone") # check timezone
attr(accdata$dt, "tzone") <- 'GMT' # change the timezone to GMT
accdata$dttz <- accdata$dt # set dttz to dt
attr(accdata$dttz, "tzone") <- tzOffset # change the timezone to tzOffset
attr(accdata$dttz, "tzone") # check that dttz is in local time
str(accdata)
startTime
endTime

# remove unused columns
#accdata <- accdata[,c("name","dt","dttz","ts","temp","ax","ay","az","mx","my","mz","Sats","HDOP","Lat","Long","FixAge","DateUTC","TimeUTC","DateAge","Altitude","Course","Speed")]
accdata <- subset(accdata, select = -c(date,time) )

# Run subset() function to extract data for the selected timerange
accdata <- subset(accdata, accdata$dttz >= startTime & accdata$dttz <= endTime)
str(accdata)

# Export the combined data for later use
write.csv(accdata, file=paste(deploymentName,"-COMBINED", ".csv",sep=""), row.names = FALSE)

#simple plot (CAUTION: takes a very long time to load, depending on processing power)
library(ggplot2)
ggplot() +
  geom_line(data = accdata, aes(x = dttz, y = ax,color = 'AX')) +
  geom_line(data = accdata, aes(x = dttz, y = ay,color = 'AY')) +
  geom_line(data = accdata, aes(x = dttz, y = az,color = 'AZ')) +
  scale_colour_manual(name="Axis",
                      values=c(AX="red", AY="blue", AZ="green")) +
  ylab("Raw ACC") + 
  xlab("Time") 

# Extract GPS
gpsData <- accdata[,c("Sats","HDOP","Lat","Long","FixAge","DateUTC","TimeUTC","DateAge","Altitude","Course","Speed")]
#remove blank lines
gpsData <- gpsData[!is.na(gpsData["Sats"]),]
gpsData$Lat <- gpsData$Lat/1000000
gpsData$Long <- gpsData$Long/1000000
#remove bad hits
gpsData <- gpsData[gpsData["Sats"]>2,] # Keep anything with 3 or more Sats
gpsData <- gpsData[gpsData["Lat"]<=90,]
gpsData <- gpsData[gpsData["Long"]<=180,]
## for southern hemisphere deployments
gpsData <- gpsData[gpsData["Lat"]<0,]
gpsData <- gpsData[gpsData["Long"]<0,]
# create proper datetime objects (convert GMT to local)
gpsData$dt <- as.POSIXct(paste(gpsData$DateUTC, gpsData$TimeUTC), format="%m/%d/%Y %H:%M:%S", tz='GMT')
attr(gpsData$dt, "tzone") # check that dt is in GMT
gpsData$dttz <- gpsData$dt # set dttz to dt
attr(gpsData$dttz, "tzone") <- tzOffset # change the timezone to tzOffset
attr(gpsData$dttz, "tzone") # check that dttz is in local time
str(gpsData)

# check to make sure data is reasonably located
plot(gpsData$Long,gpsData$Lat)
rownames(gpsData) <- c()

write.csv(gpsData, file=paste(deploymentName,"-GPSData", ".csv",sep=""), row.names = FALSE)

## Map
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

mapZoom <- 13 # KJH!! Adjust this to fit all the data (20 is all the way zoomed in)
#plot the extent of this map (NOTE: must adjust zoom parameter)
myMap <-  get_googlemap(center = c(lon = mean(c(min(gpsData$Long), max(gpsData$Long))),  lat = mean(c(min(gpsData$Lat),max(gpsData$Lat)))),
                        zoom = mapZoom, 
                        size = c(640,400),
                        scale = 2,
                        extent="device",
                        maptype="hybrid", #"terrain"
                        style = c(feature = "all", element = "labels", visibility = "off"))

#plot map
ggmap(myMap)+
  geom_point(data=gpsData,aes(x=as.numeric(as.character(Long)),y=as.numeric(as.character(Lat))),alpha = 0.5, color= 'yellow') +
  scale_alpha(guide = 'none') + 
  theme(legend.position="right", 
        plot.title = element_text(hjust = 0.5)) + 
  ggtitle(deploymentName) + coord_fixed(1.3) + 
  labs(
    x = "Longitude",
    y = "Latitude"
  )
#save map output as .png
ggsave(sprintf("%s_Map.png",deploymentName), plot = last_plot(), device = "png",
       scale = 2, width = 7, height = 5, units = "in", dpi = 300, limitsize = F)

# at this point will have achieved 3 objectives: 
# 1) combined all raw data into single csv
# 2) saved separate file containing only GPS data, and 
# 3) created a map to visualize movements
