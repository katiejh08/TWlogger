###        Combine csv files        ###
###           Version 3.X           ###

## For GPS/LSM303 data (for all TWLogger Version 3 units)
## This assumes that the tag time was synced with GMT time
## Set the timezone offset below to create a local time variable 


# Objectives: 
# 1) combine all raw data into single csv
# 2) save separate file containing only GPS data, and 
# 3) create a map to visualize movements

#### Load Packages ####
pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}
# Load Packages
pkgTest("tidyverse")
pkgTest("ggplot2")
pkgTest("lubridate")
pkgTest("leaflet")
pkgTest("htmlwidgets")
pkgTest("argosfilter")
# Set the Working Directory to the path of source file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(current_path)


#### Global Variables ####

# Choose a timezone offset to convert from GMT to local time
# tzOffset <- "Etc/GMT+3" # Falklands time in the winter
tzOffset <- "Etc/GMT+7" # CA

# Specify the start and end time of deployment (NOTE: will be specific to each deployment, use local deployment time)
startTime <- as.POSIXct(strptime("2016-07-08 17:47:00",format="%Y-%m-%d %H:%M:%S"),tz=tzOffset)
# Always use logger retrieval time (regardless if retrieved before battery died)
endTime <- as.POSIXct(strptime("2020-07-10 09:42:00",format="%Y-%m-%d %H:%M:%S"),tz=tzOffset)

# Select the first CSV of the group you want to combine
filename <- file.choose("Select the first CSV of the group")
pathChoice <- dirname(filename)

#### Import Data ####

# This imports all CSV files in the directory chosen
filenames <- list.files(path = pathChoice, pattern = "*.csv", all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = TRUE)
import.list <- plyr::llply(paste(pathChoice,"/",filenames,sep = ""), read.csv)
accdata <- do.call("rbind", sapply(paste(pathChoice,"/",filenames,sep = ""), read.csv, simplify = FALSE))
#rm(import.list)
# Create a name column containing the date and tag number
accdata$name <- row.names(accdata)
deploymentName <- strsplit(accdata$name[1],'/')
deploymentName <- deploymentName[[1]][(length(deploymentName[[1]]))-1] #pulls name of deployment from row names (n-1 element of split string)
accdata$name <- deploymentName
#check to see if worked
str(accdata)
#rm(pathChoice)
#rm(filenames)

#### Process Data #### 

# Reorder columns to prepare to change column names
accdata <- accdata[,c("name", "Date.MM.DD.YYYY.","Time.hh.mm.ss.","Timestamp.Ms.","Temp.Raw.",
                      "ACCELX","ACCELY","ACCELZ","MAGX","MAGY","MAGZ","Sats","HDOP","Latitude","Longitude","FixAge","DateUTC","TimeUTC","DateAge","Altitude","Course","Speed")]
# Change column names to more practical shorter names
colnames(accdata)[1:22] <- c("name","date","time","ts","temp","ax","ay","az","mx","my","mz","Sats","HDOP","Lat","Long","FixAge","DateUTC","TimeUTC","DateAge","Altitude","Course","Speed")
rownames(accdata) <- c()

# Create proper datetime objects (convert GMT to local)
accdata$dt <- as.POSIXct(paste(accdata$date, accdata$time), format="%m/%d/%Y %H:%M:%S", tz='GMT')
attr(accdata$dt, "tzone") # check that dt is in GMT
accdata$dttz <- accdata$dt # create dttz from dt
attr(accdata$dttz, "tzone") <- tzOffset # change the timezone to tzOffset
attr(accdata$dttz, "tzone") # check that dttz is in local time
str(accdata)

# Remove unused columns
accdata <- subset(accdata, select = -c(date,time) )

# Run subset() function to extract data for the selected timerange
accdata <- subset(accdata, accdata$dttz >= startTime & accdata$dttz <= endTime)
str(accdata)

plotLength <- ifelse(length(accdata$ax)< 10000,length(accdata$ax),10000)
# Simple plot (CAUTION: takes a very long time to load, depending on processing power)
ggplot(data = accdata[1:plotLength,]) +
  geom_line(aes(x = dttz, y = ax,color = 'AX')) +
  geom_line(aes(x = dttz, y = ay,color = 'AY')) +
  geom_line(aes(x = dttz, y = az,color = 'AZ')) +
  scale_colour_manual(name="Axis",
                      values=c(AX="red", AY="blue", AZ="green")) +
  ylab("Raw ACC") + 
  xlab("Time") + 
  ggtitle("First 10,000 lines of Accelerometer data")

#### Export Data ####

# Export the combined data for later use
write.csv(accdata, file=paste(deploymentName,"-COMBINED", ".csv",sep=""), row.names = FALSE)


#### Extract GPS ####
gpsData <- accdata[,c("dt","dttz", "Sats","HDOP","Lat","Long","FixAge","DateUTC","TimeUTC","DateAge","Altitude","Course","Speed")]
#remove blank lines
gpsData <- gpsData[!is.na(gpsData["Sats"]),]
gpsData$Lat <- gpsData$Lat/1000000
gpsData$Long <- gpsData$Long/1000000
#remove bad hits
gpsData <- gpsData %>% 
  filter(between(Lat,-90,90),
         Sats > 2,
         between(Long, -180,180),
         sdafilter(Lat,Long,dt,rep(3,length(gpsData$Lat)), vmax = 30, ang = c(15, 25), distlim = c(2500, 5000))!= "removed")

# create proper datetime objects (convert GMT to local)
gpsData$dtGPS <- as.POSIXct(paste(gpsData$DateUTC, gpsData$TimeUTC), format="%m/%d/%Y %H:%M:%S", tz='GMT')
attr(gpsData$dtGPS, "tzone") # check that dt is in GMT
gpsData$dttzGPS <- gpsData$dtGPS # set dttz to dt
attr(gpsData$dttzGPS, "tzone") <- tzOffset # change the timezone to tzOffset
attr(gpsData$dttzGPS, "tzone") # check that dttz is in local time
str(gpsData)

# check to make sure data is reasonably located
plot(gpsData$Long,gpsData$Lat)
rownames(gpsData) <- c()

write.csv(gpsData, file=paste(deploymentName,"-GPSData", ".csv",sep=""), row.names = FALSE)

# initiate the leaflet instance and store it to a variable
m = leaflet() %>%
  # Base groups
  addProviderTiles(providers$Stamen.Toner, group = "Toner (default)") %>%
  addTiles(group = "OSM") %>%
  addProviderTiles(providers$Esri.OceanBasemap, group = "Oceans") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>% 
  addCircleMarkers( 
    lng = gpsData$Long,  
    lat = gpsData$Lat,
    popup = gpsData$dttz, 
    radius = 5, 
    stroke = FALSE, 
    fillOpacity = 0.75,
    clusterOptions = markerClusterOptions(),
    group = deploymentName
  )  %>%
  addPolylines(
    lng = gpsData$Long, 
    lat = gpsData$Lat,
    group = "lines"
  ) %>% 
  # Layers control
  addLayersControl(
    baseGroups = c("Toner (default)", "OSM", "Oceans", "Toner Lite"),
    overlayGroups = c(deploymentName,"lines"),
    options = layersControlOptions(collapsed = F),
    position = "topright"
  )
# we can "run"/compile the map, by running the printing it
m

saveWidget(m, file=paste0(deploymentName,".html"))



