##########################################
####        Combine csv files        #####
####           Version 1.21          #####
##########################################

## For Original Year 1 data (TWLogger Version 1.21)
# This allows the user to choose the folder containing the multiple CSV files of the Drone
setwd("C:/Users/Musculus/Documents/R/TWlogger/00Data")

tzOffset <- "Etc/GMT+3"
#require(rChoiceDialogs)
# Specify the start and end time of deployment (NOTE: will be specific to each deployment, use local deployment time)
# startTime <- as.POSIXct(strptime("2018-07-08 17:47:00",format="%Y-%m-%d %H:%M:%S"),tz=tzOffset)
# Always use logger retrieval time (regardless if retrieved before battery died)
# endTime <- as.POSIXct(strptime("2018-07-10 09:42:00",format="%Y-%m-%d %H:%M:%S"),tz=tzOffset)


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

# select only columns with datetime, lat, long and alt
str(accdata)
accdata <- accdata[,c("name", "Date.MM.DD.YYYY.","Time.hh.mm.ss.","Timestamp.Ms.","Temp.Raw.",
                      "ACCELX","ACCELY","ACCELZ","MAGX","MAGY","MAGZ")]
# accdata <- accdata[,c("name", "Date.MM.DD.YYYY.","Time.hh.mm.ss.","Timestamp.Ms.","Temp.Raw.",
#                       "ACCELX","ACCELY","ACCELZ","MAGX","MAGY","MAGZ","Sats","HDOP","Latitude","Longitude","FixAge","DateUTC","TimeUTC","DateAge","Altitude","Course","Speed")]
# change column names to more practical shorter names
colnames(accdata)[1:11] <- c("name","date","time","ts","temp","ax","ay","az","mx","my","mz")
rownames(accdata) <- c()

# create proper datetime objects
# accdata$dt <- as.POSIXct(paste(accdata$date, accdata$time), format="%m/%d/%Y %H:%M:%S", tz='GMT')
# accdata$dttz <- format(accdata$dt, tz=tzOffset, usetz=TRUE)
# accdata$dttz <- as.POSIXct(strptime(accdata$dttz,format="%Y-%m-%d %H:%M:%S"),tz=tzOffset)
# Don't do this for July 2018

# create proper datetime objects (convert GMT to local)
accdata$dt <- as.POSIXct(paste(accdata$date, accdata$time), format="%m/%d/%Y %H:%M:%S", tz='GMT')
attr(accdata$dt, "tzone") # check that dt is in GMT
accdata$dttz <- accdata$dt # set dttz to dt
attr(accdata$dttz, "tzone") <- tzOffset # change the timezone to tzOffset
attr(accdata$dttz, "tzone") # check that dttz is in local time
str(accdata)

#simple plot
plot(accdata$dttz)

# remove unused columns
#accdata <- accdata[,c("name","dt","dttz","ts","temp","ax","ay","az","mx","my","mz","Sats","HDOP","Lat","Long","FixAge","DateUTC","TimeUTC","DateAge","Altitude","Course","Speed")]
accdata <- subset(accdata, select = -c(date,time) )

# Run subset() function to extract data for the selected timerange
accdata <- subset(accdata, accdata$dttz >= startTime & accdata$dttz <= endTime)
str(accdata)

# Export the combined data for later use
write.csv(accdata, file=paste(deploymentName,"-COMBINED", ".csv",sep=""), row.names = FALSE)
