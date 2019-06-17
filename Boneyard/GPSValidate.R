library(tidyverse)
library(dplyr)
library(lubridate)

# Combine GPS & acc data
# Select the first CSV of the group you want to combine
filename <- file.choose("Select the first CSV of the group")
pathChoice <- dirname(filename)

# This imports all CSV files in the directory chosen
filenames <- list.files(path = pathChoice, pattern = "*.csv", all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = TRUE)
#library(plyr)
import.list <- plyr::llply(paste(pathChoice,"/",filenames,sep = ""), read_csv)
# dataGPS <-bind_rows(import.list)
GPSdata <- do.call("rbind", sapply(paste(pathChoice,"/",filenames,sep = ""), read.csv, simplify = FALSE))
rm(import.list)
#this creates a name column containing the date and tag number
depIDstring <- do.call(rbind, strsplit(row.names(GPSdata), '_'))
depIDstring <- depIDstring[,3]
depIDstring <- do.call(rbind, strsplit(depIDstring, '-'))
GPSdata$ID <- as.factor(depIDstring[,1])
rm(depIDstring)
rownames(GPSdata) <- c()
#check to see if worked
str(GPSdata)
#rm(pathChoice)
#rm(filenames)
data <- GPSdata # reset
# GPSdata <- data
# create proper datetime objects (convert GMT to local)
data$dt <- as.POSIXct(paste(data$DateUTC, data$TimeUTC), format="%m/%d/%Y %H:%M:%S", tz='GMT')
attr(data$dt, "tzone") # check that dt is in GMT
data$dttz <- data$dt # set dttz to dt
attr(data$dttz, "tzone") <- tzOffset # change the timezone to tzOffset
attr(data$dttz, "tzone") # check that dttz is in local time
sapply(data, function(x) sum(is.na(x)))
data[is.na(data$dt),]
# reorders columns to prepare to change column names
data <- data[,c("ID","name","Sats","HDOP","Lat","Long","FixAge","DateUTC","TimeUTC","DateAge","dt","dttz")]
colnames(data)[5:6] <- c("lat","long")

##########################################
####    Extract yday, mth, ...       #####
##########################################
#create a date-object
data$date <- as.Date(data$dttz, tz = tzOffset)

# define seasonal cycles per bird
#first time need to install package
#install.packages("lubridate")

data$yr <- year(data$dttz)
data$mth <- month(data$dttz)
data$name <- data$ID

# define some other potential splits for tracks
data$name_yr_mth <- paste(data$name,data$yr,data$mth,sep='_')
data$name_yr <- paste(data$name,data$yr,sep='_')

# determine yday and unique days per bird
data$yday <- yday(data$dttz)
data$yday <- sprintf('%03d',data$yday)
data$yday <- paste('D',data$yday,sep='')

# define a date per indiv
data$indday <- as.factor(paste(data$name, data$yr, data$yday, sep ='_', collapse = NULL))

########################################################
#### Calculate point to point movement statistics    ###
########################################################
source('Point2Point-stats.R')
data$hr <- hour(data$dttz)	

source('dailystats.R')
data <- merge(data,daylocs[,c("indday","daily.dist","daily.dir","daily.c.dist","straightness")],all.x=T)
data <- merge(data,unique(data2[,c("indday","daily.start","daily.end")]),all.x=T)

data$dayrange <- as.numeric(difftime(data$daily.end,data$daily.start))/3600

########################################################
#### Calculate sunrise and sunset times              ###
########################################################

source('SunriseSunsetTimes.R')
