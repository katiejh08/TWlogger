# Combine GPS & acc data
# Select the first CSV of the group you want to combine
filename <- file.choose("Select the first CSV of the group")
pathChoice <- dirname(filename)

# This imports all CSV files in the directory chosen
filenames <- list.files(path = pathChoice, pattern = "*.csv", all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = TRUE)
library(plyr)
import.list <- llply(paste(pathChoice,"/",filenames,sep = ""), read_csv)
# dataGPS <-bind_rows(import.list)
GPSdata <- do.call("rbind", sapply(paste(pathChoice,"/",filenames,sep = ""), read.csv, simplify = FALSE))
#rm(import.list)
#this creates a name column containing the date and tag number
GPSdata$name <- row.names(GPSdata)
deploymentName <- strsplit(GPSdata$name,'/')
deploymentName <- deploymentName[[1]][(length(deploymentName[[1]]))-1] #pulls name of deployment from row names (n-1 element of split string)
GPSdata$name <- deploymentName
#check to see if worked
str(GPSdata)
#rm(pathChoice)
#rm(filenames)

# reorders columns to prepare to change column names
GPSdata <- GPSdata[,c("name","Sats","HDOP","Lat","Long","FixAge","DateUTC","TimeUTC","DateAge","Altitude","Course","Speed","dt","dttz")]
