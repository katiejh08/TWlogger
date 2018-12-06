##########################################
####        Combine csv files        #####
####           Version 2.X           #####
##########################################

## For Version 2 Data
# This allows the user to choose the folder containing the multiple CSV files
setwd("~/Documents/Projects/R/TWlogger/00Data")
#tzOffset <- "Etc/GMT+7"
tzOffset <- "Etc/GMT"

# Select the first CSV of the group you want to combine
filename <- file.choose("Select the first CSV of the group")
pathChoice <- dirname(filename)

#require(rChoiceDialogs)
#pathChoice = choose.dir()
# This imports all CSV files in the directory chosen
filenames <- list.files(path = pathChoice, pattern = "*.csv", all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = TRUE)
library(plyr)
import.list <- llply(paste(pathChoice,"/",filenames,sep = ""), read.csv)
accdata <- do.call("rbind", sapply(paste(pathChoice,"/",filenames,sep = ""), read.csv, simplify = FALSE))
#rm(import.list)
#this creates a name column containing the date and tag number
accdata$name <- row.names(accdata)
deploymentName <- strsplit(accdata$name[1],'/')
deploymentName <- deploymentName[[1]][2]
deploymentName <- strsplit(deploymentName,'-')
deploymentName <- deploymentName[[1]][1]
accdata$name <- deploymentName
#rm(pathChoice)
#rm(filenames)

# select only columns with datetime, lat, long and alt
str(accdata)
accdata <- accdata[,c("name", "Date.MM.DD.YYYY.","Time.hh.mm.ss.","Timestamp.Ms.","Temp.Raw.",
                      "ACCELX","ACCELY","ACCELZ","MAGX","MAGY","MAGZ")]
# change column names to more practical shorter names
colnames(accdata)[1:11] <- c("name", "date","time","ts","temp","ax","ay","az","mx","my","mz")
rownames(accdata) <- c()

# create proper datetime objects
accdata$dt <- as.POSIXct(paste(accdata$date, accdata$time), format="%m/%d/%Y %H:%M:%S", tz='GMT')
accdata$dttz <- format(accdata$dt, tz=tzOffset, usetz=TRUE)
accdata$dttz <- as.POSIXct(strptime(accdata$dttz,format="%Y-%m-%d %H:%M:%S"),tz=tzOffset)

#simple plot
library(ggplot2)
ggplot() +
  geom_line(data = accdata, aes(x = dttz, y = ax,color = 'AX')) +
  geom_line(data = accdata, aes(x = dttz, y = ay,color = 'AY')) +
  geom_line(data = accdata, aes(x = dttz, y = az,color = 'AZ')) +
  scale_colour_manual(name="Axis",
                      values=c(AX="red", AY="blue", AZ="green")) +
  ylab("Raw ACC") + 
  xlab("Time") 

# Export the combined data for later use
## ToDo - Make the filename populate from the data so you dont have to change it every time
write.csv(accdata, file=paste("Tag7_LSM9Ds0_CalibrationData", ".csv",sep=""))


