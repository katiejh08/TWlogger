##########################################
####        Apply Calibrations       #####
##########################################
library(tidyverse)
library(dplyr)
library(zoo)
library(tagtools)

setwd("~/Documents/Projects/R/TWlogger/00Data")

# Import rediscretized data
filename <- file.choose()

data <- read_csv(filename, 
                 col_types = cols(
                   dttz = col_datetime(),
                   ax = col_double(),
                   ay = col_double(),
                   az = col_double(),
                   mx = col_double(),
                   my = col_double(),
                   mz = col_double(),
                   secs_since = col_double()))
depid <- basename(filename)
depid <- strsplit(depid,'-')
depid <- depid[[1]][2]

# NOTE: This is to fix dttz. CombineCSV created dttz as GMT -3.
# However, tag was programmed accidentally to PST GMT -8. Need to change dttz back 
# to original tag start time then correct the time zone.
library(lubridate)
attr(data$dttz, "tzone") # check that dt is in GMT
data$dt <- force_tz(data$dttz,"Etc/GMT+11") # In future use this line on dttz with GMT+3
attr(data$dt, "tzone") <- "GMT"
attr(data$dt, "tzone")
data$dttz <- data$dt
attr(data$dttz, "tzone") <- "Etc/GMT+3"
attr(data$dttz, "tzone") # check that dttz is in Falklands time

# Also need to transform to North East Up sensor orientation used in Tag Tools 
# Acc	 [x -y z]
data$axT <- data$ax * (1.0)
data$ayT <- data$ay * (-1.0)
data$azT <- data$az * (1.0)
# Create a matrix with Acc data
At <- cbind(data$axT,data$ayT,data$azT)
# Check for NA
#count(is.na(At))
sum(is.na(At))
# If NAs, remove using linear approximation
if(sum(is.na(At)>0)){
  Atnarm <- At
  Atnarm <- na.approx(Atnarm, na.rm = FALSE)
} else {
  Atnarm <- At
}
# Check again for NAs
sum(is.na(Atnarm))
# Create an Acc sensor structure using At (NOTE: manually change fs value if not 50)
Atstruct <- sens_struct(data = Atnarm, fs = 50, depid = depid,type='acc')
# Plot 
# Alist <- list(A = Atstruct$data)
# plott(Alist,50)

data$mxT <- data$mx * 1.0
data$myT <- data$my * (-1.0)
data$mzT <- data$mz * 1.0
# Create a matrix with Mag data
Mt <- cbind(data$mxT,data$myT,data$mzT)
# Check for NA
sum(is.na(Mt))
# If NAs, remove using linear approximation
if(sum(is.na(Mt))>0){
  Mtnarm <- Mt
  Mtnarm <- na.approx(Mtnarm, na.rm = FALSE)
} else {
  Mtnarm <- Mt
}
# Check again for NAs
sum(is.na(Mtnarm))
# Create an Mag sensor structure using Mt (NOTE: manually change fs value if not 50)
Mtstruct <- sens_struct(data = Mtnarm, fs = 50, depid = depid,type='mag')
# Plot 
# Mlist <- list(M = Mtstruct$data)
# plott(Mlist,50)
# import calibration file for logger
cal <- read_csv(file.choose())
AccCal <- list(poly = cbind(cal$AccPoly1,cal$AccPoly2), cross = cbind(cal$AccCross1,cal$AccCross2,cal$AccCross3))
MagCal <- list(poly = cbind(cal$MagPoly1,cal$MagPoly2), cross = cbind(cal$MagCross1,cal$MagCross2,cal$MagCross3))


AtCal <- apply_cal(Atnarm,cal = AccCal, T = NULL)
list <- list(A = AtCal[10000:180000,])
plott(list,50)
MtCal <- apply_cal(Mtnarm,cal = MagCal, T = NULL)
list <- list(M = MtCal[10000:300000,])
plott(list,50)

    ## NOTE: This Section is optional, only use if spikes in Mag Data.
    # Look at plotted magnetometer for bad hits and find a threshold
    hiThresh <- 50
    loThresh <- -35
    MtCalThresh <- MtCal
    MtCalThresh[MtCalThresh > hiThresh] = NA
    MtCalThresh[MtCalThresh < loThresh] = NA
    sum(is.na(MtCalThresh))
    MtCalThresh <- na.approx(MtCalThresh, na.rm = FALSE)
    
    list <- list(M = MtCalThresh[10000:300000,])
    plott(list,50)
    sum(is.na(MtCalThresh))
    # If all looks Good, Save the Data back to MtCal
    MtCal <- MtCalThresh

#Check for field strength near the expected values (on Average)
#~9.81 for Acc, ? for Mag
AMcheck <- check_AM(A=AtCal,M= MtCal,fs=50)
AMlist <- list(A = AMcheck$fstr)
plott(AMlist,50)

#Save Calibrated Data Back To Dataframe
data2 <-cbind(data[,1],data[,20],data[2:3],AtCal,MtCal,data[11:13])
#Rename Columns
colnames(data2) <- c("dttz","dt","name","ts","Ax","Ay","Az",
                     "Mx","My","Mz","secs_since","true_since","tsDif")
names(data2)
#Save Output
write_csv(data2, file.path(dirname(filename), paste(depid,"-Final.csv",sep="")))

## Optional Section to view Pitch, Roll and Heading
#Pitch Roll and Heading
pr <- a2pr(AtCal,50)
plott(pr,fs=50)
h <- m2h(M= MtCal, A= AtCal, sampling_rate = 50)
plott(h,fs=50)




