##########################################
####   Process Calibration Data      #####
##########################################

library(tidyverse)
library(dplyr)
library(zoo)
library(tagtools)

setwd("~/Documents/Projects/R/TWlogger")

# Import rediscretized data
filename <- file.choose()
# data <- read_csv(filename)
data <- read_csv(filename, 
                 col_types = cols(
                   dttz = col_datetime(),
                   ax = col_double(),
                   ay = col_double(),
                   az = col_double(),
                   mx = col_double(),
                   my = col_double(),
                   mz = col_double(),
                   secs_since = col_double(),
                   temp = col_double()))
depid <- basename(filename)
depid <- strsplit(depid,'-')
depid <- depid[[1]][2]

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
Alist <- list(A = Atstruct$data)
plott(Alist,50)

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
Mlist <- list(M = Mtstruct$data)
plott(Mlist,50)

# Atnarm[nrow(Atnarm),]<- Atnarm[(nrow(Atnarm)-1),]
# Mtnarm[nrow(Mtnarm),]<- Mtnarm[(nrow(Mtnarm)-1),]

Acrop <- crop(Atstruct$data, Atstruct$sampling_rate)
# Plot 
list <- list(A = Acrop$Y)
plott(list,50)
Acrop <- crop(Acrop$Y, Atstruct$sampling_rate)
# Run Spherical Cal
AccCal <- spherical_cal(Acrop$Y, n = 9.81, method = NULL)

## Magnetometer Calibration
Mcrop <- crop(Mtstruct$data, sampling_rate = 50)
# Plot 
list <- list(M = Mcrop$Y)
plott(list,50)
Mcrop <- crop(Mcrop$Y, Mtstruct$sampling_rate)

# Input the magnetic Field Strength for the location
# https://www.ngdc.noaa.gov/geomag-web/#igrfwmm
# NOTE: website provides data in nano teslas, need to divide by 1000 
# 28281.1 / 1000 = 28.2811
# 47974.5 / 1000 = 47.9745
fieldStrength <- 49.58
MagCal <- spherical_cal(Mcrop$Y, n = fieldStrength, method = NULL)


#Compute field intensity of acceleration and magnetometer data, 
#and the inclination angle of the magnetic field. This is useful for 
#checking the quality of a calibration, for detecting drift, and for 
#validating the mapping of the sensor axes to the tag axes.
AMcheck <- check_AM(A=AccCal$Y,M= MagCal$Y,fs=50)
list <- list(A = AMcheck$fstr)
plott(list,50)


Ac <- apply_cal(Atnarm,cal = AccCal$G, T = NULL)
list <- list(A = Ac)
plott(list,50)

# Combine calibration estimations into one dataframe
cal <-data.frame(cbind(AccCal$G$poly,AccCal$G$cross,MagCal$G$poly,MagCal$G$cross))
colnames(cal) <-c("AccPoly1","AccPoly2","AccCross1","AccCross2","AccCross3",
                  "MagPoly1","MagPoly2","MagCross1","MagCross2","MagCross3")
cal
write.csv(cal, file=paste0(depid,".csv"),row.names=FALSE)

