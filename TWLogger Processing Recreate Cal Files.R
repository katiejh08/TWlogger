
# Setup environment -------------------------------------------------------

# This function loads required packages and installs them if they are not found
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
pkgTest("dplyr")
pkgTest("zoo")
pkgTest("tagtools")

# Select parent folder that contains all deployment folders
dataPath <- choose.dir(caption="Select the folder containing deployment raw data folders")
# Select folder that contains calibration meta data
metaPath <- choose.dir(caption="Select the folder containing calibration meta data") 
# Load calMeta
load("calMeta.RData")

# Set the working directory to the location of this file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(current_path)
getwd()

# This script includes two options:
# 1) Use calMeta to automatically crop within a for loop OR
# 2) Manually crop Atstruct and Mtstruct

# Option 1: Recreate cal files with tcues using calMeta -------------------

# Iteratively process deployments
for(i in 1:length(calMeta$depid)) {
  # Save working directory so you can return to it at end of for loop
  current_path <- getwd()
  # Resets working directory to deployment folder being processed
  setwd(dataPath)
  # This will tell you which file or folder is being processed
  cat(paste0("\nProcessing: ",calMeta$depid[i])) 
  # Load data
  data <- read_csv(file=paste0(dataPath,"/",calMeta$depid[i],"_Calibration-50Hz.csv"))
  depid <- as.character(calMeta$depid[i])
  model <- as.character(calMeta$model[i])
  # Define sampling rate
  fs <- 50
  # Deal with "least significant bit"
  if(model=="LSM9DS0"){
    data$ax <- (data$ax * 0.061) / 1000
    data$ay <- (data$ay * 0.061) / 1000
    data$az <- (data$az * 0.061) / 1000
  }else{
    data$ax <- (data$ax * 0.00025)
    data$ay <- (data$ay * 0.00025)
    data$az <- (data$az * 0.00025)
  }
  # Transform axes to North East Up sensor orientation used in Tag Tools 
  # Acc	 [x -y z]
  data$axT <- data$ax * (1.0)
  data$ayT <- data$ay * (-1.0)
  data$azT <- data$az * (1.0)
  # Create a matrix with Acc data
  At <- cbind(data$axT,data$ayT,data$azT)
  # Check for NA
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
  # Create an Acc sensor structure using At
  Atstruct <- sens_struct(data = Atnarm, fs = fs, depid = depid,type='acc')
  # Transform axes to North East Up sensor orientation used in Tag Tools 
  # Mag	 [x -y z]
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
  # Create an Mag sensor structure using Mt
  Mtstruct <- sens_struct(data = Mtnarm, fs = fs, depid = depid,type='mag')
  # Accelerometer Calibration
  Acrop <- crop_to(Atstruct$data,Atstruct$sampling_rate,tcues=c(calMeta$startTimeAcrop[i],calMeta$endTimeAcrop[i]))
  list <- list(A = Acrop$X)
  plott(list,fs)
  # Run Spherical Cal
  # NOTE: multiplying by gravity standard converts tag's Gs to ms2-
  AccCal <- spherical_cal(Acrop$X, n = 9.80665, method = 'cross') # Changed method from NULL on 3/13/20 # changed n from 9.81 to Arduino gravity standard for 303 on 4/7/20
  # Magnetometer Calibration
  Mcrop <- crop_to(Mtstruct$data,Mtstruct$sampling_rate,tcues=c(calMeta$startTimeMcrop[i],calMeta$endTimeMcrop[i]))
  list <- list(M = Mcrop$X)
  plott(list,fs)
  # Input the magnetic Field Strength for the location
  fieldStrength <- as.numeric(calMeta$fieldStrength[i])
  MagCal <- spherical_cal(Mcrop$X, n = fieldStrength, method = 'cross') # Changed this from NULL on 3/13/20
  #Compute field intensity of acceleration and magnetometer data, 
  #and the inclination angle of the magnetic field. This is useful for 
  #checking the quality of a calibration, for detecting drift, and for 
  #validating the mapping of the sensor axes to the tag axes.
  AMcheck <- check_AM(A=AccCal$Y,M= MagCal$Y,fs=fs)
  Ac <- apply_cal(Atnarm,cal = AccCal$G, T = NULL)
  median(AMcheck$fstr[,1])
  mean(AMcheck$fstr[,1]) 
  # Combine calibration estimations into one dataframe
  cal <-data.frame(cbind(AccCal$G$poly,AccCal$G$cross,MagCal$G$poly,MagCal$G$cross))
  colnames(cal) <-c("AccPoly1","AccPoly2","AccCross1","AccCross2","AccCross3",
                    "MagPoly1","MagPoly2","MagCross1","MagCross2","MagCross3")
  # Save new calibration values
  write.csv(cal, file=paste0(depid,"-crossMethod-bitFix.csv"), row.names = FALSE)
}


# Option 2: Manually crop data (Acrop and Mcrop) ------------------------

# Load data
filename <- file.choose("Select the CSV to import")
data <- read_csv(filename)
# Define model
model <- strsplit(basename(filename),'_')[[1]]
model <- model[1]
# Define depid for saving Rdata
depid <- strsplit(basename(filename),'_')[[1]]
depid <- paste0(depid[1],"_",depid[2])
# Add identifiers to calMeta dataframe
temp <- data.frame(cbind(model,depid))
temp$model<-as.character(model)
temp$depid<-as.character(depid)

# Process data
# Define sampling rate
fs <- 50

# Deal with "least significant bit" aka "bitFix"
if(model=="LSM9DS0"){
  data$ax <- (data$ax * 0.061) / 1000
  data$ay <- (data$ay * 0.061) / 1000
  data$az <- (data$az * 0.061) / 1000
}else{
  data$ax <- (data$ax * 0.00025) # This is *0.004/16. Will get Acrop values back to one. /16 shift bits and *.004 gets back to G
  data$ay <- (data$ay * 0.00025)
  data$az <- (data$az * 0.00025)
}

# Transform axes to North East Up sensor orientation used in Tag Tools 
# Acc	 [x -y z]
data$axT <- data$ax * (1.0)
data$ayT <- data$ay * (-1.0)
data$azT <- data$az * (1.0)
# Create a matrix with Acc data
At <- cbind(data$axT,data$ayT,data$azT)
# Check for NA
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
# Create an Acc sensor structure using At
Atstruct <- sens_struct(data = Atnarm, fs = fs, depid = depid,type='acc')
# Plot 
Alist <- list(A = Atstruct$data)
plott(Alist,fs)

# Transform axes to North East Up sensor orientation used in Tag Tools 
# Mag	 [x -y z]
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
# Create an Mag sensor structure using Mt
Mtstruct <- sens_struct(data = Mtnarm, fs = fs, depid = depid,type='mag')
# Plot 
Mlist <- list(M = Mtstruct$data)
plott(Mlist,fs)

# Accelerometer Calibration
Acrop <- crop(Atstruct$data, Atstruct$sampling_rate)
# Plot 
list <- list(A = Acrop$Y)
plott(list,fs)
# Save time cues
temp$startTimeAcrop <- Acrop$tcues[1]
temp$endTimeAcrop <- Acrop$tcues[2]

# If needed refine Acrop
Acrop <- crop(Acrop$Y, Atstruct$sampling_rate)
# Check refinement
list <- list(A = Acrop$Y)
plott(list,fs)

# Refine time cues
temp$endTimeAcrop <- as.numeric(temp$startTimeAcrop[1]+Acrop$tcues[2]) # Must calculate endTime first so you don't override startTime and lose intial value
temp$startTimeAcrop <- as.numeric(temp$startTimeAcrop[1] + Acrop$tcues[1])

# Chose not to use thresholds on Acc data (only on Mag, if necessary)
# ## NOTE: This Section is optional, only use if spikes in Acc data. This turns values outside of threshold into NA and then interpolates.
# # x-axis
# hiThresh <- 20000
# loThresh <- -20000
# AcropThresh <- Acrop$Y[,1]
# AcropThresh[AcropThresh > hiThresh] = NA
# AcropThresh[AcropThresh < loThresh] = NA
# sum(is.na(AcropThresh))
# AcropThresh <- na.approx(AcropThresh, na.rm = FALSE)
# list <- list(A = AcropThresh)
# plott(list,fs)
# sum(is.na(AcropThresh))
# # If all looks good, save data back to Acrop
# Acrop$Y[,1] <- AcropThresh
# # y-axis
# hiThresh <- 20000
# loThresh <- -20000
# AcropThresh <- Acrop$Y[,2]
# AcropThresh[AcropThresh > hiThresh] = NA
# AcropThresh[AcropThresh < loThresh] = NA
# sum(is.na(AcropThresh))
# AcropThresh <- na.approx(AcropThresh, na.rm = FALSE)
# list <- list(A = AcropThresh)
# plott(list,fs)
# sum(is.na(AcropThresh))
# # If all looks good, save data back to Acrop
# Acrop$Y[,2] <- AcropThresh
# # z-axis
# hiThresh <- 20000
# loThresh <- -20000
# AcropThresh <- Acrop$Y[,3]
# AcropThresh[AcropThresh > hiThresh] = NA
# AcropThresh[AcropThresh < loThresh] = NA
# sum(is.na(AcropThresh))
# AcropThresh <- na.approx(AcropThresh, na.rm = FALSE)
# list <- list(A = AcropThresh)
# plott(list,fs)
# sum(is.na(AcropThresh))
# # If all looks good, save data back to Acrop
# Acrop$Y[,3] <- AcropThresh

# Run Spherical Cal
AccCal <- spherical_cal(Acrop$Y, n = 9.80665, method = 'cross') # Changed this from NULL on 3/13/20

# Magnetometer Calibration
Mcrop <- crop(Mtstruct$data, sampling_rate = fs)
# Plot 
list <- list(M = Mcrop$Y)
plott(list,fs)
# Save time cues
temp$startTimeMcrop <- Mcrop$tcues[1]
temp$endTimeMcrop <- Mcrop$tcues[2]

# If needed refine Mcrop
Mcrop <- crop(Mcrop$Y, Mtstruct$sampling_rate)
# Check refinement
list <- list(M = Mcrop$Y)
plott(list,fs)
# Refine time cues
temp$endTimeMcrop <- as.numeric(temp$startTimeMcrop[1]+Mcrop$tcues[2])
temp$startTimeMcrop <- as.numeric(temp$startTimeMcrop[1] + Mcrop$tcues[1])

# Input the magnetic Field Strength for the location (https://www.ngdc.noaa.gov/geomag-web/#igrfwmm)
# NOTE: website provides data in nano teslas, need to divide by 1000 
# 47518.7 / 1000 = 47.52 # 2018 June SphereCal Asilomar 
# 28290.8 / 1000 = 28.29 # 2018 July SphereCal Falklands 
# 47954.3 / 1000 = 47.95 # 2019 Feb SphereCal Stanford

fieldStrength <- 47.95
temp$fieldStrength <- fieldStrength
MagCal <- spherical_cal(Mcrop$Y, n = fieldStrength, method = 'cross') # Changed this from NULL on 3/13/20

# Compute field intensity of acc and mag data and inclination angle of mag field
# This checks the quality of a calibration for detecting drift and validating the mapping of the sensor axes to the tag axes
AMcheck <- check_AM(A=AccCal$Y,M= MagCal$Y,fs=fs)
# If it's a good calibration the mean should be around 9.81 ms2- 
list <- list(A = AMcheck$fstr)
plott(list,fs)

Ac <- apply_cal(Atnarm,cal = AccCal$G, T = NULL)
list <- list(A = Ac)
plott(list,fs)

# If it's a good calibration the mean should be around 9.81 ms2- 
median(AMcheck$fstr[,1])
temp$meanAccFstr<-mean(AMcheck$fstr[,1])  # Checks acc field strength (column 1) # This could be higher than 10 because it's not static only. Dynamic is also included.
# mean(AMcheck$fstr[,2]) # Checks mag field strength (column 2)
# plot(AMcheck$fstr[100000:1000000,1])  # Plots acc field strength (column 1)
# 
# pStart <- 1
# pEnd <- 1000
# ggplot()+
#   geom_hline(aes(yintercept=9.81),alpha=.8) +
#   geom_line(aes(x=seq(1,pEnd-pStart+1),y=AMcheck$fstr[pStart:pEnd,1]))
# ggplotly()

# Combine calibration estimations into one dataframe
cal <-data.frame(cbind(AccCal$G$poly,AccCal$G$cross,MagCal$G$poly,MagCal$G$cross))
colnames(cal) <-c("AccPoly1","AccPoly2","AccCross1","AccCross2","AccCross3",
                  "MagPoly1","MagPoly2","MagCross1","MagCross2","MagCross3")

# Save new calibration values
write.csv(cal, file=paste0(depid,"-crossMethod-bitFix.csv"), row.names = FALSE)

# Add tcues to calMeta dataframe
# Only need to create calMeta once
# calMeta <- temp
calMeta <- rbind(calMeta,temp)
# Save as .RData
save(calMeta,file="calMeta.RData")

# Clean environment -------------------------------------------------------
rm(Ac,AccCal,Acrop,Alist,AMcheck,At,Atnarm,Atstruct,cal,data,depid,fieldStrength,filename,list,
   MagCal,Mcrop,Mtnarm,Mtstruct,Mt,Mlist)


# Bit fix notes ---------------------------------------------------------
# https://github.com/adafruit/Adafruit_LSM9DS0_Library/blob/master/Adafruit_LSM9DS0.cpp

# LSM303
# static float _lsm303Accel_MG_LSB = 0.001F; // 1, 2, 4 or 12 mg per lsb
# 1/1000*4 = the number we're going to multiple by 0.004
# to go from the datasheet to the numbers that create gravity, 
# the gravity standard is 9.80665
# multiply raw value by 0.004, and then they multiply by 9.80665 
# event->acceleration.x = (float)raw.x * _lsm303Accel_MG_LSB * SENSORS_GRAVITY_STANDARD;
# event->acceleration.y = (float)raw.y * _lsm303Accel_MG_LSB * SENSORS_GRAVITY_STANDARD;
# event->acceleration.z = (float)raw.z * _lsm303Accel_MG_LSB * SENSORS_GRAVITY_STANDARD;

# SENSORS_GRAVITY_EARTH (9.80665F) /**< Earth's gravity in m/s^2 */
# DLSM90 define LSM9DS0_ACCEL_MG_LSB_2G (0.061F)

# LSM9so0
# define LSM9DS0_ACCEL_MG_LSB_2G (0.061F)
# event->acceleration.x = accelData.x * _accel_mg_lsb;
# event->acceleration.x /= 1000;
# event->acceleration.x *= SENSORS_GRAVITY_STANDARD;
# event->acceleration.y = accelData.y * _accel_mg_lsb;
# event->acceleration.y /= 1000;
# event->acceleration.y *= SENSORS_GRAVITY_STANDARD;
# event->acceleration.z = accelData.z * _accel_mg_lsb;
# event->acceleration.z /= 1000;
# event->acceleration.z *= SENSORS_GRAVITY_STANDARD;

