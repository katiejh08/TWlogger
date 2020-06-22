# Setup Environment -------------------------------------------------------

# Set the Timezone Offset from UTC 
tzOffset <- "Etc/GMT+3" # Falklands time in the winter
# Set the Working Directory to the location of this File
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(current_path)
getwd()

# Create a function to allow you to check if data exists in the dataframe
`%notin%` <- Negate(`%in%`)

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
pkgTest("ggplot2")
pkgTest("lubridate")
pkgTest("leaflet")
pkgTest("htmlwidgets")
pkgTest("argosfilter")
pkgTest("zoo")
pkgTest("dplyr")
pkgTest("tagtools")
pkgTest("plotly")
pkgTest("RcppRoll")
pkgTest("car")
pkgTest("gridExtra")
pkgTest("signal")
pkgTest("ggpubr")
pkgTest("ggmap")
pkgTest("maps")
pkgTest("mapdata")

# Load data ---------------------------------------------------------------

# Load depMeta
load("depMeta.RData")
# Select data to import
load(file.choose())
# Preserve original data
datax <- data
# Create depid (deployment ID)
depid <- data$name[1]

# Part A: Calibrate by applying benchCal values ------------------------------------------------------------
fs <- 50 # define sampling rate  

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
# Transform axes of Mag to orientation used in Tag Tools
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

# Import calibration file for logger
# Check which file is needed
cat(paste0("\nSelect calibration file named: ",depMeta$calCross[depMeta$depid==depid][1]))

cal <- read_csv(file.choose())
AccCal <- list(poly = cbind(cal$AccPoly1,cal$AccPoly2), cross = cbind(cal$AccCross1,cal$AccCross2,cal$AccCross3))
MagCal <- list(poly = cbind(cal$MagPoly1,cal$MagPoly2), cross = cbind(cal$MagCross1,cal$MagCross2,cal$MagCross3))

AtCal <- apply_cal(Atnarm,cal = AccCal, T = NULL)
# list <- list(A = AtCal[10000:180000,])
# plott(list,50)
MtCal <- apply_cal(Mtnarm,cal = MagCal, T = NULL)
# list <- list(M = MtCal[10000:300000,])
# plott(list,fs, interactive = T)
# plott(list,fs)

    ## NOTE: This Section is optional, only use if spikes in Mag Data. This turns values outside of threshold into NA and then interpolates.
    # hiThresh <- 45 # Look at plotted magnetometer for bad hits and find a threshold
    # loThresh <- -45
    # MtCalThresh <- MtCal
    # MtCalThresh[MtCalThresh > hiThresh] = NA
    # MtCalThresh[MtCalThresh < loThresh] = NA
    # sum(is.na(MtCalThresh))
    # MtCalThresh <- na.approx(MtCalThresh, na.rm = FALSE)
    # 
    # list <- list(M = MtCalThresh[10000:300000,])
    # plott(list,fs)
    # sum(is.na(MtCalThresh))
    # # If all looks Good, Save the Data back to MtCal
    # MtCal <- MtCalThresh

#Compute field intensity of acceleration and magnetometer data and the inclination angle of the magnetic field. This is useful for
#checking the quality of a calibration, for detecting drift, and for validating the mapping of the sensor axes to the tag axes.
AMcheck <- check_AM(A=AtCal,M=MtCal,fs=fs)
# If it's a good calibration the mean should be around 9.81 ms2- 
median(AMcheck$fstr[,1]) # Checks acc field strength (column 1) 
mean(AMcheck$fstr[,1])  # Checks acc field strength (column 1) # This could be higher than 10 because it's not static only. Dynamic is also included.
mean(AMcheck$fstr[,2]) # Checks mag field strength (column 2)
# plot(AMcheck$fstr[100000:1000000,1])  # Plots acc field strength (column 1)

pStart <- 10000
pEnd <- 300000
ggplot()+
  geom_hline(aes(yintercept=9.81),alpha=.8) +
  geom_line(aes(x=seq(1,pEnd-pStart+1),y=AMcheck$fstr[pStart:pEnd,1]))
# ggplotly()

# Save Calibrated Data Back To Dataframe
data2 <-cbind(data[,1:4],AtCal,MtCal,data[12:14])
# Rename Columns
colnames(data2) <- c("dttz","name","ts","temp","Ax","Ay","Az",
                     "Mx","My","Mz","secs_since","true_since","tsDiff")
benchCal <- data2
rm(data2)

save(benchCal,file=paste0(depid,"_onBird-50Hz-benchCal.RData"))


# Part B: Calibrate using in-situ method --------------------------------

# Reset data to original uncalibrated upload
data <- datax 



# Part B1: Decimate data to 1-Hz ---------------------------------------------------

# Down sample each vector separately
df <- 50 # Set decimation factor df
fs <- 50 # Set original sampling rate

# For datetime select every nth value
dttz <- data$dttz
a <- dttz
dttz_down <- a[seq(1, length(a), df)]
# For true_since select every nth value
true_since <- data$true_since
a <- true_since
true_since_down <- a[seq(1, length(a), df)]

# Create individual vectors from acc fields
Ax <- data$ax
Ay <- data$ay
Az <- data$az
# Convert vectors to numeric matrix
Ax_mat <- matrix(Ax,ncol=1)
Ay_mat <- matrix(Ay,ncol=1)
Az_mat <- matrix(Az,ncol=1)
# Use decimate function
Ax_down <- decimate(Ax_mat,df,ftype="fir")
Ay_down <- decimate(Ay_mat,df,ftype="fir")
Az_down <- decimate(Az_mat,df,ftype="fir")

# Create individual vectors from mag fields
Mx <- data$mx
My <- data$my
Mz <- data$mz
# Convert vectors to numeric matrix
Mx_mat <- matrix(Mx,ncol=1)
My_mat <- matrix(My,ncol=1)
Mz_mat <- matrix(Mz,ncol=1)
# Use decimate function
Mx_down <- decimate(Mx_mat,df,ftype="fir")
My_down <- decimate(My_mat,df,ftype="fir")
Mz_down <- decimate(Mz_mat,df,ftype="fir")

# Combine down sampled data into one dataframe
data_down <- cbind.data.frame(dttz_down,true_since_down,Ax_down,Ay_down,Az_down,Mx_down,My_down,Mz_down)
data <- data_down
# Create Bird ID after downsampling
data$ID <- depid
# Change column names to more practical shorter names
colnames(data)[1:9] <- c("dttz","true_since","Ax","Ay","Az","Mx","My","Mz","ID")
# Reorder columns
data <- data[,c("ID","dttz","true_since","Ax","Ay","Az","Mx","My","Mz")]


# Part B2: Extract calibration values -------------------------------------

# Redefine fs 
fs = fs/df

# Transform axes to North East Up sensor orientation used in Tag Tools 
# Acc	 [x -y z]
data$axT <- data$Ax * (1.0)
data$ayT <- data$Ay * (-1.0)
data$azT <- data$Az * (1.0)
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

# Create an Acc sensor structure using At (NOTE: manually change fs value if not 50)
Atstruct <- sens_struct(data = Atnarm, fs = fs, depid = depid,type='acc') # fs is 1hz
# Plot 
Alist <- list(A = Atstruct$data)
plott(Alist,fs) # fs is 1Hz

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
# Create an Mag sensor structure using Mt (NOTE: manually change fs value if not 50)
Mtstruct <- sens_struct(data = Mtnarm, fs = fs, depid = depid,type='mag')
# # Plot 
# Mlist <- list(M = Mtstruct$data)
# plott(Mlist,fs)

# Accelerometer Calibration
Acrop <- crop(Atstruct$data, Atstruct$sampling_rate)

# Run Spherical Cal
AccCal <- spherical_cal(Acrop$Y, n = 9.81, method = 'cross') # Changed this from NULL on 3/13/20

# Magnetometer Calibration
Mcrop <- crop(Mtstruct$data, sampling_rate = fs)

# Magnetometer Calibration

# Input the magnetic Field Strength for the location (https://www.ngdc.noaa.gov/geomag-web/#igrfwmm)
# NOTE: website provides data in nano teslas, need to divide by 1000 
# SDI coordinates are -51.366602°and -60.087346°
fieldStrength <- depMeta$magneticField[depMeta$depid==depid]
MagCal <- spherical_cal(Mcrop$Y, n = fieldStrength, method = 'cross') # Changed this from NULL on 3/13/20

#Compute field intensity of acceleration and magnetometer data, 
#and the inclination angle of the magnetic field. This is useful for 
#checking the quality of a calibration, for detecting drift, and for 
#validating the mapping of the sensor axes to the tag axes.
AMcheck <- check_AM(A=AccCal$Y,M= AccCal$Y,fs=fs) #KJH: Changed MagCal to AccCal on 3/23/20 after removing Mag data
# If it's a good calibration the mean should be around 9.81 ms2- 
list <- list(A = AMcheck$fstr)
plott(list,fs)

# If it's a good calibration the mean should be around 9.81 ms2- 
median(AMcheck$fstr[,1])
mean(AMcheck$fstr[,1])  # Checks acc field strength (column 1) # This could be higher than 10 because it's not static only. Dynamic is also included.
mean(AMcheck$fstr[,2]) # Checks mag field strength (column 2)
plot(AMcheck$fstr[1000:10000,1])  # Plots acc field strength (column 1)

pStart <- 1
pEnd <- 1000000
ggplot()+
  geom_hline(aes(yintercept=9.81),alpha=.8) +
  geom_line(aes(x=seq(1,pEnd-pStart+1),y=AMcheck$fstr[pStart:pEnd,1]))
ggplotly()

# # Save Acrop and Mcrop for future use
# save(Acrop, file=paste0(depid,"-Acrop.RData"))
# save(Mcrop, file=paste0(depid,"-Mcrop.RData"))

# Combine calibration estimations into one dataframe
cal <-data.frame(cbind(AccCal$G$poly,AccCal$G$cross,MagCal$G$poly,MagCal$G$cross))
colnames(cal) <-c("AccPoly1","AccPoly2","AccCross1","AccCross2","AccCross3",
                  "MagPoly1","MagPoly2","MagCross1","MagCross2","MagCross3")


# Save new calibration values
save(cal, file=paste0(depid,"-crossMethod-inSitu.RData"))
write.csv(cal, file=paste0(depid,"-crossMethod-inSitu.csv"), row.names = FALSE)

## Apply calibration values to original data

# Reset data to original uncalibrated upload
data <- datax 
# Reset fs to fs of original data
fs <- 50

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

# Transform axes of Mag to orientation used in Tag Tools
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

# Import calibration file for logger
# NOTE: cal is already in environment (if created above)
# Check AM field strength (if it doesn't look right, calibrate based on field collected data)
AccCal <- list(poly = cbind(cal$AccPoly1,cal$AccPoly2), cross = cbind(cal$AccCross1,cal$AccCross2,cal$AccCross3))
MagCal <- list(poly = cbind(cal$MagPoly1,cal$MagPoly2), cross = cbind(cal$MagCross1,cal$MagCross2,cal$MagCross3))

AtCal <- apply_cal(Atnarm,cal = AccCal, T = NULL)
list <- list(A = AtCal[10000:180000,])
plott(list,fs)

MtCal <- apply_cal(Mtnarm,cal = MagCal, T = NULL)
list <- list(M = MtCal[10000:300000,])
# plott(list,fs, interactive = T)
plott(list,fs)

      ## NOTE: This Section is optional, only use if spikes in Mag Data. This turns values outside of threshold into NA and then interpolates.
      hiThresh <- 45 # Look at plotted magnetometer for bad hits and find a threshold
      loThresh <- -45
      MtCalThresh <- MtCal
      MtCalThresh[MtCalThresh > hiThresh] = NA
      MtCalThresh[MtCalThresh < loThresh] = NA
      sum(is.na(MtCalThresh))
      MtCalThresh <- na.approx(MtCalThresh, na.rm = FALSE)
      list <- list(M = MtCalThresh[10000:300000,])
      plott(list,fs)
      sum(is.na(MtCalThresh))
      # If all looks Good, Save the Data back to MtCal
      MtCal <- MtCalThresh

#Compute field intensity of acceleration and magnetometer data and the inclination angle of the magnetic field. This is useful for
#checking the quality of a calibration, for detecting drift, and for validating the mapping of the sensor axes to the tag axes.
AMcheck <- check_AM(A=AtCal,M=MtCal,fs=fs)
# If it's a good calibration the mean should be around 9.81 ms2- 
median(AMcheck$fstr[,1])
mean(AMcheck$fstr[,1])  # Checks acc field strength (column 1) # This could be higher than 10 because it's not static only. Dynamic is also included.
mean(AMcheck$fstr[,2]) # Checks mag field strength (column 2)
# plot(AMcheck$fstr[100000:1000000,1])  # Plots acc field strength (column 1)

pStart <- 1
pEnd <- 100000
ggplot()+
  geom_hline(aes(yintercept=9.81),alpha=0.8) +
  geom_line(aes(x=seq(1,pEnd-pStart+1),y=AMcheck$fstr[pStart:pEnd,1]))

# Save Calibrated Data Back To Dataframe
inSitu_temp <-cbind(data[,1:4],AtCal,MtCal,data[12:14])
# Rename Columns
colnames(inSitu_temp) <- c("dttz","name","ts","temp","Ax","Ay","Az",
                     "Mx","My","Mz","secs_since","true_since","tsDiff")
