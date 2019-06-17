###############################################################
####    After csv combined, create uniform sampling        ####
####                 Multiple files at once                ####
###############################################################

library(tidyverse)
library(ggplot2)
require(rChoiceDialogs)
pathChoice = rchoose.dir()
# pathChoice = rchoose.dir(default = "C:\\",caption = "Select folder containing CSV Files")
# This imports all CSV files in the directory chosen
filenames <- list.files(path = pathChoice, pattern = "*.csv", all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE)
# Run make48hz function below before running import.list
import.list <- lapply(file.path(pathChoice,filenames), make48hz)

# Run this function (all the way through create_csv) before running import.list
make48hz <- function(filenameA){
  # read acc file
  data <- read_csv(filenameA)
  data2 <- data %>%
    # seconds since the beginning
    mutate(secs_since = as.numeric(dt - min(dt))) %>%
    # group into within-seconds blocks
    group_by(secs_since) %>%
    # frequency and period of sampling
    dplyr::mutate(freq = n(),
                  period = 1 / n(),
                  # fraction of a second since beginning of second (i.e. 0-1)
                  frac_sec = (row_number() - 1) / n(),
                  # seconds since beginning (e.g. 9.456)
                  true_since = secs_since + frac_sec) %>%
    ungroup %>%
    # Filter out first and last seconds because they're partial
    filter(secs_since > 0,
           secs_since < max(secs_since)) %>%
    # true time down to fractional second (e.g. 2018-06-07 16:57:12.1234)
    mutate(true_time = min(dttz) + true_since)
  # show a table with the frequency of frequencies
  data2$freq %>% table
  # use frequency table to judge best resampling rate
  new_freq <- 48
  # "- 1" b/c true_since starts at 1 (we drop second 0)
  temp_approx <- approxfun(data2$true_since - 1, 
                         data2$temp, 
                         method = 'constant')
  ax_approx <- approxfun(data2$true_since - 1, 
                         data2$ax, 
                         method = 'constant')
  ay_approx <- approxfun(data2$true_since - 1, 
                         data2$ay, 
                         method = 'constant')
  az_approx <- approxfun(data2$true_since - 1, 
                         data2$az, 
                         method = 'constant')
  mx_approx <- approxfun(data2$true_since - 1, 
                         data2$mx, 
                         method = 'constant')
  my_approx <- approxfun(data2$true_since - 1, 
                         data2$my, 
                         method = 'constant')
  mz_approx <- approxfun(data2$true_since - 1, 
                         data2$mz, 
                         method = 'constant')
  gx_approx <- approxfun(data2$true_since - 1, 
                         data2$gx, 
                         method = 'constant')
  gy_approx <- approxfun(data2$true_since - 1, 
                         data2$gy, 
                         method = 'constant')
  gz_approx <- approxfun(data2$true_since - 1, 
                         data2$gz, 
                         method = 'constant')
  data3 <- data.frame(dttz = seq(from = min(data2$true_time),
                                 to = max(data2$true_time),
                                 by = 1 / new_freq)) %>%
    mutate(num_time = as.numeric(dttz - min(dttz)),
           temp = temp_approx(num_time),
           ax = ax_approx(num_time),
           ay = ay_approx(num_time),
           az = az_approx(num_time),
           mx = mx_approx(num_time),
           my = my_approx(num_time),
           mz = mz_approx(num_time),
           gx = gx_approx(num_time),
           gy = gy_approx(num_time),
           gz = gz_approx(num_time))
  
  # check results
  # x11()
  # data2 %>%
  #   slice(1e6:(1e6+100)) %>%
  #   ggplot(aes(x = true_time,
  #              y = mx)) +
  #   geom_line() +
  #   geom_point(size = 1) +
  #   labs(title = 'Original data')
  # 
  # time_rng <- range(data2$true_time[1e6:(1e6+100)])
  # x11()
  # data3 %>%
  #   filter(between(dttz, time_rng[1], time_rng[2])) %>%
  #   ggplot(aes(x = dttz,
  #              y = mx)) +
  #   geom_line() +
  #   geom_point(size = 1) +
  #   labs(title = 'Rediscretized data')
  
  write_csv(data3, file.path(dirname(filenameA), paste("48HZ-",basename(filenameA),sep="")))
} 

##########################################
####        Create NC file           #####
##########################################

# Import rediscretized data
filename <- file.choose()
data48hz <- read_csv(filename)
depid <- basename(filename) # add code to remove "48HZ-" and "-COMBINED.csv"



##########################################
####         Acc Processing          #####
##########################################

# Tag data are raw values from sensor and need to be converted to ms-2
# (Acc orientation is North West Up in raw data)
# Calculated values can be obtained by:
#   event->acceleration.x = accelData.x * _accel_mg_lsb (0.061)
#   event->acceleration.x /= 1000
#   event->acceleration.x *= SENSORS_GRAVITY_STANDARD (9.80665)
# Linear Acceleration: mg per LSB
#define LSM9DS0_ACCEL_MG_LSB_2G (0.061F)
#define LSM9DS0_ACCEL_MG_LSB_4G (0.122F)
#define LSM9DS0_ACCEL_MG_LSB_6G (0.183F)
#define LSM9DS0_ACCEL_MG_LSB_8G (0.244F)
#define LSM9DS0_ACCEL_MG_LSB_16G (0.732F) 

# Also need to transform to North East Up sensor orientation used in Tag Tools 
# Acc	 [x -y z]
data48hz$axms2 <- ((data48hz$ax * 0.061)/1000)* 9.8065
data48hz$ayms2 <- (((data48hz$ay * 0.061)/1000)* 9.8065)* (-1)
data48hz$azms2 <- ((data48hz$az * 0.061)/1000)* 9.8065
# Create a matrix with Acc data
At <- cbind(data48hz$axms2,data48hz$ayms2,data48hz$azms2)
# Check for NA
count(is.na(At))
# If NAs, remove using linear approximation
Atnarm <- At
Atnarm <- na.approx(Atnarm, na.rm = FALSE)
# Check again for NAs
count(is.na(Atnarm))
# Create an Acc sensor structure using At
Atstruct <- sens_struct(data = Atnarm, fs = 48, depid = 'T1D20170212',type='acc')

##########################################
####         Mag Processing          #####
##########################################

# Tag data are raw values from sensor and need to be converted to Gauss
# (Mag orientation is North West Down in raw data)
# Calculated values can be obtained by:
#   event->magnetic.x = magData.x * _mag_mgauss_lsb (0.08F);
#   event->magnetic.x /= 1000; 
# Magnetic Field Strength: gauss range
#define LSM9DS0_MAG_MGAUSS_2GAUSS      (0.08F)
#define LSM9DS0_MAG_MGAUSS_4GAUSS      (0.16F)
#define LSM9DS0_MAG_MGAUSS_8GAUSS      (0.32F)
#define LSM9DS0_MAG_MGAUSS_12GAUSS     (0.48F)

# Also need to transform to North East Up sensor orientation used in Tag Tools
# Mag	 [x -y -z]

data48hz$mxG <- ((data48hz$mx * 0.08)/1000)
data48hz$myG <- ((data48hz$my * 0.08)/1000)* (-1)
data48hz$mzG <- ((data48hz$mz * 0.08)/1000)* (-1)
# Create a matrix with Mag data
Mt <- cbind(data48hz$mxG,data48hz$myG,data48hz$mzG)
# Check for NA
count(is.na(Mt))
# If NAs, remove using linear approximation
Mtnarm <- Mt
Mtnarm <- na.approx(Mtnarm, na.rm = FALSE)
# Check again for NAs
count(is.na(Mtnarm))
# Create an Acc sensor structure using At
Mtstruct <- sens_struct(data = Mtnarm, fs = 48, depid = 'T1D20170212',type='mag')

##########################################
####           Calibration           #####
##########################################


# 
AMcheck <- check_AM(A=Atnarm,M= Mtnarm,fs=48)

# Plot 
list <- list(A = Atstruct$data)
plott(list, Atstruct$sampling_rate)

##########################################
####         Calculate ODBA          #####
##########################################

# Calculate ODBA (Need to read about Wilson method, filter pass, and n)
e <- odba(A = Atnarm, sampling_rate = Atstruct$sampling_rate, method = 'wilson', n = 1)
ba <- list(odba = e)
plott(ba, Atstruct$sampling_rate)

list <- a2pr(samplematrix)

