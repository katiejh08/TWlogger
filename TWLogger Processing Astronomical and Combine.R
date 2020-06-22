# Setup Environment -------------------------------------------------------

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
pkgTest("leaflet")
pkgTest("htmlwidgets")
pkgTest("argosfilter")
pkgTest("ggmap")
pkgTest("maps")
pkgTest("mapdata")
pkgTest("maptools")
pkgTest("sp")

# Select parent folder that contains all deployment folders
dataPath <- choose.dir(caption="Select the folder containing deployment raw data folders") 
# Select folder that contains meta data
metaPath <- choose.dir(caption="Select the folder containing meta data") 
# Select output folder
savePath <- choose.dir(caption="Select the folder containing output folder")

# Set working directory to location of meta data
setwd(metaPath)
load("depMeta.RData")
load("depSum.RData") # This was created using this script (Option 2 below)

# Filter depMeta to include only the logger platform for which you're processing
depMeta <- depMeta %>% 
  dplyr::filter(platform=="TW",
                island=="SDI") %>% 
  arrange(depid)

# Set the Working Directory to the location of this File
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(current_path)
getwd()

# NOTE: only load if needing to recreate depSum
# source("../Global Functions/find_Astronomical.R") # Finds Sunrise and sunset times
# source("../Global Functions/pt2pt_fxns.R") # Finds Point to Point metrics

# Set the Timezone Offset from UTC 
tzOffset <- "Etc/GMT+3"

# This script includes two options:
# 1) Use depSum to add previously calculated astronomical data
# 2) Create or recreate depSum from each deployment

# Option 1: Add astronomical variables using depSum -----------------------
for(i in 1:length(depMeta$depid)) {
  # Save working directory so you can return to it at end of for loop
  current_path <- getwd()
  # Resets working directory to deployment folder being processed
  setwd(paste0(dataPath,"/",depMeta$depid[i],"/"))
  # This will tell you which file or folder is being processed
  cat(paste0("\nProcessing: ",depMeta$depid[i])) 
  load(paste0(dataPath,"/",depMeta$depid[i],"/",depMeta$depid[i],"-1Hz-VDBA-ODBA-clipVal-bitFix.RData"))
  # load(paste0(dataPath,"/",depMeta$depid[i],"/",depMeta$depid[i],"-10Hz-VDBA-ODBA-clipVal-bitFix.RData"))
  depid <- data$depid[1]
  
  # Add helpful variables
  data <- left_join(data,dplyr::select(depSum,depid,lat,long,srise:solarnoon,solarMidnight,dawnT,duskT), by = "depid")
  data <- data %>%
    # Add astronomical variables
    dplyr:: mutate(yday = yday(dttz),
                   season = as.factor(ifelse(yday > 173 & yday <= 265,"Winter",
                                             ifelse(yday >= 355 | yday <= 79,"Summer","Other"))),
                                              # yday > 79 & yday <= 173,"Fall" # Add this back if needed
                                              # yday > 265 & yday < 355, "Spring" # Add back in if needed
                   time = hms::as.hms(dttz,tz = tzOffset),
                   timeBin = floor(as.numeric(difftime(time,solarMidnight,units = "hours"))),
                   timeBin = ifelse(timeBin < 0,timeBin+24,timeBin),
                   hr = hour(dttz),
                   yr = year(dttz)) %>% 
    # Add tag model
    dplyr:: mutate(model = ifelse(yr=="2017","LSM9DS0","LSM303")) %>% 
    # Remove unnecessary variables
    select(-c(srise,sset,dawn,dusk,yday))
  
  # Combine into one dataset
  if(i==1) dataset <- data else dataset <- rbind(dataset,data)
  setwd(current_path)
  rm(current_path)
}

# Save as .RData file
setwd(savePath)
save(dataset, file="dataset-1Hz-VDBA-ODBA-clipVal-bitFix.RData")
# save(dataset,file="dataset-10Hz-VDBA-ODB-clipVal-bitFix.RData")

# Create or recreate depSum from each deployment --------------------------

# Combine deployments into one dataset
for(i in 1:length(depMeta$depid)) {
  # Save working directory so you can return to it at end of for loop
  current_path <- getwd()
  # Resets working directory to deployment folder being processed
  setwd(paste0(dataPath,"/",depMeta$depid[i],"/"))
  # This will tell you which file or folder is being processed (good for debugging)
  cat(paste0("\nProcessing: ",depMeta$depid[i])) 
  if(depMeta$island=="SDI"){
    # load(paste0(dataPath,"/",depMeta$depid[i],"/",depMeta$depid[i],"-1Hz-VDBA-ODBA-clipVal-bitFix.RData"))
    load(paste0(dataPath,"/",depMeta$depid[i],"/",depMeta$depid[i],"-10Hz-VDBA-ODBA-clipVal-bitFix.RData"))
  }else if(depMeta$island=="NWI"){
    load(paste0(dataPath,"/",depMeta$depid[i],"/",depMeta$depid[i],"-1Hz.RData"))
    load(paste0(dataPath,"/",depMeta$depid[i],"/",depMeta$depid[i],"-10Hz.RData"))
  }
  # Combine into one dataset
  if(i==1) dataset <- data else dataset <- rbind(dataset,data)
  setwd(current_path)
  rm(current_path)
}

# Create deployment summary
depSum <- dataset %>% 
  group_by(depid) %>% 
  summarize(depDate = first(dttz)) # Creates table with deployment date
depSum$lat <- mean(TWgps$lat)
depSum$long <- mean(TWgps$long)

#Find Sunrise and sunset times
astro_data <- find_Astronomical(depSum$long,
                                depSum$lat,
                                depSum$depDate)
astro_data$long <- NULL
astro_data$lat <- NULL
depSum <- cbind(depSum,astro_data)

# Create SolarMidnight based hourly bins
depSum$solarMidnight <- hms::as.hms((depSum$solarnoon- dhours(12)),tz = tzOffset)
depSum$dawnT <- (hour(depSum$dawn) + minute(depSum$dawn)/60) - (hour(depSum$solarMidnight) + minute(depSum$solarMidnight)/60)
depSum$duskT <- hour(depSum$dusk) + minute(depSum$dusk)/60 - (hour(depSum$solarMidnight) + minute(depSum$solarMidnight)/60)

# Save as an RData file
setwd(savePath)
save(astro_data, file=paste0("astro_data.RData"))
save(depSum, file=paste0("depSumTW.RData"))

# Quick fix to add depid to depSum ----------------------------------------

# colnames(depSum)[1]<-"depid_old" # Rename old depid so it's not in the way
# depSum$ID<-substr(depSum$depid,1,3) # Create ID to have a join variable for depMeta
# depSum<-left_join(depSum,dplyr::select(depMeta,ID,depid),by="ID")
# # Reorder and rename columns
# depSum <- depSum[,c("depid","ID","depid_old","depDate","lat","long","dttz","srise","sset","dawn","dusk",
#                     "solarpos","solarnoon","night","astronomical","solarMidnight","dawnT","duskT")]
# colnames(depSum) <- c("depid","depid2","depDate","lat","long","dttz","srise","sset","dawn","dusk",
#                       "solarpos","solarnoon","night","astronomical","solarMidnight","dawnT","duskT")
# depSum$night<-NULL


# Recalculate astronomical for dataset ------------------------------------
names(dataset)
dataset$night<-NULL
dataset$astronomical<-NULL
dataset$lat<-NULL
dataset$long<-NULL
dataset<-left_join(dataset,dplyr::select(depSum,depid,lat,long,srise,sset,dawn,dusk),by="depid")

# Note: this includes crepuscular with daytime
dataset$night <- ifelse(hms::as.hms(dataset$dttz, tz=attr(dataset$dttz, "tzone")) < hms::as.hms(dataset$dawn, tz=attr(dataset$dttz, "tzone")) | hms::as.hms(dataset$dttz, tz=attr(dataset$dttz, "tzone")) > hms::as.hms(dataset$dusk, tz=attr(dataset$dttz, "tzone")),'night','day')
# Determine if day night dusk or dawn
dataset$astronomical <- ifelse(hms::as.hms(dataset$dttz, tz=attr(dataset$dttz, "tzone")) > hms::as.hms(dataset$srise, tz=attr(dataset$dttz, "tzone")) & hms::as.hms(dataset$dttz, tz=attr(dataset$dttz, "tzone")) < hms::as.hms(dataset$sset, tz=attr(dataset$dttz, "tzone")),'day', 
                            ifelse(hms::as.hms(dataset$dttz, tz=attr(dataset$dttz, "tzone")) < hms::as.hms(dataset$srise, tz=attr(dataset$dttz, "tzone")) & hms::as.hms(dataset$dttz, tz=attr(dataset$dttz, "tzone")) > hms::as.hms(dataset$dawn, tz=attr(dataset$dttz, "tzone")),'dawn',
                                   ifelse(hms::as.hms(dataset$dttz, tz=attr(dataset$dttz, "tzone")) < hms::as.hms(dataset$dusk, tz=attr(dataset$dttz, "tzone")) & hms::as.hms(dataset$dttz, tz=attr(dataset$dttz, "tzone")) > hms::as.hms(dataset$sset, tz=attr(dataset$dttz, "tzone")) ,'dusk','night')))

