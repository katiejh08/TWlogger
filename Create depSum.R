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
pkgTest("ggmap")
pkgTest("maps")
pkgTest("mapdata")

# Select parent folder that contains all deployment folders
dataPath <- choose.dir(caption="Select the folder containing deployment raw data folders") 
# Selet folder that contains meta data
metaPath <- choose.dir(caption="Select the folder containing meta data") 

# Load depMeta
load("depMeta.RData")
# Filter depMeta to include only the logger platform for which you're processing (e.g. to include only TWLogger and not IGU)
depMeta <- depMeta %>% 
  dplyr::filter(platform=="TW")

# Finds Sunrise and sunset times
source("../Global Functions/find_Astronomical.R")
# Finds Point to Point metrics
source("../Global Functions/pt2pt_fxns.R")

# Set the Timezone Offset from UTC 
tzOffset <- "Etc/GMT+3" # Falklands time in the winter
# Set the Working Directory to the location of this File
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
getwd()


# Combine deployments into one dataset ------------------------------------

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

# Process data to create deployment summary ----------------------

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
# save(astro_data, file=paste0("astro_data", ".RData"))
# save(depSumTW, file=paste0("depSumTW", ".RData"))

#################################################3

# # KJH: Quick fix for depSum
# temp<-data.frame(name = depMeta$depid,depid = depMeta$depid2)
# depSum<-left_join(depSum,temp,by=depid)
# # Reorder columns
# depSum <- depSum[,c("name","depid","depDate","lat","long","dttz","srise","sset","dawn","dusk",
#                     "solarpos","solarnoon","night","astronomical","solarMidnight","dawnT","duskT")]
# # Rename columns
# colnames(depSum) <- c("depid","depid2","depDate","lat","long","dttz","srise","sset","dawn","dusk",
#                       "solarpos","solarnoon","night","astronomical","solarMidnight","dawnT","duskT")
# rm(temp)

# Select parent folder that contains all deployment folders
# dataPath <- choose.dir(caption="Select the folder containing deployment raw data folders") 
# savePath <-choose.dir(caption="Select the folder where you would like to save output")