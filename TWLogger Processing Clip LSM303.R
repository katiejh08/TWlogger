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
savePath <-choose.dir(caption="Select the folder where you would like to save output")

# Set the Timezone Offset from UTC 
tzOffset <- "Etc/GMT+3"

# Set the Working Directory to the location of this File
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(current_path)
getwd()

# Process Data ------------------------------------------------------------

# Extract outer clip values from LSM9DS0 ----------------------
# Will use these outer max and min values below to clip LSM303 data to match 2G limit of LSM9DS0 data.

load("depMeta.RData")
depMeta <- depMeta %>% 
  dplyr::filter(platform=="TW",
                island=="SDI",
                model=="LSM9DS0")

for(i in 1:length(depMeta$depid)) {
  # Save working directory so you can return to it at end of for loop
  current_path <- getwd()
  # Resets working directory to deployment folder being processed
  setwd(paste0(dataPath,"/",depMeta$depid[i],"/"))
  # This will tell you which file or folder is being processed
  cat(paste0("\nProcessing: ",depMeta$depid[i])) 
  load(paste0(dataPath,"/",depMeta$depid[i],"/",depMeta$depid[i],"-50Hz-benchCal-24hr.RData"))
  depid <- data$depid[1]
  # Extract min and max values of each depid
  temp <- data %>% 
    mutate(pos_Ax = ifelse(Ax>0,1,0), # Label positive values as 1 and negative as 0 to use below in summarize
           pos_Ay = ifelse(Ay>0,1,0),
           pos_Az = ifelse(Az>0,1,0)) %>%
    summarize(depid=first(depid),
              model="LSM9DS0",
              max_pos_Ax=round(max(Ax),6),
              min_pos_Ax=round(min(Ax[pos_Ax=="1"]),6), # This extracts the positive value closest to zero
              min_neg_Ax=round(max(Ax[pos_Ax=="0"]),6), # This extracts the negative value closest to zero
              max_neg_Ax=round(min(Ax),6), # 
              
              max_pos_Ay=round(max(Ay),6),
              min_pos_Ay=round(min(Ay[pos_Ay=="1"]),6),
              min_neg_Ay=round(max(Ay[pos_Ay=="0"]),6),
              max_neg_Ay=round(min(Ay),6),
              
              max_pos_Az=round(max(Az),6),
              min_pos_Az=round(min(Az[pos_Az=="1"]),6),
              min_neg_Az=round(max(Az[pos_Az=="0"]),6),
              max_neg_Az=round(min(Az),6))
  
  # Combine into one dataset
  if(i==1) clipVals9DS0 <- temp else clipVals9DS0 <- rbind(clipVals9DS0,temp)
  # rm(temp)
  setwd(current_path)
  rm(current_path)
}

# Extract absolute min and max from each vector
sumClipVals9DS0 <- clipVals9DS0 %>% 
  summarize(max_pos_Ax=round(max(max_pos_Ax),6),
            min_pos_Ax=round(min(min_pos_Ax),6),
            min_neg_Ax=round(max(min_neg_Ax),6),
            max_neg_Ax=round(min(max_neg_Ax),6),
            
            max_pos_Ay=round(max(max_pos_Ay),6),
            min_pos_Ay=round(min(min_pos_Ay),6),
            min_neg_Ay=round(max(min_neg_Ay),6),
            max_neg_Ay=round(min(max_neg_Ay),6),
            
            max_pos_Az=round(max(max_pos_Az),6),
            min_pos_Az=round(min(min_pos_Az),6),
            min_neg_Az=round(max(min_neg_Az),6),
            max_neg_Az=round(min(max_neg_Az),6))

# Determine min and max values of each axis for LMS303 ----------------------
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(current_path)
getwd()
load("depMeta.RData")
depMeta <- depMeta %>% 
  dplyr::filter(platform=="TW",
                island=="SDI",
                model=="LSM303")

for(i in 1:length(depMeta$depid)) {
  # Save working directory so you can return to it at end of for loop
  current_path <- getwd()
  # Resets working directory to deployment folder being processed
  setwd(paste0(dataPath,"/",depMeta$depid[i],"/"))
  # This will tell you which file or folder is being processed (good for debugging)
  cat(paste0("\nProcessing: ",depMeta$depid[i])) 
  load(paste0(dataPath,"/",depMeta$depid[i],"/",depMeta$depid[i],"-50Hz-benchCal-24hr.RData"))
  depid <- data$depid[1]
  # Extract min and max values of each depid
  temp <- data %>% 
    mutate(pos_Ax = ifelse(Ax>0,1,0),
           pos_Ay = ifelse(Ay>0,1,0),
           pos_Az = ifelse(Az>0,1,0)) %>%
    summarize(depid=first(depid),
              model="LSM9DS0",
              max_pos_Ax=round(max(Ax),6),
              min_pos_Ax=round(min(Ax[pos_Ax=="1"]),6),
              min_neg_Ax=round(max(Ax[pos_Ax=="0"]),6),
              max_neg_Ax=round(min(Ax),6),
              
              max_pos_Ay=round(max(Ay),6),
              min_pos_Ay=round(min(Ay[pos_Ay=="1"]),6),
              min_neg_Ay=round(max(Ay[pos_Ay=="0"]),6),
              max_neg_Ay=round(min(Ay),6),
              
              max_pos_Az=round(max(Az),6),
              min_pos_Az=round(min(Az[pos_Az=="1"]),6),
              min_neg_Az=round(max(Az[pos_Az=="0"]),6),
              max_neg_Az=round(min(Az),6))
  
  # Combine into one dataset
  if(i==1) clipVals303 <- temp else clipVals303 <- rbind(clipVals303,temp)
  # rm(temp)
  setwd(current_path)
  rm(current_path)
}
# Extract absolute min and max from each vector
sumClipVals303 <- clipVals303 %>% 
  summarize(max_pos_Ax=round(max(max_pos_Ax),6),
            min_pos_Ax=round(min(min_pos_Ax),6),
            min_neg_Ax=round(max(min_neg_Ax),6),
            max_neg_Ax=round(min(max_neg_Ax),6),
            
            max_pos_Ay=round(max(max_pos_Ay),6),
            min_pos_Ay=round(min(min_pos_Ay),6),
            min_neg_Ay=round(max(min_neg_Ay),6),
            max_neg_Ay=round(min(max_neg_Ay),6),
            
            max_pos_Az=round(max(max_pos_Az),6),
            min_pos_Az=round(min(min_pos_Az),6),
            min_neg_Az=round(max(min_neg_Az),6),
            max_neg_Az=round(min(max_neg_Az),6))

# Process LSM303 by clipping upper values of each vector using 9DS0 clipVals ----------------------
# Need to do this to match 9DS0, because 9DS0 clipped at 2G whereas the 303 recorded much higher.

# Reset working directory
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(current_path)
getwd()
# Reset depMeta
load("depMeta.RData")
depMeta <- depMeta %>% 
  dplyr::filter(platform=="TW",
                island=="SDI",
                model=="LSM303")

for(i in 1:length(depMeta$depid)) {
  # Save working directory so you can return to it at end of for loop
  current_path <- getwd()
  # Resets working directory to deployment folder being processed
  setwd(paste0(dataPath,"/",depMeta$depid[i],"/"))
  # This will tell you which file or folder is being processed (good for debugging)
  cat(paste0("\nProcessing: ",depMeta$depid[i])) 
  load(paste0(dataPath,"/",depMeta$depid[i],"/",depMeta$depid[i],"-50Hz-benchCal-24hr.RData"))
  depid <- data$depid[1]
  
  # Clip maximum (magnitude) values of each vector (i.e., outers)
  data$Ax[data$Ax>sumClipVals9DS0$max_pos_Ax[1]] <- sumClipVals9DS0$max_pos_Ax[1]
  data$Ax[data$Ax<sumClipVals9DS0$max_neg_Ax[1]] <- sumClipVals9DS0$max_neg_Ax[1]
  
  data$Ay[data$Ay>sumClipVals9DS0$max_pos_Ay[1]] <- sumClipVals9DS0$max_pos_Ay[1]
  data$Ay[data$Ay<sumClipVals9DS0$max_neg_Ay[1]] <- sumClipVals9DS0$max_neg_Ay[1]      
  
  data$Az[data$Az>sumClipVals9DS0$max_pos_Az[1]] <- sumClipVals9DS0$max_pos_Az[1]
  data$Az[data$Az<sumClipVals9DS0$max_neg_Az[1]] <- sumClipVals9DS0$max_neg_Az[1]
  
  # Round axes to match the sampling capacity (e.g. +/- 2G) of LSM9DS0
  data$Ax <- round(data$Ax,6)
  data$Ay <- round(data$Ay,6)
  data$Az <- round(data$Az,6)
  
  # Save
  save(data, file=paste0(depid,"-50Hz-benchCal-24hr-clipVal.RData"))
  setwd(current_path)
  rm(current_path)
}  

# Process LSM9DS0 deployments and resave with same file name structure to be able to process jointly later

# Reset working directory
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(current_path)
getwd()
# Reset depMeta
load("depMeta.RData")
depMeta <- depMeta %>% 
  dplyr::filter(platform=="TW",
                island=="SDI",
                model=="LSM9DS0")
for(i in 1:length(depMeta$depid)) {
  # Save working directory so you can return to it at end of for loop
  current_path <- getwd()
  # Resets working directory to deployment folder being processed
  setwd(paste0(dataPath,"/",depMeta$depid[i],"/"))
  # This will tell you which file or folder is being processed (good for debugging)
  cat(paste0("\nProcessing: ",depMeta$depid[i])) 
  load(paste0(dataPath,"/",depMeta$depid[i],"/",depMeta$depid[i],"-50Hz-benchCal-24hr.RData"))
  depid <- data$depid[1]
  # Identify positive and negative values 
  data <- data %>% 
    mutate(pos_Ax = ifelse(Ax>0,1,0),
           pos_Ay = ifelse(Ay>0,1,0),
           pos_Az = ifelse(Az>0,1,0))
  # Use LSM303 clipVals to clip "inner" values of each vector
  # Clip minimum (magnitude) values of each vector (i.e., "inners")
  data$Ax[data$Ax >0 & data$Ax < sumClipVals303$min_pos_Ax[1]] <- sumClipVals303$min_pos_Ax[1]
  data$Ax[data$Ax <0 & data$Ax > sumClipVals303$min_neg_Ax[1]] <- sumClipVals303$min_neg_Ax[1]
  
  data$Ay[data$Ay >0 & data$Ay < sumClipVals303$min_pos_Ay[1]] <- sumClipVals303$min_pos_Ay[1]
  data$Ay[data$Ay <0 & data$Ay > sumClipVals303$min_neg_Ay[1]] <- sumClipVals303$min_neg_Ay[1]
  
  data$Az[data$Az >0 & data$Az < sumClipVals303$min_pos_Az[1]] <- sumClipVals303$min_pos_Az[1]
  data$Az[data$Az <0 & data$Az > sumClipVals303$min_neg_Az[1]] <- sumClipVals303$min_neg_Az[1]
  
  # Round axes to match the sampling precision of LSM303
  data$Ax <- round(data$Ax,6)
  data$Ay <- round(data$Ay,6)
  data$Az <- round(data$Az,6)
  
  # Save as .RData
  save(data, file=paste0(depid,"-50Hz-benchCal-24hr-clipVal.RData"))
  setwd(current_path)
  rm(current_path)
}

# Save clipVals as .RData
save(sumClipVals9DS0, file="sumClipVals9DS0.RData")
save(sumClipVals303, file="sumClipVals303.RData")

