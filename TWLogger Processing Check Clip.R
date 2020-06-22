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
# Select folder that contains meta data
metaPath <- choose.dir(caption="Select the folder containing meta data") 
# Select output folder
savePath <- choose.dir(caption="Select the folder containing output folder")

# Set working directory to location of meta data and load depMeta
setwd(metaPath)
load("depMeta.RData")
# Filter depMeta to include only the logger platform for which you're processing
depMeta <- depMeta %>% 
  dplyr::filter(platform=="TW",
                island=="SDI") %>% 
  arrange(depid)

# Load data
setwd(savePath)
load("dataset-1Hz-VDBA-ODBA-clipVal.RData")
# load("dataset-10Hz-VDBA-ODBA.RData")
setwd(current_path)

# Set the Working Directory to the location of this file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(current_path)
getwd()

# Load helper data, in this case clip values to control for tag versioning
load("sumClipVals303.RData")
load("sumClipVals9DS0.RData")

# Set the Timezone Offset from UTC 
tzOffset <- "Etc/GMT+3"

# Process data ------------------------------------------------------------

dataset <- dataset %>% 
  mutate(pos_Ax=ifelse(Ax>0,1,0),
         pos_Ay=ifelse(Ay>0,1,0),
         pos_Az=ifelse(Az>0,1,0))

# Check min and max limits per vector for each axis
max(dataset$Ax) # Outer pos
min(dataset$Ax[dataset$pos_Ax=="1"]) # Inner pos
max(dataset$Ax[dataset$pos_Ax=="0"]) # Inner neg
min(dataset$Ax) # Outer neg

max(dataset$Ay) # Outer pos
min(dataset$Ay[dataset$pos_Ax=="1"]) # Inner pos
max(dataset$Ay[dataset$pos_Ax=="0"]) # Inner neg
min(dataset$Ay) # Outer neg

max(dataset$Az) # Outer pos
min(dataset$Az[dataset$pos_Ax=="1"]) # Inner pos
max(dataset$Az[dataset$pos_Ax=="0"]) # Inner neg
min(dataset$Az) # Outer neg

# Check to make sure clipping worked
# Iteratively process deployments
for(i in 1:length(depMeta$depid)) {
  # Save working directory so you can return to it at end of for loop
  current_path <- getwd()
  # Resets working directory to deployment folder being processed
  setwd(paste0(dataPath,"/",depMeta$depid[i],"/"))
  # This will tell you which file or folder is being processed (good for debugging)
  cat(paste0("\nProcessing: ",depMeta$depid[i])) 
  load(paste0(dataPath,"/",depMeta$depid[i],"/",depMeta$depid[i],"-50Hz-benchCal-24hr-clipVal.RData"))
  depid <- data$depid[1]
  # Extract min and max values of each depid
  temp <- data %>% 
    mutate(pos_Ax = ifelse(Ax>0,1,0),
           pos_Ay = ifelse(Ay>0,1,0),
           pos_Az = ifelse(Az>0,1,0)) %>%
    summarize(depid=first(depid),
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
  if(i==1) clipValsCheck <- temp else clipValsCheck <- rbind(clipValsCheck,temp)
  # rm(temp)
  setwd(current_path)
  rm(current_path)
}

# Extract absolute min and max from each vector
sumClipValsCheck <- clipValsCheck %>% 
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


# Plot distribution of minimum values per axis
# Check inner positive min (Ax)
p <- dataset %>%
  dplyr::filter(pos_Ax=="1") %>% 
  ggplot() + 
  geom_histogram(aes(Ax),binwidth = .05)+
  geom_vline(xintercept = sumClipVals303$min_pos_Ax,color="red") +
  theme(legend.position = "none") +
  facet_wrap(~depid)
p

# Check inner positive min (Ay)
p1 <- dataset %>%
  dplyr::filter(pos_Ay=="1") %>% 
  ggplot() + 
  geom_histogram(aes(Ay),binwidth = .01)+
  geom_vline(xintercept = sumClipVals303$min_pos_Ay,color="red") +
  theme(legend.position = "none") +
  facet_wrap(~depid)
p1

# Check inner positive min (Az)
p2 <- dataset %>%
  dplyr::filter(pos_Az=="1") %>% 
  ggplot() + 
  geom_histogram(aes(Az),binwidth = .05)+
  geom_vline(xintercept = sumClipVals303$min_pos_Az,color="red") +
  theme(legend.position = "none") +
  facet_wrap(~depid)
p2

# Check inner negative max (Ax)
p <- dataset %>%
  dplyr::filter(pos_Ax=="0") %>% 
  ggplot() + 
  geom_histogram(aes(Ax),binwidth = .05)+
  geom_vline(xintercept = sumClipVals303$min_neg_Ax,color="red") +
  theme(legend.position = "none") +
  facet_wrap(~depid)
p

# Check inner negative max (Ay)
p1 <- dataset %>%
  dplyr::filter(pos_Ay=="0") %>% 
  ggplot() + 
  geom_histogram(aes(Ay),binwidth = .05)+
  geom_vline(xintercept = sumClipVals303$min_neg_Ay,color="red") +
  theme(legend.position = "none") +
  facet_wrap(~depid)
p1

# Check inner negative max (Az)
p2 <- dataset %>%
  dplyr::filter(pos_Az=="0") %>% 
  ggplot() + 
  geom_histogram(aes(Az),binwidth = .05)+
  geom_vline(xintercept = sumClipVals303$min_neg_Az,color="red") +
  theme(legend.position = "none") +
  facet_wrap(~depid)
p2

# Check outer negative min
p <- dataset %>%
  dplyr::filter(pos_Ay=="1") %>% 
  ggplot() + 
  geom_histogram(aes(Ay),binwidth = .05)+
  geom_vline(xintercept = sumClipVals303$min_pos_Ay,color="red") +
  theme(legend.position = "none") +
  facet_wrap(~depid)
p
