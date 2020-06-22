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
# Select output folder
savePath <- choose.dir(caption="Select the folder containing output folder")

# Function for getting the mode of a vector
mode <- function(x) {
  unique_x <- unique(x)
  unique_x[which.max(tabulate(match(x, unique_x)))]
}

# Set working directory to location of meta data and load depMeta
setwd(metaPath)
load("depMeta.RData")
# Filter depMeta to include only the logger platform for which you're processing
depMeta <- depMeta %>% 
  dplyr::filter(platform=="TW",
                island=="SDI") %>% 
  arrange(depid)

# Set working directory to location of data and load
setwd(savePath)
load("TWgpsDataset-notRediscretized-WithExclusions.RData") # Load acc data
load("data_with_states_new.RData") # Load data with states


# Add states --------------------------------------------------------------
data_with_states <-bind_rows(data_with_states)
gpsStates <- left_join(data_with_states, dplyr::select(dataset,depid,dttz,long,lat), by = c("depid","dttz"))

# columns of interest are ID, dttz, state_classif, lat, long
gpsStates <- gpsStates %>% 
  group_by(depid) %>% 
  # This associates each acceleration value with the following GPS hit
  mutate(gps_hit = lag(cumsum(!is.na(lat)), default = 0))

# Save off gps data
GPS_data <- gpsStates %>%
  drop_na(lat) %>%
  dplyr::select(depid,gps_hit,dttz,lat,long)

gpsStates$state_classif<-as.factor(gpsStates$state_classif)
gpsStates2 <- gpsStates %>% 
  # For each bird's GPS hit...
  group_by(depid,gps_hit) %>% 
  # Find the state mode for the associated acceleration values
  summarize(stateMode = mode(state_classif),
            state1 = count(state_classif=="1")/n()) %>% View
  # Join GPS data 
  left_join(GPS_data, by = c("depid","gps_hit")) %>% 
  drop_na(dttz) 

GPS_states_one_modelSP <-   SpatialPointsDataFrame(cbind(GPS_states_one_model$x, GPS_states_one_model$y), GPS_states_one_model, match.ID = FALSE)
crds <-   UTM_to_longlat(GPS_states_one_modelSP,20,'south')
colnames(crds) <- c('long','lat')
GPS_states_one_model$lat <- crds$lat
GPS_states_one_model$long <- crds$long
GPS_states_one_model$state <-as.factor(GPS_states_one_model$state)

# Recalculate astronomical
