library(tidyverse)
library(dplyr)
library(maptools) # to calculate solarpos
library(stringr)
library(lubridate)
library(adehabitatLT)
#library(argosfilter)
library(geosphere)
library(ggpubr)
library(sp)
library(rgdal)
library(tibble)
library("rnaturalearth")
library("rnaturalearthdata")
library("sf")
library("ggspatial")
library(marmap)
library(mapdata)
library(metR)

source('UTMConversionfxns.R')

# Set the Working Directory
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(current_path)
getwd()

### ToDo

#### Processing Notes ####
# All Positions have been processed using a SDA filter, which removes positions with unrealistic speeds.
# Periods with any gap of 3 hours (including from the start or endpoint of the period) are excluded from analysis.
# To standardize the movmement data for analysis, periods with sufficient data
# are then rediscretized into a 15 minute sampling rate
# 

### Global Variables

# max time in minutes between GPS points to include a cycle
exclude_Time <- 180
# Rediscretization time (900 seconds is 15 minutes)
reDistT <- 900
# Travel speed (Km/h)  
# multiply the speed value in knots by 1.852 (2kts = 3.7 km)
travelSpeed <- 4.25
# Loewr boundary latitude of study area
areaLat <- 32
# Thresholds for distance analysis
refDistLo <- 2.5 # Low threshold, also used in plot
refDistHi <- 10  # High threshold also used in plot

#### Import Data ####

# Import Time Zone offsets for all deployments
tzOffsets <- read.csv(paste0(getwd(),"/00Data/FrontiersTZOffsets.csv") , sep=",",header=TRUE)

# NOTE: The dataprocessing steps require all the deployments to be the same format
# Column Names and Format:
#Lat	- Decimal Degrees
#Long - Decimal Degrees
#DateTimeUTC - 2017-09-26 17:56:00
filename <- file.choose("Select the first CSV of the group")
species <- substr(basename(filename),1,2) # extract the species
pathChoice <- dirname(filename)
# This imports all CSV files in the directory chosen
filenames <- list.files(path = pathChoice, pattern = "*.csv", all.files = FALSE, full.names = TRUE, recursive = FALSE, ignore.case = TRUE)
# Create an empty dataframe to hold all the data
dataset = data.frame()
# Iteratively import CSV files from each deployment, find sunrise and sunset times, and create columns for sorting and processing.
for(i in 1:length(filenames)) {
  data <- read.csv(filenames[i], sep=",",header=TRUE)
  # select only columns with name, device number, datetime, lat, long and alt
  data <- data[,c("DateTimeUTC","Lat","Long")]
  # change column names to more practical shorter names
  colnames(data)[1:3] <- c("dt","lat","long")
  # extract deployment name from filename
  name <- strsplit(filenames[i],"[.]")[[1]][1] # This removes the .CSV
  name <- str_replace_all(name,"\\\\","/") # this makes it work for both windows and mac
  name <- sapply(strsplit(name,"[/]"),tail,1) # Take just the filename
  name <- str_replace(name,"-SDAFilter","") # Split at the '-SDAFilter' for just the whale name
  data$name <- name
  # Join Time Zone offset
  tzOffset <- tzOffsets %>% filter(ID == name) %>% .$tzOffset %>% as.character()
  data$tzOffset <- tzOffset
  # Create DateTime (UTC)
  data$dt  <- as.POSIXct(strptime(data$dt,format="%Y-%m-%d %H:%M:%S"),tz="GMT")
  # Convert to local time
  data$dttz <- data$dt # set dttz to dt
  attr(data$dttz, "tzone") <- tzOffset # change the timezone to tzOffset
  # Create useful identifiers
  data$date <- as.Date(data$dttz, tz = tzOffset)
  data$yr <- year(data$dttz)
  data$mth <- month(data$dttz)
  data$hr <- hour(data$dttz)	
  data$yday <- yday(data$dttz)
  data$yday <- sprintf('%03d',data$yday)
  data$yday <- paste('D',data$yday,sep='')
  data$indday <- as.factor(paste(data$name, data$yday, sep ='_', collapse = NULL))

  # Calculate distance between consecutive points
  source('Point2Point-stats.R')
  # Determine Solar Position for each point
  source('SunriseSunsetTimes.R')
  
  # Create Cycle and Period for grouping
  astro_data <- data %>%
    ungroup %>%
    arrange(dttz) %>%
    #Tidy up some unused columns
    subset(.,select = -c(hr, mth, yr, dir)) %>%
    mutate(period =  cumsum(replace_na(lag(astronomical) != astronomical, 1)))

  z <- length(astro_data$name)
  for(j in 2:z){
    if( (astro_data$astronomical[j-1] == "day" & astro_data$astronomical[j] == "night") |
        (astro_data$astronomical[j-1] == "night" & astro_data$astronomical[j] == "day")){
      astro_data$period[j:length(astro_data$name)] = astro_data$period[j:length(astro_data$name)] + 1
    }
  }
  astro_data <- astro_data%>%
    ungroup %>%
    arrange(dttz) %>%
    mutate(cycle = floor((period-1) / 4)) %>% 
    ungroup
  #convert dur (time between consecutive points) to minutes
  astro_data$dur <- astro_data$dur*60 
  # Combine into Dataset
  if(i==1) dataset <- astro_data else dataset <- rbind(dataset,astro_data)
}

#remove duplicates (if any)
dataset <- dataset %>% ungroup
dataset <- dataset[!duplicated(dataset),]
# Add species
dataset$species <- species

#### Process Data ####

# Determine Data quality and Travel Exclusions
dataQC <- dataset %>%
  group_by(name, cycle, period, astronomical) %>%
  # Check for Gaps in the GPS Data
  # Calculate time between gps hits (min)
  # 4 special cases: Calculate time gap between First Location in a 
  # period and the corresponding period beginning time (i.e. Dawn)
  mutate(time_Diff = case_when((row_number() == 1 & period %% 4 == 1) ~ difftime(hms::as.hms(dttz), hms::as.hms(srise), units = "secs")/60,# Day  
                               (row_number() == 1 & period %% 4 == 2) ~ difftime(hms::as.hms(dttz), hms::as.hms(sset), units = "secs")/60, # Dusk
                               (row_number() == 1 & period %% 4 == 3) ~ difftime(hms::as.hms(dttz), hms::as.hms(dusk), units = "secs")/60, # Night
                               (row_number() == 1 & period %% 4 == 0) ~ difftime(hms::as.hms(dttz), hms::as.hms(dawn), units = "secs")/60, # Dawn
                               TRUE ~ (difftime(dt,lag(dt), units = "secs")/60 )), # Standard Case (difference between consecutive points)
         # Calculate time between last location and the corresponding period end time (i.e. Dawn). 
         end_Diff = case_when((row_number() == n() & period %% 4 == 1) ~ difftime(hms::as.hms(sset), hms::as.hms(dttz), units = "secs")/60, # Day
                              (row_number() == n() & period %% 4 == 2) ~ difftime(hms::as.hms(dusk), hms::as.hms(dttz), units = "secs")/60, # Dusk
                              (row_number() == n() & period %% 4 == 3) ~ difftime(hms::as.hms(dawn), hms::as.hms(dttz), units = "secs")/60, # Night
                              (row_number() == n() & period %% 4 == 0) ~ difftime(hms::as.hms(srise), hms::as.hms(dttz), units = "secs")/60,# Dawn
                              TRUE ~ 0),
         # Deal with negative values from subtraction order
         time_Diff = ifelse(time_Diff< 0, 1440 + time_Diff, time_Diff), 
         end_Diff = ifelse(end_Diff < 0, 1440 + end_Diff, end_Diff),
         # Save Start and end time of each period
         period_Start = case_when(period %% 4 == 1 ~ first(srise), # Day  
                                  period %% 4 == 2 ~ first(sset),  # Dusk
                                  period %% 4 == 3 ~ first(dusk),  # Night
                                  period %% 4 == 0 ~ first(dawn)), # Dawn) 
         period_End = case_when(period %% 4 == 1 ~ last(sset), # Day
                                period %% 4 == 2 ~ last(dusk), # Dusk
                                period %% 4 == 3 ~ last(dawn), # Night
                                period %% 4 == 0 ~ last(srise))) %>% # Dawn
  ungroup %>% # View()
  group_by(name, cycle) %>% 
  # Exclude any periods with big gaps in data AND any that have no corresponding day 
  mutate(hasDay = ifelse(any(astronomical == "day", na.rm = FALSE),1,0),
         exclude = ifelse(any(time_Diff > exclude_Time | hasDay < 1 | end_Diff > exclude_Time,na.rm=TRUE), 1, 0 ))  %>% 
  ungroup 

# add unique identifier to group by name and period in plots
dataQC$namePeriod <- paste(dataQC$name, dataQC$period, sep = "-")

# Determine Travelling Behavior
dist_all_periods <- dataQC %>%
  arrange(name, dt) %>% 
  group_by(name, cycle, period, astronomical) %>%
  summarize(firstLong = first(long),
            firstLat = first(lat),
            firstdt = first(dt),
            lastLong = last(long),
            lastLat = last(lat),
            lastdt = last(dt),
            withinDistMadeGood = distGeo(cbind(firstLong, firstLat), # dist in KM
                                          cbind(lastLong, lastLat))/1e3,
            withinSpeedMadeGood = withinDistMadeGood/as.double(difftime(lastdt, firstdt, units = "hours")),
            travelWithin = ifelse(withinSpeedMadeGood > travelSpeed, 1, 0)) %>%
  ungroup %>%
  group_by(name,astronomical) %>% 
  mutate(cycleDistMadeGood = distGeo(cbind(firstLong, firstLat), # dist in KM
                                        cbind(lead(firstLong), lead(firstLat)))/1e3,
         cycleSpeedMadeGood = cycleDistMadeGood/as.double(difftime(lead(firstdt), firstdt, units = "hours")),
         travelCycle = ifelse(cycleSpeedMadeGood > travelSpeed, 1, 0),
         inArea = ifelse(firstLat > areaLat & lastLat > areaLat, 1, 0 )) %>% 
  ungroup %>% 
  group_by(name, cycle) %>% 
  # cycle speed for last cycle is determined by speed calculated from first pos in each period
  # and the last pos in cycle (the calc period gets shorter with each successive period)
  mutate(cycleDistMadeGood = case_when(is.na(cycleDistMadeGood) ~ distGeo(cbind(firstLong, firstLat), # dist in KM
                                                                          cbind(last(lastLong), last(lastLat)))/1e3,
                                       TRUE ~ cycleDistMadeGood),
         cycleSpeedMadeGood = case_when(is.na(cycleSpeedMadeGood) ~ cycleDistMadeGood/as.double(difftime(last(lastdt), firstdt, units = "hours")),
                                       TRUE ~ cycleSpeedMadeGood),
         travelCycle = ifelse(cycleSpeedMadeGood > travelSpeed, 1, 0)) %>% 
  ungroup %>%
  group_by(name) %>% 
  mutate(overallDisplacement = distGeo(cbind(first(firstLong), first(firstLat)), # dist in KM
                                       cbind(last(lastLong), last(lastLat)))/1e3,
         overallSpeed = overallDisplacement/as.double(difftime(last(lastdt), first(firstdt), units = "hours"))) %>% 
  ungroup %>% 
  arrange(name)

# Summarize Time based exclusions
exclusionOutput <- dataQC %>% 
  group_by(name, cycle, period, astronomical, exclude, period_Start, period_End) %>%
  summarize(max_timeDiff = max(time_Diff, na.rm = TRUE),
            max_endDiff = max(end_Diff, na.rm = TRUE))
# Join Time and Travel Exclusions
exclusionOutput <- left_join(exclusionOutput, 
                             dplyr::select(dist_all_periods, name, cycle, period, astronomical, 
                                           withinDistMadeGood, withinSpeedMadeGood, 
                                           travelWithin, cycleDistMadeGood, cycleSpeedMadeGood, 
                                           travelCycle, inArea, overallDisplacement, overallSpeed), by = c("name", "cycle", "period", "astronomical"))
test <- exclusionOutput %>% filter(astronomical %in% c("day", "night")) %>% 
  group_by(astronomical)# %>% 
hist(test$withinSpeedMadeGood, breaks = seq(0,10,.25))

  hist(test$withinSpeedMadeGood[exclusionOutput$astronomical == 'day'], breaks = seq(0,10,.25))
  hist(test$withinSpeedMadeGood[exclusionOutput$astronomical == 'night'], breaks = seq(0,10,.25))
  hist(test$cycleSpeedMadeGood, breaks = seq(0,10,.25))
  
# Export a CSV with Exclusions and Reason for Excluding
write_csv(exclusionOutput, file.path(pathChoice,paste0(species,"-exclusionOutput.csv")))
unique(exclusionOutput$name)

#plot the distribution of time differences in hours between points
plot(dataQC$time_Diff)

#### Rediscretize Data ####
dataQC_SP <- longlat_to_UTM(dataQC$long,dataQC$lat)
dataQC_SP_All <-bind_cols(dataQC_SP,dataQC) # combine all data
dataQC_SP_All <- SpatialPointsDataFrame(cbind(dataQC_SP$x, dataQC_SP$y), dataQC_SP_All, match.ID = FALSE)
dataQC_SP_All <- dataQC_SP_All[!duplicated(dataQC_SP_All$dt),] # remove duplicate times
# make sure there are no duplicate times (JAF)
dataQC_SP_All$dt[duplicated(dataQC_SP_All$dt)] 
# Inputs for AdeHabitatLT Resicretization Function
xy <- coordinates(dataQC_SP_All)
id <- dataQC_SP_All$name
da <- dataQC_SP_All$dt
burstAll <- dataQC_SP_All$namePeriod 

# Create a Trajectory (used in Rediscretization function)
# burst allows each period to be rediscretized individually
litr <- as.ltraj(xy, da, id, burst = burstAll,  typeII = TRUE)
head(litr)
plot(litr)
## Rediscretize Trajectories
reLitr <- redisltraj(litr, reDistT, burst = burst, type = "time") # 900 seconds is 15 minutes
head(reLitr)
plot(reLitr)
# Convert to Dataframe
rediscretized_data <- ld(reLitr)

# Summary of exclusions
dataQC_sum <- dataQC %>% 
  subset(.,select = c(astronomical, cycle, period, namePeriod, exclude )) %>% 
  filter(astronomical %in% c('day', 'night')) %>% 
  distinct

class(rediscretized_data$burst)
class(dataQC_sum$namePeriod)
dataQC_sum$burst <- as.factor(dataQC_sum$namePeriod)
str(dataQC_sum$burst)
str(rediscretized_data$burst)

# Join Exclusion data to rediscretized data
redist_cent <- left_join(rediscretized_data, 
                         dplyr::select(dataQC_sum, namePeriod, burst, cycle, period, astronomical, exclude), by = c("burst"))
# Join Travel Exclusions
redist_cent <- left_join(redist_cent, 
                             dplyr::select(dist_all_periods, name, cycle, period, astronomical,
                                           travelCycle, inArea), by = c("id" = "name", "cycle", "period", "astronomical"))
#### Calculate Centroid & Distances ####

# Calculate the Daytime Centroid of each period using re-discretized data  
# NOTE: We are comparing consecutive Day - Night pairs 
# (Day always preceding Night)
cent_day_period_Redist <- redist_cent %>% 
  filter(astronomical == 'day') %>%
  group_by(id, cycle, period, astronomical) %>% 
  summarize(cent_yR = mean(y),
            cent_xR = mean(x)) %>%
  ungroup %>%
  arrange(id)
# Join the Daytime Centroids
redist_cent <- left_join(redist_cent, dplyr::select(cent_day_period_Redist, id, cycle, cent_xR, cent_yR), by = c("id", "cycle"))

# Calculate distances of each point from daytime centroid 
# (distances in KM)
redisr_cent_distances <- redist_cent %>%
  group_by(id, cycle, period, astronomical, exclude, inArea) %>%
  mutate(dist_to_day_cent = round(sqrt((x - cent_xR)^2 + (y - cent_yR)^2)/1e3,2))  %>%  # dist in KM) 
  ungroup() 

#### Plot Results ####

#Cumulative Distribution Plot
cumDist <- redisr_cent_distances %>% 
  filter(is.na(travelCycle) | travelCycle != 1, exclude != 1, inArea == 1) %>% #View()
  group_by(id, astronomical) %>% 
  mutate(totalPositions = n(),
         numPeriods = length(unique(period)),
         meanDisttoCent = mean(dist_to_day_cent,na.rm=TRUE),
         sdDisttoCent = sd(dist_to_day_cent,na.rm=TRUE)) %>%
  ungroup() %>% 
  group_by(id, astronomical, totalPositions, numPeriods, meanDisttoCent, sdDisttoCent, dist_to_day_cent) %>%
  arrange(desc(dist_to_day_cent)) %>% #View()
  dplyr::summarize(n = n()) %>%
  arrange(dist_to_day_cent) %>%
  mutate(percentBelow = cumsum(n)/totalPositions) %>%
  ungroup() %>%
  arrange(id, astronomical, desc(percentBelow))


# Summarize by individual (mean line reflects a mean by animal, not by deployment length)
meanLine <- cumDist %>%
            group_by(id,astronomical) %>% 
            mutate(percentFact= cut(percentBelow, breaks = seq(0, 1, by = .05), labels = FALSE),
                   percentMid = percentFact/20-.025) %>% # center of each bin
            ungroup() %>%
            group_by(id,astronomical, percentFact, percentMid) %>% 
            summarize(meanDist = mean(dist_to_day_cent, na.rm = TRUE)) %>% 
            ungroup() %>% #  View()
            arrange(astronomical, percentFact) %>% 
            group_by(astronomical, percentFact, percentMid) %>% # View()
            summarize(meanDistAll = mean(meanDist, na.rm = TRUE)) #%>% View()

meanlineD <- meanLine %>% filter(astronomical ==  "day")
meanlineN <- meanLine %>% filter(astronomical ==  "night")


# Threshold Lines for reference
refLines <- data.frame(x1 = refDistLo, x2 = refDistHi, yLo = 0, yHi = 100)
refLineSize <- 1.2 # thickness of lines

#animal <- "Bm140719-TDR5"
#redisr_cent_distances %>% filter(id ==  animal) %>% View()
# pDay <- cumDist %>% filter(astronomical ==  "day") %>% # & id ==  animal) %>%  #View()
pDay <-  ggplot() +
  geom_line(data = cumDist[cumDist$astronomical ==  "day",],
            aes(x=dist_to_day_cent, y = percentBelow*100, color = id )) + 
  geom_line(data = meanlineD, aes(x=meanDistAll, y = percentMid*100),color = "black", size = 2) + 
  geom_segment(aes(x = refDistLo, y = yLo, xend = refDistLo, yend = yHi), data = refLines, colour = "red", size = refLineSize) +
  geom_segment(aes(x = refDistHi, y = yLo, xend = refDistHi, yend = yHi), data = refLines, colour = "red", size = refLineSize, linetype = 2) +
  scale_y_continuous(breaks=seq(0,100,10)) +
  #scale_x_continuous(breaks=seq(0,50,5)) +
  xlim(0,50) +
  labs(x="Distance from Daytime Centroid (Km)",
       y = "Cumulative Distribution (%)",
       title = "Day") + 
  theme_classic(base_size = 13) + 
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title.x = element_text(color="black", size=18),
        axis.title.y = element_text(color="black", size=18))#,
        #legend.position="none")
pDay

#pNight <- cumDist %>% filter(astronomical ==  "night") %>% #  & id ==  animal) %>% # View()
pNight <-  ggplot() +
  geom_line(data = cumDist[cumDist$astronomical ==  "night",],
            aes(x=dist_to_day_cent, y = percentBelow*100, color = id), 
            show.legend = TRUE) + 
  geom_line(data = meanlineN, 
            aes(x=meanDistAll, y = percentMid*100),
            color = "black", size = 2, show.legend = FALSE) + 
  geom_segment(data = refLines, 
               aes(x = refDistLo, y = yLo, xend = refDistLo, yend = yHi), 
               colour = "red", size = refLineSize, show.legend = FALSE) +
  geom_segment(data = refLines,
               aes(x = refDistHi, y = yLo, xend = refDistHi, yend = yHi), 
               colour = "red", size = refLineSize, linetype = 2, show.legend = FALSE) +
  scale_y_continuous(breaks=seq(0,100,10)) +
  #scale_x_continuous(breaks=seq(0,50,5)) +
  xlim(0,50) +
  labs(x="Distance from Daytime Centroid (Km)",
       y = "Cumulative Distribution (%)",
       title = "Night") + 
  theme_classic(base_size = 13) + 
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title.x = element_text(color="black", size=18),
        axis.title.y = element_text(color="black", size=18))#,
        #legend.position="none")
pNight
ggarrange(pDay, pNight, ncol = 2, legend = 'bottom', common.legend = TRUE)
# This will save a file at 300 ppi
ggsave(sprintf("%s_CumDist.png", species ), width=8, height=6, dpi=300)


#### Export Results ####

outputResults <- cumDist %>% 
  group_by(id, astronomical, totalPositions, meanDisttoCent, sdDisttoCent) %>% 
  summarize(threshLoPercent = ifelse(is.infinite(min(percentBelow[dist_to_day_cent >= refDistLo])),
                                                 1,min(percentBelow[dist_to_day_cent >= refDistLo])),
            threshHiPercent = ifelse(is.infinite(min(percentBelow[dist_to_day_cent >= refDistHi])),
                                                 1, min(percentBelow[dist_to_day_cent >= refDistHi])))
meanLineResults <- meanLine %>% 
  group_by(id = "Overall Mean",astronomical) %>% 
  summarize(threshLoPercent = ifelse(is.infinite(min(percentMid[meanDistAll >= refDistLo])),
                                     1,min(percentMid[meanDistAll >= refDistLo])),
            threshHiPercent = ifelse(is.infinite(min(percentMid[meanDistAll >= refDistHi])),
                                     1, min(percentMid[meanDistAll >= refDistHi])))
outputResultsCombined <- rbind(outputResults, meanLineResults) 

# Export a CSV with Exclusions and Reason for Excluding
write_csv(outputResultsCombined, file.path(pathChoice,paste0(species,"-resultsOutput.csv")))

#### Plot Deployment Map ####

world <- ne_countries(scale = "large", returnclass = "sf")
b = getNOAA.bathy(lon1 = min(dataset$long), lon2 = max(dataset$long), lat1 = max(dataset$lat), lat2 = min(dataset$lat),resolution = 1)
# convert bathymetry to data frame
bf = fortify.bathy(b)
datasetNoTravel <- left_join(dataQC,   dplyr::select(exclusionOutput, name, cycle, period, astronomical, exclude,
                                                      travelCycle, inArea), by = c("name", "cycle", "period", "astronomical", "exclude"))
datasetNoTravel <- subset(datasetNoTravel, travelCycle < 1 & inArea == 1)
p<-ggplot() +
  geom_sf(data = world) +
  coord_sf(xlim = c(min(datasetNoTravel$long), max(datasetNoTravel$long)), ylim = c(min(datasetNoTravel$lat), max(datasetNoTravel$lat)), expand = FALSE) +
  # add 100m and 200m contours
  geom_contour(data = bf, 
               aes(x=x, y=y, z=z),
               breaks=c(-200),
               size=c(0.4),
               colour="darkgrey", show.legend = FALSE) +
  geom_text_contour(data = bf, aes(x=x, y=y,z = z),breaks=c(-200), 
                    show.legend = FALSE, size = 2.2, alpha = .6, nudge_y = -.002) +
  geom_point(data = datasetNoTravel, mapping = aes(long,lat, color = name),size=1) +
  theme_classic() +
  theme(legend.position = "bottom") + 
  # legend.title = element_blank())  
  annotation_scale(location = "bl", width_hint = 0.5) + 
  annotation_north_arrow(location = "bl", 
                         which_north = "true", 
                         pad_x = unit(0.75, "in"), 
                         pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering) + 
  xlab("Longitude") + 
  ylab("Latitude") + 
  # ggtitle(sprintf("%s (local)", timePLocal[i])) + 
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "aliceblue"))
p
# This will save a file at 300 ppi
ggsave(sprintf("%s_DeploymentMap.png", species ), width=8, height=6, dpi=300)
# plot(rediscretized_data$x[rediscretized_data$x>200000],rediscretized_data$y[rediscretized_data$x>200000])

