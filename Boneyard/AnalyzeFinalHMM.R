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

setwd("~/Projects/R/TWlogger")
tzOffset <-"Etc/GMT+3"

# Load R workspace
load("~/Projects/R/TWlogger/data_for_Katie.RData")

# Combine list into one dataframe
data <-bind_rows(data_with_states)

#############################
####  Pre Process Data   ####
#############################

## Create Spatial Points
data$long <- -60.09
data$lat <- -51.37

# Check dttz 
attr(data$dttz, "tzone") #Check tz
attr(data$dttz, "tzone") <- tzOffset

data$date <- as.Date(data$dttz, tz = tzOffset)

# Run the sunrise sunset script
source('SunriseSunsetTimes.r')

# Check for NAs
sapply(data, function(x) sum(is.na(x)))

# Determine Season (calculated from 2018 solstices and equinoxes)
data$yday <- yday(data$dttz)
data$season <- rep('NA')
data[which(data$yday > 79 & data$yday <= 173),]$season <- 'Fall'
data[which(data$yday > 173 & data$yday <= 265),]$season <- 'Winter'
data[which(data$yday > 265 & data$yday < 355),]$season <- 'Spring'
data[which(data$yday >= 355 | data$yday <= 79),]$season <- 'Summer'
data$season <- as.factor(data$season)

unique(data$season)

##########################################
####   Calculate State Proportions    ####
##########################################
unique(data$astronomical)

# Calculate state proportions per 24 hour cycle
stateSummaryIND24 <- data %>% 
  group_by(season, ID) %>%
  mutate(cycleLength = n()) %>% #View
  ungroup %>% 
  group_by(season, ID, state_classif,cycleLength) %>% 
  summarize(stateCountCycle = n()) %>% 
  mutate(statePropCycle = stateCountCycle/cycleLength) %>% 
  ungroup

stateSummaryIND24$season_state <- paste(stateSummaryIND24$season,
                                            stateSummaryIND24$state_classif,sep="_")

# Plot state proportions per 24 hour cycle
Pcycle <- stateSummaryIND24 %>% 
  group_by(season) %>% 
  ggplot() +
  geom_boxplot(aes(x = state_classif, y = statePropCycle, group = season_state, color = season)) +
  labs(x="State",
       y = "Proportion (%)",
       title = "State Proportions (24hr Cycle)") + 
  theme_classic(base_size = 13) + 
  theme(plot.title = element_text(size=14)) +
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title.x = element_text(color="black", size=14),
        axis.title.y = element_text(color="black", size=14))
Pcycle

# Calculate state proportions per individual per period
stateSummaryIND <- data %>% 
  group_by(season, ID, astronomical) %>% 
  mutate(periodLength = n()) %>% 
  ungroup %>% 
  group_by(season, ID, astronomical, state_classif,periodLength) %>% 
  summarize(stateCount = n()) %>% 
  mutate(stateProp = stateCount/periodLength) %>% 
  ungroup

# Change classes for plotting
stateSummaryIND$ID <- as.factor(stateSummaryIND$ID)
stateSummaryIND$astronomical <- as.factor(stateSummaryIND$astronomical)
str(stateSummaryIND) # Check classes
# Create vectors to retain groupings for plots
stateSummaryIND$season_per_state <- paste(stateSummaryIND$season,
                                          stateSummaryIND$astronomical,
                                          stateSummaryIND$state_classif,sep="_")
stateSummaryIND$season_period <- paste(stateSummaryIND$season,
                                          stateSummaryIND$astronomical,sep="_")

# Reduce to two two periods day and night (e.g. combine dawn, day, and dusk)
stateSummaryIND1 <- data %>% 
  group_by(season, ID) %>% 
  mutate(dayNight = ifelse(astronomical %in% c("dawn","day","dusk"),"day","night")) %>%
  ungroup %>% 
  group_by(season, ID, dayNight) %>% 
  mutate(periodLength2 = n()) %>% 
  ungroup %>% 
  group_by(season, ID, dayNight, state_classif,periodLength2) %>% 
  summarize(stateCount2 = n()) %>% 
  mutate(stateProp2 = stateCount2/periodLength2) %>% 
  ungroup
# Create vectors to retain groupings for plots
stateSummaryIND1$ID <- as.factor(stateSummaryIND1$ID)
stateSummaryIND1$dayNight <- as.factor(stateSummaryIND1$dayNight)
str(stateSummaryIND1)
stateSummaryIND1$season_per_state <- paste(stateSummaryIND1$season,
                                          stateSummaryIND1$dayNight,
                                          stateSummaryIND1$state_classif,sep="_")
stateSummaryIND1$season_period <- paste(stateSummaryIND1$season,
                                       stateSummaryIND1$dayNight,sep="_")
# Plot
stateSummaryIND %>% 
  group_by(season,astronomical) %>% 
  ggplot() +
    geom_boxplot(aes(x = season_per_state, y = stateProp, 
                   group = season_per_state, color = season_period)) +
    labs(x="State",
       y = "Proportion (%)",
       title = "State Proportions") + 
    theme_classic(base_size = 13) + 
    theme(plot.title = element_text(size=14)) +
    theme(axis.text = element_text(size = 14, color = "black"),
        axis.title.x = element_text(color="black", size=14),
        axis.title.y = element_text(color="black", size=14))

# Plot each period separately
Pdawn <- stateSummaryIND %>% 
  group_by(season,astronomical) %>% 
  filter(astronomical == "dawn") %>% #View
  ggplot() +
  geom_boxplot(aes(x = state_classif, y = stateProp, 
                   group = season_per_state, color = season)) +
  labs(x="State",
       y = "Proportion (%)",
       title = "State Proportions (Dawn)") + 
  theme_classic(base_size = 13) + 
  theme(plot.title = element_text(size=14)) +
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title.x = element_text(color="black", size=14),
        axis.title.y = element_text(color="black", size=14))
Pdawn
Pday <- stateSummaryIND %>% 
  group_by(season,astronomical) %>% 
  filter(astronomical == "day") %>% 
  ggplot() +
  geom_boxplot(aes(x = state_classif, y = stateProp, 
                   group = season_per_state, color = season)) +
  labs(x="State",
       y = "Proportion (%)",
       title = "State Proportions (Day)") + 
  theme_classic(base_size = 13) + 
  theme(plot.title = element_text(size=14)) +
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title.x = element_text(color="black", size=14),
        axis.title.y = element_text(color="black", size=14))
Pday
Pdusk <- stateSummaryIND %>% 
  group_by(season,astronomical) %>% 
  filter(astronomical == "dusk") %>% 
  ggplot() +
  geom_boxplot(aes(x = state_classif, y = stateProp, 
                   group = season_per_state, color = season)) +
  labs(x="State",
       y = "Proportion (%)",
       title = "State Proportions (Dusk)") + 
  theme_classic(base_size = 13) + 
  theme(plot.title = element_text(size=14)) +
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title.x = element_text(color="black", size=14),
        axis.title.y = element_text(color="black", size=14))
Pdusk
Pnight <- stateSummaryIND %>% 
  group_by(season,astronomical) %>% 
  filter(astronomical == "night") %>% 
  ggplot() +
  geom_boxplot(aes(x = state_classif, y = stateProp, 
                   group = season_per_state, color = season)) +
  labs(x="State",
       y = "Proportion (%)",
       title = "State Proportions (Night)") + 
  theme_classic(base_size = 13) + 
  theme(plot.title = element_text(size=14)) +
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title.x = element_text(color="black", size=14),
        axis.title.y = element_text(color="black", size=14))
Pnight
# Stack plots to compare full cycle
PallPeriods <- ggarrange(Pdawn,Pday,Pdusk,Pnight,nrow=2,ncol=2)
PallPeriods

# Save a file at 300 ppi
ggsave(PallPeriods, file="Seasonal State Proportions by Period.png",width=12, height=6, dpi=300)

# Create day and night plots
Pday2 <- stateSummaryIND1 %>% 
  group_by(season,dayNight) %>% 
  filter(dayNight == "day") %>% 
  ggplot() +
  geom_boxplot(aes(x = state_classif, y = stateProp2, 
                   group = season_per_state, color = season)) +
  labs(x="State",
       y = "Proportion (%)",
       title = "State Proportions (Dawn to Dusk)") + 
  theme_classic(base_size = 13) + 
  theme(plot.title = element_text(size=14)) +
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title.x = element_text(color="black", size=14),
        axis.title.y = element_text(color="black", size=14))
Pday2
Pnight2 <- stateSummaryIND1 %>% 
  group_by(season,dayNight) %>% 
  filter(dayNight == "night") %>% 
  ggplot() +
  geom_boxplot(aes(x = state_classif, y = stateProp2, 
                   group = season_per_state, color = season)) +
  labs(x="State",
       y = "Proportion (%)",
       title = "State Proportions (Night)") + 
  theme_classic(base_size = 13) + 
  theme(plot.title = element_text(size=14)) +
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title.x = element_text(color="black", size=14),
        axis.title.y = element_text(color="black", size=14))
Pnight2
P2Periods <- ggarrange(Pday2,Pnight2,nrow=2)
P2Periods
ggsave(P2Periods, file="Seasonal State Proportions by Two Periods.png",width=12, height=6, dpi=300)

# I forget what this following plot is used for:
# stateSummary <- stateSummaryIND %>% 
#   group_by(season, astronomical, state_classif) %>% 
#   summarize(groupMeanProp = mean(stateProp))
# 
# ggplot() +
#   geom_col(data = stateSummary,
#                aes(x = astronomical, y = groupMeanProp, group = state_classif)) +
#   labs(x="State",
#        y = "Proportion (%)",
#        title = "State Proportions") + 
#   theme_classic(base_size = 13) + 
#   theme(plot.title = element_text(size=14)) +
#   theme(axis.text = element_text(size = 14, color = "black"),
#         axis.title.x = element_text(color="black", size=14),
#         axis.title.y = element_text(color="black", size=14))

# Add ODBA to data
# dataODBA <- left_join(rediscretized_data,
#                       dplyr::select(dataset_sum, namePeriod, cycle, 
#                                     period, astronomical), 
#                       by = c("burst"="namePeriod"))