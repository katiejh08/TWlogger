library(dplyr)
library(tidyverse)
library(maptools)
library(ggplot2)
library(scales)
library(ggpubr)

# Test of Roland data
setwd("~/Projects/R/TWlogger")
tzOffset <-"Etc/GMT+3"

#############################################################
##  Combine all tag final metrics files with state classifications
##                 save to R.data ##
##     (NOTE: Only need to do this once)                   
#############################################################

# Import all tag data with metrics 
# Select the first CSV of the group you want to combine
filename <- file.choose("Select the first CSV of the group")
pathChoice <- dirname(filename)

# This imports all CSV files in the directory chosen
filenames <- list.files(path = pathChoice, pattern = "*.csv", all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = TRUE)
library(plyr)
import.list <- llply(paste(pathChoice,"/",filenames,sep = ""), read_csv)
data <-bind_rows(import.list)
# rm(import.list)
str(data)
data$ID <-as.factor(data$ID)

unique(data$ID)
attr(data$dttz, "tzone") # check tz
attr(data$dttz, "tzone") <- tzOffset # change the timezone to tzOffset

# Load Roland's data
load("~/Projects/R/TWlogger/data_for_Katie.RData")

# Combine list into one dataframe
dataRoland <-bind_rows(Amag_rollmean_states)
rm(Amag_rollmean_states)
attr(dataRoland$dttz_down, "tzone") # check tz
attr(dataRoland$dttz_down, "tzone") <- tzOffset # change the timezone to tzOffset
dataRoland$ID <- as.factor(dataRoland$ID)

# Join tag data and Roland's data
data <- left_join(data,dataRoland,by=c("ID","dttz"="dttz_down"))
class(data$state_classif)
data$state_classif <- as.factor(data$state_classif)

# Pitch and roll (NOTE: tagtools calculates in radians so must change to degrees)
data <- data %>%
  group_by(ID) %>% 
  mutate(pitch = (a2pr(cbind(Ax,Ay,Az),1)$p)*(180/pi),
         roll = (a2pr(cbind(Ax,Ay,Az),1)$r)*(180/pi)) %>% 
  ungroup # %>%  View

# Save workspace for later use
save(data, file = "FinalMetrics.RData")

#############################################################

# Load all tag data (includes Roland's state classifications)
load("~/Projects/R/TWlogger/FinalMetrics.RData")

class(data$ID)
# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(c(1,3,3,4,4,4,8))

# Roll offset correction
rollStats <- data %>% 
  group_by(ID) %>% #View
  dplyr::summarise(rollMean = mean(roll),
            rollMode = getmode(roll))


# Subset known activities from behavioral observations (NOTE: must change times and bird ID)
birdOI <- "V26"
activity <- "Foraging (wrack)-1"
startTime <- as.POSIXct(strptime("2017-02-15 10:01:10",format="%Y-%m-%d %H:%M:%S"),tz=tzOffset)
endTime <- as.POSIXct(strptime("2017-02-15 10:02:40",format="%Y-%m-%d %H:%M:%S"),tz=tzOffset)
data_birdOI <- subset(data, data$dttz >= startTime & data$dttz <= endTime & data$ID == birdOI)

# Interpreting pitch and roll
# A descending animal will have a negative pitch angle while an animal rolled with its right side up
# will have a positive roll angle.

# Plot subsets
# Need to label pitch and roll in legend
p1<- data_birdOI %>%
  ggplot() +
  theme(legend.position="top") +
  geom_line(aes(dttz, pitch), color = 'red') +
  geom_line(aes(dttz, roll), color = 'blue') +
  # scale_color_discrete(name = "Orientation", labels = c("Pitch", "Roll")) +
  geom_point(aes(dttz, pitch,shape = as.factor(state_classif), group = state_classif)) +
  scale_shape_discrete(name = "State") +
  scale_x_datetime(date_breaks = "20 secs", labels = date_format("%M:%S", tz = tzOffset)) +
  theme_minimal() +
  labs(x = "Time", y = "Pitch and Roll") +
  ggtitle(paste(birdOI,'-',activity))
p1


p2<- data_birdOI %>%
  gather(Axis, acc, Ax:Az) %>% #View
  ggplot() +
  theme(legend.position="top") +
  geom_line(aes(dttz, acc, color = Axis)) +
  geom_point(aes(dttz, acc,shape = as.factor(state_classif), group = state_classif)) +
  scale_shape_discrete(name = "State") +
  scale_x_datetime(date_breaks = "20 secs", labels = date_format("%M:%S", tz = tzOffset)) +
  theme_minimal() +
  labs(x = "Time", y = "Acceleration") +
  ggtitle(paste(birdOI,'-',activity))
p2

p3<- data_birdOI %>%
  gather(Axis, acc, Ax:Az) %>% #View
  ggplot() +
  theme(legend.position="top") +
  geom_line(aes(dttz, acc, color = Axis)) +
  scale_x_datetime(date_breaks = "20 secs", labels = date_format("%M:%S", tz = tzOffset)) +
  theme_minimal() +
  labs(x = "Time", y = "Acceleration") + 
  ggtitle(paste(birdOI,'-',activity))
p3

# Save plots
b<-ggarrange(p1,p2,p3,nrow=3)
ggsave(b, file=paste("Outputs/",  birdOI,'-',activity, ".png", sep=''), scale=2)


p <- testx %>%
  gather(axis, acc, Ax:Az) %>%
  ggplot(aes(dttz, y = state_classif),color = 'black') +
  geom_point() +
  theme_minimal() +
  labs(x = "Time", y = "HMM State Classification")
p
ggsave(p1, file=paste(depid,'-',"24hrAcc.png", sep=''))