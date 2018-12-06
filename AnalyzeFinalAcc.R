
# Analysis Goals: start broad and hone in
# 1) Broadest classification resting or active
# Identify parameters to discern between resting and active behavior
# Use running mean and thresholds to ID resting behavior, so blips in movement don't mislead analysis
# e.g. I want to look at periods where ave movement is greater than X, otherwise it's resting behavior
# 2) Plot non resting behaviors, do some last longer or have different parameters 
# e.g. digging probably has a lot of repeated movement with stops in between, but you consider 
# that whole section to be digging. l;kjl


########################################
####         Import Acc Data       #####
########################################
library(tidyverse)
library(dplyr)
library(zoo)
library(tagtools)
library(lubridate)

setwd("~/Projects/R/TWlogger")

# Import calibrated rediscretized data
filename <- file.choose()

data <- read_csv(filename, 
                 col_types = cols(
                   #dttz = col_datetime(),
                   #dt = col_datetime(),
                   temp = col_double(),
                   Ax = col_double(),
                   Ay = col_double(),
                   Az = col_double(),
                   Mx = col_double(),
                   My = col_double(),
                   Mz = col_double(),
                   freq = col_double(),
                   secs_since = col_double()))
tzOffset <-"Etc/GMT+3"
str(data)
attr(data$dttz, "tzone") #Check tz
data$dttz <- force_tz(data$dttz,tzone=tzOffset)
attr(data$dttz, "tzone") #Check tz
head(data$dttz)

depid <- basename(filename)
depid <- strsplit(depid,'-')
depid <- depid[[1]][1]
depid
########################################
####          Plot Acc Data        #####
########################################
library(tidyverse)
library(plotly)
library(ggplot2)

# Subset data to only include Acc
data2 <- data[,c("dttz","Ax","Ay","Az")]

# Specify the start and end time of segment of interest (NOTE: change each time)
startTime <- as.POSIXct(strptime("2018-07-8 18:30:00",format="%Y-%m-%d %H:%M:%S"),tz=tzOffset)
endTime <- as.POSIXct(strptime("2018-07-9 18:30:00",format="%Y-%m-%d %H:%M:%S"),tz=tzOffset)
# Run subset() function to extract data for the selected timerange
practice <- subset(data2, data2$dttz >= startTime & data2$dttz <= endTime)

# Create static plot
p <- practice %>%
  gather(axis, acc, Ax:Az) %>%
  ggplot(aes(dttz, acc, color = axis)) +
  geom_line() +
  theme_classic() +
  labs(x = "Time", y = "Acceleration")
p

# Create interactive plot
ggplotly(p)

##########################################
####       Calculate Metrics         #####
##########################################
library(RcppRoll)

# These are the metrics to use in k-means clustering analysis

# The following could all potentially be put into a pipe.

# Calculate ODBA (Need to read about Wilson method, filter pass, and n)
A <- cbind(data2$Ax, data2$Ay, data2$Az)
o <- odba(A, sampling_rate = 50,method="wilson",n = 50) # n is sampling window, e.g. 50=1s
# Plot ODBA
#ba <- list(odba = o)
#plott(ba, 50) # NOTE: Change if different sampling rate

# Calculate pitch and roll 
pr <- a2pr(A,50)
#prlist <-list(pitch=pr$p,roll=pr$r)
#plott(prlist,fs=50)
p <- pr$p
r <-pr$r

# Calculate jerk
jerk <-njerk(A,sampling_rate=50)
#jerklist <-(jerk=jerk)
#plott(jerklist,fs=50)

# Calculate msa
msa <-msa(A)
#msalist <- list(msa = msa)
#plott(msalist,fs=50)

# Calculate variance of heave
varheave <-roll_var(data2$Az, n=50, fill=NA) #n is window size, e.g. 1s=50
# fill=NA replaced missing values created by moving window (e.g.first 24 and last 25 rows were empty) 
# Replace top NA values with the 1st calculated value and last NAs with last caluculated value. 
foo <-varheave[25]
foo              
varheave[1:24] <-foo 
varheave[1:25]
foo <-varheave[4320025]
foo
varheave[4320026:4320050] <-foo
varheave[4320026:4320050]
rm(foo)
# varlist <- list(varheave = varheave)
# plott(varlist,fs=50)

# Calculate variance of surge (replace NAs with adjacent first and last values)
varsurge <- roll_var(data2$Ax, n=50, fill=NA)
foo <-varsurge[25]
foo              
varsurge[1:24] <-foo 
varsurge[1:25]
foo <-varsurge[4320025]
foo
varsurge[4320026:4320050] <-foo
varsurge[4320026:4320050]
rm(foo)

# Calculate variance of sway (replace NAs with adjacent first and last values)
varsway <- roll_var(data2$Ay, n=50, fill=NA)
foo <-varsway[25]
foo              
varsway[1:24] <-foo 
varsway[1:25]
foo <-varsway[4320025]
foo
varsway[4320026:4320050] <-foo
varsway[4320026:4320050]
rm(foo)

# Might want to calculate variance of surge and sway as well to look for patterns

# Calculate norm
normAcc <- norm2(A)
# normlist <- list(normAcc = normAcc)
# plott(normlist,fs=50)

# Plot multiple metrics
# overallList <- list(jerk = jerk, odba = o,msa = msa)
# plott(overallList,fs=50)
# Combine metrics into new df
dttz <-data2$dttz
data3 <-cbind.data.frame(dttz,o,jerk,varheave,varsurge,varsway,msa, p,r,normAcc)
# Check for NAs
sapply(data3, function(x) sum(is.na(x)))
str(data3)

# Subset metrics to practice using PCA, kmeans, and HMM analyses
startMetrics <- as.POSIXct(strptime("2018-07-9 7:00:00",format="%Y-%m-%d %H:%M:%S"),tz=tzOffset)
endMetrics <- as.POSIXct(strptime("2018-07-9 11:00:00",format="%Y-%m-%d %H:%M:%S"),tz=tzOffset)
# Run subset() function to extract data for the selected timerange
data4 <- subset(data3, data3$dttz >= startMetrics & data3$dttz <= endMetrics)

##########################################
####              PCA                #####
##########################################
# Run PCA to determine which metrics explain the most variance in the data
library(devtools)
library("ggbiplot")

data4.pca <- prcomp(data4[,2:10], center = TRUE,scale. = TRUE)
summary(data4.pca)
ggbiplot(data4.pca,ellipse=TRUE,obs.scale = 1,var.scale = 1) +
  ggtitle("PCA of tag metrics")+
  theme_minimal()

##########################################
####           k means               #####
##########################################
library(plotly)
library(tidyverse)
# Classify data
# kmeans assumes independence between individual observations, which is not true for acc data)
set.seed(20)
clusters <- kmeans(data4[,2:4], 2)
data4$states <- as.factor(clusters$cluster)
str(clusters)

# Plot clusters (NOTE: This is not the best way to plot)
k <- plot_ly(data4, x = ~o, y = ~jerk, z = ~varheave, color = ~states, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'ODBA'),
                      yaxis = list(title = 'Jerk'),
                      zaxis = list(title = 'Variance of Heave')))
k


##########################################
####              HMM                #####
##########################################
library(momentuHMM)
# HMM is better than kmeans because of the serial dependence of acc data
# 




################################################################################
################################################################################
# Notes:
# Create plotly interactive plot with acc, pr, and jerk
# Decimate pitch and roll to understand bird's posture at any given point
# Create loop that allows to click through data in 30 min increments (interactive plotly)
# Within each increment, highlight and click on sections, save it with an attribute
# Find peaks (tagtools fxn) in jerk and subset into coarse activity signatures
# Output of this is a table, a df that has indices of different things I've identified
# GOAL: series of indexes of activity start/end times, create plot, shade behaviors, calculate percentages



