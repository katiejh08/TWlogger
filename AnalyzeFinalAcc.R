library(tidyverse)
library(dplyr)
library(zoo)
library(tagtools)
library(lubridate)
library(plotly)
library(ggplot2)
library(RcppRoll)
library(car)
library(gridExtra)

setwd("~/Projects/R/TWlogger")
tzOffset <-"Etc/GMT+3"

# Note on dataframes:
# data is final raw acc data
# data2 is raw acc data subset to 24hr period of interest
# down is down sampled acc data
# data3 is calculated metrics (expanded version)
# data4 is calculated metrics (abbreviated version)
# data5 is subset metrics

########################################
####         Import Acc Data       #####
########################################
filename <- file.choose()
# Import calibrated rediscretized final acceleration data
data <- read_csv(filename, 
                 col_types = cols(
                   #dttz = col_datetime(),
                   #dt = col_datetime(),
                   temp = col_double(), # Only comment this out for 2017 tags
                   Ax = col_double(),
                   Ay = col_double(),
                   Az = col_double(),
                   Mx = col_double(),
                   My = col_double(),
                   Mz = col_double(),
                   freq = col_double(), # Only comment this out for 2017 tags
                   secs_since = col_double()))

# Import metrics file
# data <- read_csv(filename, 
#                  col_types = cols(
#                    dt = col_datetime()))

# Import raw acc and metrics file
# filename <- file.choose()
# data <- read_csv(filename, 
#                  col_types = cols(
#                    dt = col_datetime(),
#                    Ax = col_double(),
#                    Ay = col_double(),
#                    Az = col_double(),
#                    Amag_rollmean = col_double()))

# Import down sampled data
# filename <- file.choose()
# data <- read_csv(filename,
#                  col_types = cols(
#                    dttz_down = col_datetime()))

########################################
####       Confirm Time Zone       #####
########################################
# Check dttz. Dttz has not been retaining time zone when writing to CSV 
attr(data$dttz, "tzone") #Check tz
attr(data$dttz, "tzone") <- "Etc/GMT+3"

# If necessary, solve myriad time zone issues specific to each season ----YEEHAW!

# For July 2018 tags
# attr(data$dttz, "tzone") #Check tz
# data$dttz <- force_tz(data$dttz,tzone=tzOffset)
# attr(data$dttz, "tzone") <- "Etc/GMT+3"
# attr(data$dttz, "tzone") #Check tz
# head(data$dttz)

# For July 2017 tags
# Must subtract one hour from dt and dttz (because processed in Combine and ApplyCal as if was UTC-8)
# attr(data$dttz, "tzone") <- "Etc/GMT+4"
# attr(data$dttz, "tzone") #Check tz
# data$dttz <- force_tz(data$dttz,tzone=tzOffset)
# attr(data$dttz, "tzone") #Check tz
# attr(data$dt, "tzone") <- "Etc/GMT+1"
# attr(data$dt, "tzone") #Check tz
# data$dt <- force_tz(data$dt,tzone="GMT")
# attr(data$dt, "tzone") #Check tz

# For February 2017 tags
# attr(data$dttz, "tzone") <- "Etc/GMT+3"
# attr(data$dttz, "tzone") #Check tz
# head(data$dttz)

############################################
####  Create Deployment ID or Bird ID  #####
############################################
## Create deployment ID
# For 2018 tags
depid <- basename(filename)
depid <- strsplit(depid,'-')
depid <- depid[[1]][1]
depid

# For 2017 tags (must force manually)
# depid <- "20170214_Tag6_P25"

## Create Bird ID after subsetting and downsampling
# depid2 <- strsplit(depid,'_')
# depid2[[1]][3]
# down$ID <- depid2[[1]][3]

####################################
####        Subset Data        #####
####################################
## Subset acc raw data to only include acc
data2 <- data[,c("dttz","true_since","Ax","Ay","Az")]
# rm(data)

## Create more narrow subsets (First calculate metrics below)
# startMetrics <- as.POSIXct(strptime("2018-07-9 06:00:00",format="%Y-%m-%d %H:%M:%S"),tz=tzOffset)
# endMetrics <- as.POSIXct(strptime("2018-07-09 09:00:00",format="%Y-%m-%d %H:%M:%S"),tz=tzOffset)
# Extract data for the selected timerange
# Note: data3 is expanded metrics and data4 is abbreviated metrics; must choose which one to subset
# data5 <- subset(data3, data2$dttz >= startMetrics & data2$Metrics <= endTime)


##########################################
####         Downsample Data         #####
##########################################
# decdc runs a low pass filter (essentially a running mean)
# meaning it knocks out noise and interpolates
# every 10th of a second we've come up with a decimated (averaged) value 
# which represents the window of values at that time

# Decimation factor of 5 downsamples to 10Hz
# Decimate each vector separately
df <- 5 # Set decimation factor df
fs <- 50 # Set original sampling rate
# For datetime select ever nth value
dttz <- data2$dttz
a <- dttz
dttz_down <- a[seq(1, length(a), df)]

true_since <- data2$true_since
a <- true_since
true_since_down <- a[seq(1, length(a), df)]


Ax <- data2$Ax
Ay <- data2$Ay
Az <- data2$Az
# Convert vectors to numeric matrix
Ax_mat <- matrix(Ax,ncol=1)
Ay_mat <- matrix(Ay,ncol=1)
Az_mat <- matrix(Az,ncol=1)
# First downsample with df =5
Ax_down <- decdc(Ax_mat,df)
Ay_down <- decdc(Ay_mat,df)
Az_down <- decdc(Az_mat,df)
# Next downsample with df =10
Ax_down2 <- decdc(Ax_down,df)
Ay_down2 <- decdc(Ay_down,df)
Az_down2 <- decdc(Az_down,df)
# Reset df to 50 to be able to calculate new fs
data2_down <- cbind.data.frame(dttz_down,true_since_down,Ax_down,Ay_down,Az_down)
down <-data2_down
# Very important to run this line to reset sampling rate to down sampled rate
fs = fs/df

# Historic attempt:
# Select every nth value from datetime
# down <- data.matrix(data2)
# down <- decdc(down,5)

##########################################
####       Calculate Metrics         #####
##########################################
fs # Confirm sampling rate matches decimated data
sw <- fs # Specify sampling window (if equivalent to fs, then 1s window)

## Magnitude of acceleration
Ax <-down$Ax
Ay <-down$Ay
Az <-down$Az
Ax2 <-Ax^2
Ay2 <-Ay^2
Az2 <-Az^2
Amag <- Ax2 + Ay2 + Az2
Amag <-sqrt(Amag)

## Running mean of magnitude of acceleration
Amag_rollmean <- roll_mean(Amag,fs,fill=NA)
Amag_rollmean[seq(1,24)] <- Amag_rollmean[25]
Amag_rollmean[seq(length(Amag_rollmean)-24,length(Amag_rollmean))] <- Amag_rollmean[length(Amag_rollmean)-25]

## ODBA (Need to read about Wilson method, filter pass, and n)
A <- cbind(down$Ax_down,down$Ay_down,down$Az_down)
odba <- odba(A, sampling_rate = fs,method="wilson",n = sw) # n is sampling window, e.g. 50=1s
# Plot ODBA
# ba <- list(odba = odba)
# plott(ba, fs=fs) # NOTE: Change if different sampling rate

# ## Pitch and roll (NOTE: calculates in radians)
# pr <- a2pr(A,fs)
# #prlist <-list(pitch=pr$p,roll=pr$r)
# #plott(prlist,fs=fs)
# p <- pr$p
# r <-pr$r
# 
# ## Jerk
# jerk <-njerk(A,sampling_rate=fs)
# # jerklist <-(jerk=jerk)
# # plott(jerklist,fs=fs)
# 
# ## MSA (minimum specific acceleration)
# msa <-msa(A)
# #msalist <- list(msa = msa)
# #plott(msalist,fs=fs)
# 
# ##  Norm
# normAcc <- norm2(A)
# # normlist <- list(normAcc = normAcc)
# # plott(normlist,fs=fs)
# 
# ## Running means and variances for each axis
# mheave <-roll_mean(data2$Az, n=sw, fill=NA) # fill=NA replaced missing values created by moving window (e.g.first 24 and last 25 rows were empty) 
# mheave[seq(1,24)] <- mheave[25]
# mheave[seq(length(mheave)-24,length(mheave))] <- mheave[length(mheave)-25]
# 
# varheave <-roll_var(data2$Az, n=sw, fill=NA) #n is window size
# varheave[seq(1,24)] <- varheave[25]
# varheave[seq(length(varheave)-24,length(varheave))] <- varheave[length(varheave)-25]
# 
# msurge <-roll_mean(data2$Ax, n=sw, fill=NA)
# msurge[seq(1,24)] <- msurge[25]
# msurge[seq(length(msurge)-24,length(msurge))] <- msurge[length(msurge)-25]
# 
# varsurge <- roll_var(data2$Ax, n=sw, fill=NA)
# varsurge[seq(1,24)] <- varsurge[25]
# varsurge[seq(length(varsurge)-24,length(varsurge))] <- varsurge[length(varsurge)-25]
# 
# msway <- roll_mean(data2$Ay, n=sw, fill=NA)
# msway[seq(1,24)] <- msway[25]
# msway[seq(length(msway)-24,length(msway))] <- msway[length(msway)-25]
# 
# varsway <- roll_var(data2$Ay, n=sw, fill=NA)
# varsway[seq(1,24)] <- varsway[25]
# varsway[seq(length(varsway)-24,length(varsway))] <- varsway[length(varsway)-25]

## Plot means and variances. First Select axis to plot.
# z <- varheave
# z <- varsurge
# z <- varsway
# varlist <- list(z = z)
# plott(varlist,fs=fs)

## Plot multiple metrics
# overallList <- list(jerk = jerk, odba = odba,msa = msa)
# plott(overallList,fs=fs)

##########################################
####   Combine metrics into new df   #####
##########################################
# Combine down sampled acc data and metrics calculated using down sampled data
down <- cbind.data.frame(dttz_down,true_since_down,Ax_down,Ay_down,Az_down,Amag_rollmean,odba)


# dttz <-down$dttz
# true_since <-down$true_since
# # Create expanded metrics file
# data3 <-cbind.data.frame(dttz,true_since,Amag_rollmean,odba,jerk,mheave,varheave,msurge,varsurge,msway,varsway,msa, p,r,normAcc)
# # Create abbreviated metrics file
# data4 <-cbind.data.frame(dttz,true_since,Amag_rollmean,odba,jerk,p,r)
# Check for NAs
# sapply(data4, function(x) sum(is.na(x)))
# str(data4)

##########################################
####          Create Plots           #####
##########################################
# # Choose which metric to plot
# x <- down_24hr$Amag_rollmean # may need to change dataframe from which it is drawing
# #x <- log(data4$odba)
# 
# ## Exploratory plots: histograms, density, ecdf, q-q
# hist(x, main="Histogram of Amag_rollmean")
# hist(x,freq=F)
# lines(density(x))
# plot(density(x), main="Density Estimate of Data") #KD Plot returns the density data
# plot(ecdf(x),main="Empirical cumulative distribution function")
# z.norm<-(x-mean(x))/sd(x) # Standardized data
# qqnorm(z.norm); qqline(z.norm) # Draw a QQplot with a 45-degree reference line
# 
# ## Create static plots
# # NOTE: use dttz when requiring time of day for x-axis AND use true_since when requiring extreme zoom
# 
# # Run the following two lines to create 3 stacked plots including P1, P2, and P3
# # par(mfrow=c(3,1))
# # require(gridExtra)
# 
# # Plot acc
# p1 <- down %>%
#   gather(axis, acc, Ax:Az) %>%
#   ggplot(aes(dttz, acc, color = axis)) +
#   theme(legend.position="top") +
#   geom_line() +
#   theme_classic() +
#   labs(x = "Time", y = "Acceleration")
# p1
# 
# # Plot Amag (or swap out other metric)
# p2 <- down_24hr %>%
#   ggplot(aes(dttz, Amag),color = 'black') +
#   geom_line() +
#   theme_classic() +
#   labs(x = "Time", y = "Acc mag")
# p2
# 
# # Plot Amag_rollmean (or swap out other metric)
# p3 <- down_24hr %>%
#   ggplot(aes(x = dttz_down, y = Amag_rollmean),color= 'black') +
#   geom_line() +
#   labs(x = "Time", y = "Running mean acc mag")
# p3
# 
# # Run this to create stacked plots 
# # grid.arrange(p1, p2, p3, nrow=3)
# 
# ## Create ODBA plot
# op <- down_24hr %>% # May need to change dataframe from which it's drawing
#   ggplot(aes(dttz_down, odba),color = 'black') +
#   geom_line() +
#   theme_classic() +
#   labs(x = "Time", y = "ODBA")
# op
# 
# ## Plot successive subsets of data and save to file
# chunkLength <- 30 # Set this each time: length (in minutes) of chunks to plot
# # i = 1 # Can set i to number of plots wanted and just run what's within for loop from test to p
# # test$dttz[i] # could also name using the hh:mm of segment instead of i though need to build this out
# 
# for(i in 1:ceiling(nrow(data2)/(chunkLength *60*50))) {
#   test <- data2[seq(((i-1)*chunkLength*50*60)+1,i*chunkLength*50*60),]
#   p <- test %>%
#     gather(axis, acc, Ax:Az) %>%
#     ggplot(aes(dttz, acc, color = axis)) +
#     geom_line() +
#     theme_classic() +
#     labs(x = "Time", y = "Acceleration") +
#     ggtitle(depid)
#   p
#   # save plots as .png
#   ggsave(p, file=paste(depid,'-',i, ".png", sep=''), scale=2)
# }
# 
# ## Create stacked plots of successive subsets of data
# chunkLength <- 10 # length in minutes of chunk
# i=1
# 
# for(i in 1:ceiling(nrow(data3)/(chunkLength *60*50))) {
#   test <- data3[seq(((i-1)*chunkLength*50*60)+1,i*chunkLength*50*60),]
#   par(mfrow=c(3,1))
#   p1 <- test %>%
#     gather(axis, acc, ax:az) %>%
#     ggplot(aes(true_since, acc, color = axis)) +
#     geom_line() +
#     theme_classic() +
#     labs(x = "Time", y = "Acceleration")
#   p2 <- test %>%
#     ggplot(aes(dttz, odba),color = 'black') +
#     geom_line() +
#     theme_classic() +
#     labs(x = "Time", y = "ODBA")
#   p3 <- test %>%
#     ggplot() +
#     geom_line(aes(x=dttz, y=r*(180/pi), color='blue')) +
#     geom_line(aes(x=dttz, y=p*(180/pi), color='red')) +
#     theme_classic() +
#     labs(x = "Time", y = "Pitch and Roll")
#   # save plots as .png
#   ggsave(p, file=paste(depid,'-',i, ".png", sep=''), scale=2)
# }

## Create interactive plot (unfinished)
# ggplotly(p)

##########################################
####              HMM                #####
##########################################
# library(momentuHMM)
# HMM is better than kmeans because of the serial dependence of acc data
# Code coming from Roland Langrock

##########################################
####         Write to CSV            #####
##########################################
# ## Convert dttz back to dt (for some reason dttz does not retain tz when re-imported; always imports as UTC)
# dt <-data2$dttz
# # As needed, modify columns to be included
# final <-cbind.data.frame(dt,true_since,Amag_rollmean,odba,jerk,mheave,varheave,msurge,varsurge,msway,varsway,msa, p,r,normAcc)
# # Create abbreviated metrics file
# final2 <-cbind.data.frame(dt,true_since,Amag_rollmean,odba,jerk,p,r)

# Change dataframe and file name addition as needed
write_csv(down_24hr, file.path(dirname(filename), paste(depid,"-",fs,"Hz.csv",sep="")))
# write_csv(final2, file.path(dirname(filename), paste(depid,"-acc.csv",sep="")))




##########################################
#####
#####        Boneyard Scripts         
#####
##########################################
# All following scripts may be used to at future point, but not currently.

##########################################
####        Fit Distribution         #####
##########################################
# Currently not using this
# library(fitdistrplus)
# library(logspline)

# Choose which metric to fit distribution
# x <- odba 
# 
# descdist(x, discrete = FALSE)
# #Fit a Weibull distribution and a normal distribution:
# fit.weibull <- fitdist(x, "weibull")
# fit.norm <- fitdist(x, "norm")
# #Inspect the fit for the normal:
# plot(fit.norm)
# #Inspect the Weibull fit:
# plot(fit.weibull)
# #Use @Aksakal's procedure explained here to simulate the KS-statistic under the null.
# n.sims <- 5e4
# stats <- replicate(n.sims, {      
#   r <- rweibull(n = length(x)
#                 , shape= fit.weibull$estimate["shape"]
#                 , scale = fit.weibull$estimate["scale"]
#   )
#   as.numeric(ks.test(r
#                      , "pweibull"
#                      , shape= fit.weibull$estimate["shape"]
#                      , scale = fit.weibull$estimate["scale"])$statistic
#   )      
# })
# #The ECDF of the simulated KS-statistics looks like follows.
# plot(ecdf(stats), las = 1, main = "KS-test statistic simulation (CDF)", col = "darkorange", lwd = 1.7)
# grid()
# #Calculate p using the simulated null distribution of the KS-statistics
# fit <- logspline(stats)
# 
# 1 - plogspline(ks.test(x
#                        , "pweibull"
#                        , shape= fit.weibull$estimate["shape"]
#                        , scale = fit.weibull$estimate["scale"])$statistic
#                , fit
# )