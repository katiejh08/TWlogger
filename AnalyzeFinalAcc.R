########################################
####         Import Acc Data       #####
########################################
library(tidyverse)
library(dplyr)
library(zoo)
library(tagtools)
library(lubridate)

setwd("~/Projects/R/TWlogger")
tzOffset <-"Etc/GMT+3"
# Import calibrated rediscretized data
filename <- file.choose()
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

########################################
####       Confirm Time Zone       #####
########################################
# Check dttz. Dttz has not been retaining time zone when writing to CSV. 
attr(data$dttz, "tzone") #Check tz
attr(data$dttz, "tzone") <- "Etc/GMT+3"

# If necessary, solve myriad time zone issues specific to each season ----YEEHAW!

# For July 2018 tags
tzOffset <-"Etc/GMT+3"
str(data)
attr(data$dttz, "tzone") #Check tz
data$dttz <- force_tz(data$dttz,tzone=tzOffset)
attr(data$dttz, "tzone") <- "Etc/GMT+3"
attr(data$dttz, "tzone") #Check tz
head(data$dttz)

# For July 2017 tags
# Must subtract one hour from dt and dttz (because processed in Combine and ApplyCal as if was UTC-8)
attr(data$dttz, "tzone") <- "Etc/GMT+4"
attr(data$dttz, "tzone") #Check tz
data$dttz <- force_tz(data$dttz,tzone=tzOffset)
attr(data$dttz, "tzone") #Check tz
attr(data$dt, "tzone") <- "Etc/GMT+1"
attr(data$dt, "tzone") #Check tz
data$dt <- force_tz(data$dt,tzone="GMT")
attr(data$dt, "tzone") #Check tz

# For February 2017 tags
attr(data$dttz, "tzone") <- "Etc/GMT+3"
attr(data$dttz, "tzone") #Check tz
head(data$dttz)

########################################
####     Create Deployment ID      #####
########################################
# For 2018 tags
depid <- basename(filename)
depid <- strsplit(depid,'-')
depid <- depid[[1]][1]
depid

# For 2017 tags (if necessary)
depid <- "20170214_Tag6_P25"

# If need to override pre existing final files to now have correct tz in all final files
write_csv(data, file.path(dirname(filename), paste(depid,"-Final.csv",sep="")))
########################################
####          Plot Acc Data        #####
########################################
library(tidyverse)
library(plotly)
library(ggplot2)

# Subset data to only include Acc
data2 <- data[,c("dttz","true_since","Ax","Ay","Az")]

# Specify the start and end time of segment of interest (NOTE: change for each deployment)
startTime <- as.POSIXct(strptime("2018-07-8 18:30:00",format="%Y-%m-%d %H:%M:%S"),tz=tzOffset)
endTime <- as.POSIXct(strptime("2018-07-09 18:30:00",format="%Y-%m-%d %H:%M:%S"),tz=tzOffset)
# Run subset() function to extract data for the selected timerange
data2 <- subset(data2, data2$dttz >= startTime & data2$dttz <= endTime)

##### Create static plot of complete data
# NOTE: use dttz when requiring time of day for x-axis AND use true_since when requiring extreme zoom
p <- data4 %>%
  gather(axis, acc, Ax:Az) %>%
  ggplot(aes(true_since, acc, color = axis)) +
  geom_line() +
  theme_classic() +
  labs(x = "Time", y = "Acceleration")
p

##### Plot successive subsets of data and save to file
chunkLength <- 30 # Set this each time: length (in minutes) of chunks to plot
# i = 1 # Can set i to number of plots wanted and just run what's within for loop from test to p
# test$dttz[i] # could also name using the hh:mm of segment instead of i

for(i in 1:ceiling(nrow(data2)/(chunkLength *60*50))) {
  test <- data2[seq(((i-1)*chunkLength*50*60)+1,i*chunkLength*50*60),]
  # ggplot() +
  #   geom_line(data = test, aes(x = dttz, y = Ax,color = 'AX')) +
  #   geom_line(data = test, aes(x = dttz, y = Ay,color = 'AY')) +
  #   geom_line(data = test, aes(x = dttz, y = Az,color = 'AZ')) +
  #   scale_colour_manual(name="Axis",
  #                       values=c(AX="red", AY="blue", AZ="green")) +
  #   ylab("Raw ACC") + 
  #   xlab("Time")
  p <- test %>%
    gather(axis, acc, Ax:Az) %>%
    ggplot(aes(dttz, acc, color = axis)) +
    geom_line() +
    theme_classic() +
    labs(x = "Time", y = "Acceleration") +
    ggtitle(depid)
  p
  # save plots as .png
  ggsave(p, file=paste(depid,'-',i, ".png", sep=''), scale=2)
}

##### Create interactive plot
# ggplotly(p)

##########################################
####       Calculate Metrics         #####
##########################################
library(RcppRoll)

# Calculate ODBA (Need to read about Wilson method, filter pass, and n)
A <- cbind(data2$Ax, data2$Ay, data2$Az)
odba <- odba(A, sampling_rate = 50,method="wilson",n = 50) # n is sampling window, e.g. 50=1s
# Plot ODBA
# ba <- list(odba = odba)
# plott(ba, 50) # NOTE: Change if different sampling rate

# Calculate pitch and roll (NOTE: will be in radians)
pr <- a2pr(A,50)
#prlist <-list(pitch=pr$p,roll=pr$r)
#plott(prlist,fs=50)
p <- pr$p
r <-pr$r

# Calculate jerk
jerk <-njerk(A,sampling_rate=50)
# jerklist <-(jerk=jerk)
# plott(jerklist,fs=50)

# Calculate msa
msa <-msa(A)
#msalist <- list(msa = msa)
#plott(msalist,fs=50)

# Calculate norm
normAcc <- norm2(A)
# normlist <- list(normAcc = normAcc)
# plott(normlist,fs=50)

# Calculate rolling means and variances
# Replace top NA values with the 1st calculated value and last NAs with last caluculated value. 
mheave <-roll_mean(data2$Az, n=50, fill=NA) # fill=NA replaced missing values created by moving window (e.g.first 24 and last 25 rows were empty) 
mheave[seq(1,24)] <- mheave[25]
mheave[seq(length(mheave)-24,length(mheave))] <- mheave[length(mheave)-25]

varheave <-roll_var(data2$Az, n=50, fill=NA) #n is window size, e.g. 1s=50
varheave[seq(1,24)] <- varheave[25]
varheave[seq(length(varheave)-24,length(varheave))] <- varheave[length(varheave)-25]

msurge <-roll_mean(data2$Ax, n=50, fill=NA)
msurge[seq(1,24)] <- msurge[25]
msurge[seq(length(msurge)-24,length(msurge))] <- msurge[length(msurge)-25]

varsurge <- roll_var(data2$Ax, n=50, fill=NA)
varsurge[seq(1,24)] <- varsurge[25]
varsurge[seq(length(varsurge)-24,length(varsurge))] <- varsurge[length(varsurge)-25]

msway <- roll_mean(data2$Ay, n=50, fill=NA)
msway[seq(1,24)] <- msway[25]
msway[seq(length(msway)-24,length(msway))] <- msway[length(msway)-25]

varsway <- roll_var(data2$Ay, n=50, fill=NA)
varsway[seq(1,24)] <- varsway[25]
varsway[seq(length(varsway)-24,length(varsway))] <- varsway[length(varsway)-25]

##### Plot means and variances. First Select axis to plot.
# z <- varheave
# z <- varsurge
# z <- varsway
# varlist <- list(z = z)
# plott(varlist,fs=50)

##### Plot multiple metrics
# overallList <- list(jerk = jerk, odba = odba,msa = msa)
# plott(overallList,fs=50)

#### Combine metrics into new df
dt <-data2$dttz
true_since <-data2$true_since
# Create expanded metrics file
data3 <-cbind.data.frame(dt,true_since,odba,jerk,mheave,varheave,msurge,varsurge,msway,varsway,msa, p,r,normAcc)
# Create abbreviated metrics file
data4 <-cbind.data.frame(dt,true_since,odba,jerk,p,r)
# Check for NAs
sapply(data4, function(x) sum(is.na(x)))
str(data4)
# Add bird ID
depid2 <- strsplit(depid,'_')
depid2[[1]][3]
data4$ID <- depid2[[1]][3]

write_csv(data4, file.path(dirname(filename), paste(depid,"-metrics.csv",sep="")))

##########################################
####       Import Metrics File       #####
##########################################
# NOTE: p (pitch) and r (roll) are in radians

filename2 <- file.choose()
test <- read_csv(filename2, 
                 col_types = cols(
                   dt = col_datetime()))
                   # true_since = col_datetime(),
                   # odba = col_double(),
                   # jerk = col_double(),
                   # mheave = col_double(),
                   # varheave = col_double(),
                   # msurge = col_double(),
                   # varsurge = col_double(),
                   # msway = col_double(),
                   # varsway = col_double(),
                   # msa = col_double(),
                   # p = col_double(),
                   # r = col_double(),
                   # normAcc = col_double()))

# Subset metrics
tzOffset <-"Etc/GMT+3"
startMetrics <- as.POSIXct(strptime("2018-07-08 8:55:00",format="%Y-%m-%d %H:%M:%S"),tz=tzOffset)
endMetrics <- as.POSIXct(strptime("2018-07-09 16:58:00",format="%Y-%m-%d %H:%M:%S"),tz=tzOffset)
# Run subset() function to extract data for the selected timerange
data3 <- subset(data, data$dttz >= startMetrics & data$dttz <= endMetrics)

##########################################
####      Explore the Metrics        #####
##########################################
library(car)
library(gridExtra)

# Choose which metric to explore
x <- data4$odba 
#x <- log(data4$odba)

# Plot histograms, density plots, ecdf
hist(x, main="Histogram of observed data")
hist(x,freq=F)
lines(density(x))
plot(density(x), main="Density Estimate of Data") #KD Plot returns the density data
plot(ecdf(x),main="Empirical cumulative distribution function")

# Standardized data
z.norm<-(x-mean(x))/sd(x)
# Draw a QQplot with a 45-degree reference line
qqnorm(z.norm); qqline(z.norm) 

# Stack multiple plots to view ODBA, jerk, pitch and roll together
chunkLength <- 10 # length in minutes of chunk
i=1

for(i in 1:ceiling(nrow(data3)/(chunkLength *60*50))) {
  test <- data3[seq(((i-1)*chunkLength*50*60)+1,i*chunkLength*50*60),]
  par(mfrow=c(3,1))
  p <- test %>%
    gather(axis, acc, ax:az) %>%
    ggplot(aes(true_since, acc, color = axis)) +
    geom_line() +
    theme_classic() +
    labs(x = "Time", y = "Acceleration")
  op <- test %>%
    ggplot(aes(dttz, odba),color = 'black') +
    geom_line() +
    theme_classic() +
    labs(x = "Time", y = "ODBA")
  prp <- test %>%
    ggplot() +
    geom_line(aes(x=dttz, y=r*(180/pi), color='blue')) +
    geom_line(aes(x=dttz, y=p*(180/pi), color='red')) +
    theme_classic() +
    labs(x = "Time", y = "Pitch and Roll")
  # save plots as .png
  ggsave(p, file=paste(depid,'-',i, ".png", sep=''), scale=2)
}

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

##########################################
####              HMM                #####
##########################################
library(momentuHMM)
# HMM is better than kmeans because of the serial dependence of acc data
# Code coming from Roland Langrock

##########################################
####              PCA                #####
##########################################
# Run PCA to determine which metrics explain the most variance in the data
# Not doing this, since ODBA and jerk are accepted (would only do because it's interesting)

# library(devtools)
# library("ggbiplot")
# 
# data4.pca <- prcomp(data4[,2:10], center = TRUE,scale. = TRUE)
# summary(data4.pca)
# ggbiplot(data4.pca,ellipse=TRUE,obs.scale = 1,var.scale = 1) +
#   ggtitle("PCA of tag metrics")+
#   theme_minimal()

##########################################
####           k means               #####
##########################################
# Haven't fully adapted the following, though I don't plan to use.

# library(plotly)
# library(tidyverse)
# # Classify data
# # kmeans assumes independence between individual observations, which is not true for acc data)
# set.seed(20)
# clusters <- kmeans(data4[,2:4], 2)
# data4$states <- as.factor(clusters$cluster)
# str(clusters)
# 
# # Plot clusters (NOTE: This is not the best way to plot)
# k <- plot_ly(data4, x = ~o, y = ~jerk, z = ~varheave, color = ~states, colors = c('#BF382A', '#0C4B8E')) %>%
#   add_markers() %>%
#   layout(scene = list(xaxis = list(title = 'ODBA'),
#                       yaxis = list(title = 'Jerk'),
#                       zaxis = list(title = 'Variance of Heave')))
# k

op <- data4 %>%
  ggplot(aes(dt, odba),color = 'black') +
  geom_line() +
  theme_classic() +
  labs(x = "Time", y = "ODBA")
op
