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
# Select folder that contains band data
bandPath <- choose.dir(caption="Select the folder containing meta data")

# Load bandMeta CSV
setwd(bandPath)
bandMeta <- read_csv(paste0(getwd(),"/bandMeta.csv"))
bandMeta$OrigMassg <- as.numeric(bandMeta$OrigMassg)
bandMeta$age <- factor(bandMeta$age, levels = c("AD", "SA", "JUV","HY","Nestling"))

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
load("dataset-10Hz-VDBA-ODBA-clipVal-bitFix.RData") # Load acc data
load("data_with_states_new.RData") # Load data with states

# Set the Working Directory to the location of this File
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(current_path)
getwd()

# Set the Timezone Offset from UTC 
tzOffset <- "Etc/GMT+3"

# Add States --------------------------------------------------------------

# Combine states list into one dataframe
data <-bind_rows(data_with_states)
rm(data_with_states)

# Check dttz 
attr(data$dttz, "tzone") #Check tz

# Add states dataframe to dataset
dataset <- left_join(dataset,dplyr::select(data,depid,dttz,VDBA,state_classif), by = c("depid","dttz","VDBA"))
rm(data)
# Reorder dataset
dataset <-dataset %>% dplyr::select(depid,dttz,Ax,Ay,Az,VDBA,ODBA,state_classif,everything())

# Subset 4-states by astronomical period -----------------------------------------

# KJH: Adapted to 10-Hz data
# Calculate proportion of time spent in each state by 4 periods (dawn, day, dusk, night)
stateSum4period <- dataset %>% 
  group_by(season,yr,depid,astronomical) %>% 
  mutate(periodLength = n()) %>% 
  ungroup %>% 
  group_by(season, depid, astronomical, state_classif,periodLength) %>%
  summarize(stateCount = n(), # Each bird's total time (10-Hz) in each state per period of day
            VDBAsum = sum(VDBA)) %>% # Sum of bird's VDBA (10-Hz) for each state per period of day
  mutate(stateCountMin = round(stateCount/10/60,2), # Amount of time in minutes a bird spends in each state per period of day
         stateCountHour = round(stateCount/10/60/60,2), # Amount of time in hours a bird spends in each state per period of day
         stateProp = round(stateCount/periodLength,2)) %>% # Proportion of time a bird spends in each state per period of day
  ungroup %>% 
  mutate(depid = as.factor(depid), # Make factors for plotting
         astronomical = as.factor(astronomical),
         season_per_state = paste0(season,"_",astronomical,"_",state_classif), # Create vectors to retain groupings for plots
         season_period = paste0(season,"_",astronomical))

# Double check that time (hrs) spent aross each state adds to deployment length
stateSum4period %>% 
  group_by(depid) %>% 
  summarize(tot=sum(stateCountHour)) %>% View

# Subset 4- and 2-states by day and night ------------------------------------------

# 4-state, 2 period
# Calculate proportion of time spent in each state by 2 periods (day and night)

stateSum2period <- dataset %>% 
  group_by(season,yr,depid,dayNight) %>%
  mutate(periodLength = n()) %>%
  ungroup %>% 
  group_by(season,yr,depid,dayNight,state_classif,periodLength) %>%
  summarize(stateCount = n(),
            VDBAsum = sum(VDBA)) %>%
  mutate(stateCountMin = round(stateCount/10/60,2),
         stateCountHour = round(stateCount/10/60/60,2),
         stateProp = round(stateCount/periodLength,4)) %>%
  ungroup %>% 
  mutate(depid = as.factor(depid),
         dayNight = as.factor(dayNight),
         season_per_state = paste0(season,"_",dayNight,"_",state_classif),
         season_period = paste0(season,"_",dayNight),
         state_classif = as.factor(state_classif))

# 2-state, 2 period
stateSum2period2state <- stateSum2period %>% 
  group_by(season,yr,depid,dayNight,state_classif) %>%
  mutate(state = ifelse(state_classif %in% c("1","2"),"rest","active")) %>% # Combines active and rest states
  ungroup %>% 
  group_by(season,yr,depid,dayNight,state,periodLength) %>% 
  summarize(stateCount2state = sum(stateCount),
            VDBAsum = sum(VDBAsum)) %>% 
  mutate(stateCountMin = round(stateCount2state/10/60,2),
         stateCountHour = round(stateCount2state/10/60/60,2),
         stateProp = round(stateCount2state/periodLength,4)) %>% # Calculate proportion of day and night spent in each state
  ungroup


# Create Sum for Activity Analysis ----------------------------------------

# First calculate 24 hr VDBA
sum <- dataset %>%
  group_by(model,yr,season,depid) %>%
  summarize(VDBA24h=sum(VDBA,na.rm=TRUE))
# Plot
# sum %>% 
#   ggplot() +
#   geom_boxplot(aes(season,VDBA24h,fill=sex)) +
#   labs(x=NULL,
#        y="24-h VDBA (gravitational g)") #+
# facet_wrap(~model)

# Next calculate daytime hourly VDBA rate
rateHr <- dataset %>% 
  group_by(model,yr,season,depid,dayNight) %>% 
  dplyr::filter(dayNight == "day") %>% #
  summarize(VDBAday = sum(VDBA,na.rm=TRUE),
            dayLengthHr = n()/10/60/60,
            VDBAdayRateHr = VDBAday/dayLengthHr) %>% 
  mutate(sex=depMeta$sex[depMeta$depid==depid])
rateHr$night<-NULL
# Plot
# rateHr %>% 
#   ggplot() +
#   geom_boxplot(aes(season,VDBAdayRateHr,fill=sex)) +
#   labs(x=NULL,
#        y="Hourly VDBA (gravitational g)") #+
# facet_wrap(~model)

# Add to sum dataframe
sum<-left_join(sum,dplyr::select(rateHr,depid,sex,VDBAday,dayLengthHr,VDBAdayRateHr),by=c("depid","model","season","yr"))
# Reorder
sum <- sum[,c("depid","yr","season","sex","model","VDBA24h","VDBAday","dayLengthHr","VDBAdayRateHr")]

# Then calculate state proportions
dayStates <- stateSum2period %>% 
  dplyr::filter(dayNight=="day") %>% 
  dplyr::select(depid,state_classif,periodLength,stateCount,VDBAsum,stateCountMin,stateCountHour,stateProp) %>%
  group_by(depid) %>% 
  spread(state_classif,stateProp) %>%
  setNames(c("depid","periodLength","stateCount","VDBAsum","stateCountMin","stateCountHour","state1","state2","state3","state4")) %>% 
  group_by(depid) %>% 
  summarize(state1=mean(state1,na.rm=TRUE),
            state2=mean(state2,na.rm=TRUE),
            state3=mean(state3,na.rm=TRUE),
            state4=mean(state4,na.rm=TRUE))

# Add to sum dataframe
sum <- left_join(sum,dplyr::select(dayStates,depid,state1,state2,state3,state4),by="depid")

# Add proportion of night spent resting
rest <- stateSum2period2state %>% 
  dplyr::filter(dayNight=="night",
                state=="rest") %>% 
  dplyr::select(depid,stateProp) %>% 
  setNames(c("depid","nightRestProp"))
sum <- left_join(sum,dplyr::select(rest,depid,nightRestProp),by="depid")
sum$Hz <- "10-Hz"
# Save dataframe for import into caracaraMS-stats.R
save(sum, file="caracaraMS-data.RData")
# caracaraMS-data.RData was created to use in caracaraMS-stats.R 

# Create mass dataframe ---------------------------------------------------

mass <- bandMeta %>% 
  dplyr::filter(mass %notin% c(NA,"ND"),
                sex %notin% c(NA,"ND"),
                age== "JUV",
                crop == 0,
                season %in% c("Summer","Winter"))

save(mass, file="caracaraMSmass-data.RData")

# The following were only used for exploratory reasons. They were not included in the analysis.
# Investigate Duration Rest States (1 & 2)--------------------------------------------------

# Can we exclude night from the analysis?
rest <- stateSum2period2state %>% 
  group_by(season,dayNight,state) %>%
  summarize(stateProp = mean(stateProp)) %>% View # Proportion of time spent resting per period by season

restIND <- stateSum2period2state %>% 
  dplyr::filter(state=="rest") %>%
  group_by(yr,season,depid) %>%
  summarize(stateTotHr = sum(stateCount2state)/10/60/60)# Duration of time spent resting by season

restIND <- left_join(restIND,dplyr::select(depMeta,depid,sex),by="depid")
  
# Test for seasonal difference in time spent resting
shapiro.test(restIND$stateTotHr) # Results: W = 0.96329, p-value = 0.508
var.test(stateTotHr ~ season,data=restIND) #Results: F = 0.73113, num df = 11, denom df = 11, p-value = 0.6124

m1<-glm(stateTotHr~sex+season+yr,data=restIND)
summary(m1)
m2 <- glm(stateTotHr~sex:season+yr,data=restIND)
summary(m2)
anova(m1, m2, test = "F") # compare models 
# No effect of interaction (i.e., p>0.05) so keep m1
m3 <- glm(stateTotHr~sex+season,data=restIND)
anova(m1, m3, test = "F") # compare models 
# No effect of yr (i.e., p<0.05) so keep m1
m4 <- glm(stateTotHr~sex,data=restIND)
anova(m3, m4, test = "F") # compare models 
# Effect of season (i.e., p<0.05) so keep m3
m4a <- glm(stateTotHr~season,data=restIND)
anova(m3, m4a, test = "F") # compare models 
# Effect of sex so keep m3
m5 <- glm(stateTotHr~1,data=restIND)
anova(m3, m5, test = "F") # compare models
summary(m3)
# The model with sex and season best explains the data. 


mean(restIND$stateTotHr[restIND$season=="Winter"& restIND$sex=="Male"])
sd(restIND$stateTotHr[restIND$season=="Winter"& restIND$sex=="Male"])

mean(restIND$stateTotHr[restIND$season=="Summer"& restIND$sex=="Male"])
sd(restIND$stateTotHr[restIND$season=="Summer"& restIND$sex=="Male"])

mean(restIND$stateTotHr[restIND$season=="Winter"& restIND$sex=="Female"])
sd(restIND$stateTotHr[restIND$season=="Winter"& restIND$sex=="Female"])

mean(restIND$stateTotHr[restIND$season=="Summer"& restIND$sex=="Female"])
sd(restIND$stateTotHr[restIND$season=="Summer"& restIND$sex=="Female"])

# Test for seasonal difference in time spent in state 1
restINDstate1 <- stateSum2period %>% 
  group_by(season,depid,state_classif) %>%
  dplyr::filter(state_classif=="1") %>% 
  summarize(stateTotHr = sum(stateCount)/10/60/60)

t.test(stateTotHr ~ season, data = restINDstate1) # summer: 10.50 +/- 1.22; hrs winter: 13.45 +/-  0.80 hrs (t = -7.0336, df = 18.997, p-value = 1.074e-06)
sd(restINDstate1$stateTotHr[restINDstate1$season=="Winter"])
sd(restINDstate1$stateTotHr[restINDstate1$season=="Summer"])


# Investigate Daytime State 4 ------------------------------------------------

# Subset: day, state 4
activeINDstate4 <- stateSum2period %>% 
  group_by(yr,season,depid,dayNight,state_classif) %>%
  dplyr::filter(state_classif=="4",
                dayNight=="day") %>% 
  summarize(stateTotHr = sum(stateCount)/10/60/60) %>% 
  mutate(sex = depMeta$sex[depMeta$depid==depid])
# Add mass
activeINDstate4 <- left_join(activeINDstate4,dplyr::select(depMeta,depid,mass), by = "depid")

ggplot(activeINDstate4) +
  geom_point(aes(mass,stateTotHr,color=sex))

m1 <- glm(stateTotHr ~ sex:yr, data = activeINDstate4)
summary(m1)
m2 <- glm(stateTotHr ~ sex+yr, data = activeINDstate4)
summary(m2)
anova(m2, m1, test = "F") # interaction not significant
# keep m2
m3 <- glm(stateTotHr ~ sex, data = activeINDstate4)
summary(m3)
anova(m2, m3, test = "F") # keep m2

# Investigate Night -------------------------------------------------------

# Duration of nighttime spent active or resting by season
p1 <- stateSum2period2state %>%
  dplyr::filter(dayNight == "night") %>%
  ggplot() +
  geom_boxplot(aes(x = state, y = stateCountHour,color = season)) +
  scale_color_discrete(name = "Season") +
  scale_y_continuous(breaks = seq(0, 24, by = 2)) +
  labs(x="State",
       y = "Duration (hr)",
       title = "Duration of nighttime spent per state by season") + 
  theme_classic(base_size = 13) + 
  theme(plot.title = element_text(size=14),
        axis.text = element_text(size = 14, color = "black"),
        axis.title.x = element_text(color="black", size=14),
        axis.title.y = element_text(color="black", size=14))

# Proportion of nighttime spent in each state
p2 <- stateSum2period2state %>% 
  group_by(season,dayNight) %>% 
  dplyr::filter(dayNight == "night") %>% 
  ggplot() +
  geom_boxplot(aes(x = state, y = stateProp, color = season)) +
  ylim(0,1) +
  labs(x="State",
       y = "Proportion (%)",
       title = "Proportion of nighttime spent in each state by season") + 
  theme_classic(base_size = 13) + 
  theme(plot.title = element_text(size=14)) +
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title.x = element_text(color="black", size=14),
        axis.title.y = element_text(color="black", size=14))
pNight <- ggarrange(p1,p2,nrow=2) 
