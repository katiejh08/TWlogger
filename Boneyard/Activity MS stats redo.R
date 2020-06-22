
# Set up environment ------------------------------------------------------

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
pkgTest("zoo")
pkgTest("dplyr")
pkgTest("plotly")
pkgTest("RcppRoll")
pkgTest("car")
pkgTest("gridExtra")
pkgTest("signal")
pkgTest("ggpubr")
pkgTest("fossil")
pkgTest("argosfilter")
pkgTest("sp")
pkgTest("adehabitatLT")
pkgTest("move")
pkgTest("outliers")
pkgTest("adehabitatHR")

# Select parent folder that contains all deployment folders
dataPath <- choose.dir(caption="Select the folder containing deployment raw data folders") 
# Selet folder that contains meta data
metaPath <- choose.dir(caption="Select the folder containing meta data") 
# Select output folder
savePath <- choose.dir(caption="Select the folder containing output folder")

# Load depMeta
load(paste0(metaPath,"/depMeta.RData"))

# Helper functions specific to this analysis
`%notin%` <- Negate(`%in%`)
# Finds Sunrise and sunset times
source("../Global Functions/find_Astronomical.R")
# Finds Point to Point metrics
source("../Global Functions/pt2pt_fxns.R")
# Parameterized Exclusions
# Set the Timezone Offset from UTC 
tzOffset <- "Etc/GMT+3"

# Loads Google API key
register_google(key = read.csv("APIkey.csv",header=FALSE)[1,])
has_google_key()

# Proportion time spent resting---------------------------------------------

# By day and night
restProp <- stateSum2period2state %>% 
  dplyr::filter(state=="rest") %>% 
  summarize()
  group_by(season,dayNight,state) %>% 
  summarize(stateProp = mean(stateProp)) %>% View
restProp

# By day and night
restPropstate <- stateSum2period %>% 
  group_by(season,dayNight,state_classif) %>% 
  dplyr::filter(dayNight=="day") %>% 
  summarize(stateProp = mean(stateProp))
restPropstate

restDur <- stateSum2period2state %>% 
  dplyr::filter(state=="rest") %>%
  group_by(season,yr,depid) %>%
  summarize(dur=sum(stateCountHour)) %>%
  group_by(season) %>% 
  summarize(mean=mean(dur)) %>% View

state1Dur <- stateSum2period %>% 
  dplyr::filter(state_classif=="1") %>%
  group_by(season,yr,depid) %>%
  summarize(dur=sum(stateCountHour)) %>% 
  group_by(season) %>% 
  summarize(mean=mean(dur)) %>% View


stateSum2period %>% 
  dplyr::filter(dayNight=="day",
                state_classif=="4") %>% View
shapiro.test(restDur$dur)
var.test(dur ~ season, data = restDur)
t.test(dur ~ season, data = restDur)

mean(restDur$dur[restDur$season=="Summer"])
sd(restDur$dur[restDur$season=="Summer"])
mean(restDur$dur[restDur$season=="Winter"])
sd(restDur$dur[restDur$season=="Winter"])


# By time spent resting (24-h) --------------------------------------------
##### KJH: Copied from TWLogger Processing Add States

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

# Daytime states ----------------------------------------------------------

# State 1
state1day <- stateSum2period %>% 
  group_by(season,yr,depid,dayNight,state_classif,stateCountMin) %>%
  # filter(dayNight=="day",
  #        state_classif=="1") %>%
  filter(state_classif=="1") %>%
  ungroup

state1 %>%
  group_by(season,sex,depid) %>% 
  summarize(sum=sum(stateCountHour)) %>% 
  group_by(season,sex) %>% 
  summarize(mean=mean(sum)) %>% View

# State 2
state2day <- stateSum2period %>% 
  group_by(season,depid,dayNight,state_classif,stateCountMin) %>%
  filter(dayNight=="day",
         state_classif=="2") %>%
  ungroup

# State 3
state3day <- stateSum2period %>% 
  group_by(season,depid,dayNight,state_classif,stateCountHour) %>%
  filter(dayNight=="day",
         state_classif=="3") %>%
  mutate(sex=depMeta$sex[depMeta$depid==depid],
         model=depMeta$model[depMeta$depid==depid]) %>% 
  ungroup

# State 4
state4day <- stateSum2period %>% 
  group_by(season,depid,dayNight,state_classif,stateCountHour) %>%
  filter(dayNight=="day",
         state_classif=="4") %>%
  mutate(sex=depMeta$sex[depMeta$depid==depid],
         model=depMeta$model[depMeta$depid==depid]) %>% 
  ungroup

# State 4
state4dayProp <- stateSum2period %>% 
  group_by(season,depid,dayNight,state_classif,stateProp) %>%
  filter(dayNight=="day",
         state_classif=="4") %>%
  mutate(sex=depMeta$sex[depMeta$depid==depid],
         model=depMeta$model[depMeta$depid==depid]) %>% 
  ungroup


# This tests for seasonal differences in daytime state durations using randomized block ANOVA.
shapiro.test(state4day$stateCountMin)
var.test(stateCountMin ~ season, data = state4day)
d5 <- aov(stateCountHour ~ season+yr,data=state4day)
summary(d5)
d6 <- aov(stateCountHour ~ season+yr,data=state3day)
summary(d6)

# This tests for seasonal differences in daytime state proportions using randomized block ANOVA.
shapiro.test(state4dayProp$stateProp)
var.test(stateProp ~ season, data = state4dayProp)
m1 <- glm(stateProp ~ season, data = state4dayProp)
d5 <- aov(stateCountHour ~ season+yr,data=state4day)
summary(d5)
d6 <- aov(stateCountHour ~ season+yr,data=state3day)
summary(d6)


# State 4 stats
mean(state4day$stateCountHour[state4day$season=="Summer" & state4day$sex=="Male"])
mean(state4day$stateCountHour[state4day$season=="Summer" & state4day$sex=="Female"])

sd(state4day$stateCountHour[state4day$season=="Summer" & state4day$sex=="Male"])
sd(state4day$stateCountHour[state4day$season=="Summer" & state4day$sex=="Female"])

mean(state4day$stateCountHour[state4day$season=="Winter" & state4day$sex=="Male"])
mean(state4day$stateCountHour[state4day$season=="Winter" & state4day$sex=="Female"])

sd(state4day$stateCountHour[state4day$season=="Winter" & state4day$sex=="Male"])
sd(state4day$stateCountHour[state4day$season=="Winter" & state4day$sex=="Female"])

# State 3 stats
mean(state3day$stateCountHour[state3day$season=="Summer" & state3day$sex=="Male"])
mean(state3day$stateCountHour[state3day$season=="Summer" & state3day$sex=="Female"])

sd(state3day$stateCountHour[state3day$season=="Summer" & state3day$sex=="Male"])
sd(state3day$stateCountHour[state3day$season=="Summer" & state3day$sex=="Female"])

mean(state3day$stateCountHour[state3day$season=="Winter" & state3day$sex=="Male"])
mean(state3day$stateCountHour[state3day$season=="Winter" & state3day$sex=="Female"])

sd(state3day$stateCountHour[state3day$season=="Winter" & state3day$sex=="Male"])
sd(state3day$stateCountHour[state3day$season=="Winter" & state3day$sex=="Female"])

# GPS ---------------------------------------------------------------------


# Subset depMeta to only include TW deployments with GPS
depMeta_sub<-depMeta %>% dplyr::filter(yr%in%c(2018,2019),depid!="20180711_TW6_M62")

# load(paste0(savePath,"/TWgpsDataset-Rediscretized-WithExclusions.RData")) # Use this for distance traveled

# Calculate total daytime distance traveled

distSum <- dataset_redisc %>% 
  group_by(depid) %>%
  summarize(distSum = sum(dist.back,na.rm = TRUE)/1000)

distSum <- left_join(distSum,depMeta_sub[,c(3,4,8,9)], by = "depid") # Add season, sex, mass variables

# Test for seasonal difference in total distance traveled
shapiro.test(distSum$distSum) # Normal
var.test(distSum ~ season, data = distSum) # Equal variance
t.test(distSum ~ season, data = distSum, var.equal = TRUE) # Results: t = -2.8898, df = 10, p-value = 0.01611

# Analysis of Variance
m1 <- glm(distSum ~ season + sex, data = distSum)
summary(m1)

m2 <- glm(distSum ~ season:sex, data = distSum)
summary(m2)
anova(m1, m2, test = "F") # compare models
# No difference so keep m1

m3 <- glm(distSum ~ season, data = distSum)
anova(m1, m3, test = "F") #compare models
# Effect of sex (i.e., p<0.05) so keep m1

# Plot
se<- function(x) sd(x)/sqrt(length(x))
fun<-function(y){
  r <- c(mean(y) - 1.96*se(y), mean(y) - se(y), mean(y), mean(y) + se(y), mean(y) + 1.96*se(y))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}
ggplot() +
  stat_summary(data = distSum, aes(sex, distSum, fill = season),
               fun.data = fun, geom = "boxplot", position = position_dodge(width = 1)) +
  scale_fill_manual("Season", values = c("goldenrod2", "dodgerblue4")) +
  theme_classic()+
  theme(
    text = element_text(size = 16),
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.spacing = unit(2, "lines"),
    legend.position = "none") #+
# ylab(expression("Cumulative Daily Distance (km)")) +
# xlab("")

ggplot(distSum) +
  geom_point(aes(mass,distSum,color=sex))
cor(distSum$mass, distSum$distSum)
test <- lm(distSum~mass,data=distSum)
summary(test)

# Calculate total distance traveled SD
sd(distSum$distSum[distSum$season=="Summer"])
sd(distSum$distSum[distSum$season=="Winter"])

# Calculate max step length per season
distSU <-GPS_states_odba %>% 
  filter(season=="Summer")
distWI <-GPS_states %>% 
  filter(season=="Winter")
mean(distSU$dist,na.rm = TRUE)
mean(distWI$dist,na.rm = TRUE)
sd(distSU$dist,na.rm = TRUE)
sd(distWI$dist,na.rm = TRUE)


# KDE by season -----------------------------------------------------------



# Body condition ----------------------------------------------------------
depF <- depMeta %>% subset(sex=="Female")
depM <- depMeta %>% subset(sex=="Male")

shapiro.test(depF$mass)
shapiro.test(depM$mass)

# Test for equal variances
var.test(mass ~ season,data=depF) #Results: F = 1.2188, num df = 10, denom df = 12, p-value = 0.7349
var.test(mass ~ season,data=depM) #Results: F = 1.2188, num df = 10, denom df = 12, p-value = 0.7349

# Test for differences within each season by year to confirm pooling
t.test(mass ~ season, data=depF,var.equal = TRUE) #Results:
t.test(mass ~ season, data=depM,var.equal = TRUE) #Results: