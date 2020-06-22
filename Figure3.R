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
pkgTest("rsq")

# Select parent folder that contains all deployment folders
dataPath <- choose.dir(caption="Select the folder containing data") 
# Selet folder that contains meta data
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

# Define global variables
males <- depMeta$depid[depMeta$sex=="Male"]
females <- depMeta$depid[depMeta$sex=="Female"]
lt <-c("solid","longdash")

# Set the Working Directory to the location of this File
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
# rm(current_path)
getwd()

# Set the Timezone Offset from UTC 
tzOffset <- "Etc/GMT+3"

# Load Data ------------------------------------------------------------
setwd(savePath)
# 
load("dataset-1Hz-VDBA-ODBA-clipVal-bitFix.RData")
# load("dataset-10Hz-VDBA-ODBA-clipVal-bitFix.RData")
setwd(current_path)

# Snake swallowed elephant

# Hourly sum VDBA (by season and sex) --------------------------

# Plot hourly VDBA by season. Creates snake swallowed elephant plot.
# Meanline summer
meanlineSU <- dataset %>%
  dplyr::filter(season == "Summer") %>%
  group_by(depid, timeBin) %>%
  summarize(VDBAsum = sum(VDBA,na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(timeBin) %>% 
  summarize(meanVDBA = mean(VDBAsum))
# Meanline winter
meanlineWI <- dataset %>%
  dplyr::filter(season == "Winter") %>% #View
  group_by(depid, timeBin) %>%
  summarize(VDBAsum = sum(VDBA,na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(timeBin) %>% 
  summarize(meanVDBA = mean(VDBAsum))

dataset <- dataset %>%
  mutate(dawnT = (hour(dataset$dawn) + minute(dataset$dawn)/60) - (hour(dataset$solarMidnight) + minute(dataset$solarMidnight)/60),
         duskT = hour(dataset$dusk) + minute(dataset$dusk)/60 - (hour(dataset$solarMidnight) + minute(dataset$solarMidnight)/60))
# Calculate time of mean dawn and dusk and create dataframes to use for shading bins in total daily ODBA plot
# shadeSU = data.frame(x1=c(0,mean(unique(depSum$duskT[month(depSum$srise) == "2"]))),
#                      x2=c(mean(unique(depSum$dawnT[month(depSum$srise) == "2"])),23),
#                      y1=c(0,0),
#                      y2=c(1600,1600))
# shadeWI = data.frame(x1=c(0,mean(unique(depSum$duskT[month(depSum$srise) == "7"]))),
#                      x2=c(mean(unique(depSum$dawnT[month(depSum$srise) == "7"])),23),
#                      y1=c(0,0),
#                      y2=c(1600,1600))
shadeSU = data.frame(x1=c(0,20),
                     x2=c(3,23),
                     y1=c(0,0),
                     y2=c(1600,1600))
shadeWI = data.frame(x1=c(0,17),
                     x2=c(6,23),
                     y1=c(0,0),
                     y2=c(1600,1600))

# Summer total VDBA per solar hour
pVDBAsum24SU <- dataset %>%
  dplyr::filter(season == "Summer") %>% #View
  group_by(depid, timeBin,night) %>%
  summarize(VDBAsum = sum(VDBA,na.rm = TRUE)) %>%
  mutate(sex = depMeta$sex[depMeta$depid==depid]) %>%
  ggplot() +
  geom_line(aes(x=timeBin, y=VDBAsum, group=depid,color=sex),alpha=0.7) +
  geom_line(data=meanlineSU,aes(x=timeBin, y=meanVDBA),color="black",size=1.5) +
  scale_y_continuous(breaks = seq(0,1600, by = 200), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0,23, by = 1),expand = c(0, 0)) +
  # scale_x_continuous(breaks = seq(0,23, by = 1),expand = c(0, 0)) +
  labs(x="Solar Hour",
       y = NULL) + 
  geom_rect(data=shadeSU, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color='grey', alpha=0.5) +
  # annotate("text", x = 0.75, y = 740, label = "B. Summer", color="white", hjust = 0) +
  theme_classic(base_size = 13) +
  theme(legend.position = "none")
pVDBAsum24SU

# Winter total ODBA per solar hour
pVDBAsum24WI <- dataset %>%
  dplyr::filter(season == "Winter",
                sex=="Male") %>% #View
  group_by(depid, timeBin) %>%
  summarize(VDBAsum = sum(VDBA,na.rm = TRUE)) %>% 
  mutate(sex = depMeta$sex[depMeta$depid==depid]) %>%
  ggplot() +
  geom_line(aes(x=timeBin, y=VDBAsum, group=depid,color=sex),alpha=0.7)+
  geom_line(data=meanlineWI,aes(x=timeBin, y=meanVDBA),color="black",size=1.5) +
  scale_y_continuous(breaks = seq(0,1600, by = 200), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0,23, by = 1),expand = c(0, 0)) +
  labs(x=NULL,
       y = NULL) + 
  geom_rect(data=shadeWI, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color='grey', alpha=0.5) +
  # annotate("text", x = 0.75, y = 740, label = "A. Winter", color="white",hjust = 0) +
  theme_classic(base_size = 13) +
  theme(legend.position = "none")
pVDBAsum24WI

Podba24Seas <- ggarrange(pVDBAsum24WI,pVDBAsum24SU,nrow=2)
Podba24Seas <- annotate_figure(Podba24Seas,left = text_grob("24-h VDBA (gravitational g)", color = "black", rot = 90))

Podba24Seas
rm(meanlineSU,meanlineWI,pVDBAsum24SU,pVDBAsum24WI)
# Save a file at 300 ppi
ggsave(Podba24Seas, file="Seasonal total ODBA per solar hour_shaded with label.png",width=12, height=8,dpi=300)

# Hourly sum VDBA (by season and sex) --------------------------

# Plot hourly VDBA (sum) by season and sex

# Meanline summer males
meanlineSUm <- dataset %>%
  dplyr::filter(season == "Summer",
                depid %in% males) %>%
  group_by(depid, timeBin) %>%
  summarize(VDBAsum = sum(VDBA,na.rm = TRUE)) %>%
  ungroup() %>% 
  group_by(timeBin) %>% 
  summarize(meanVDBA = mean(VDBAsum))
# Meanline summer females
meanlineSUf <- dataset %>%
  dplyr::filter(season == "Summer",
                depid %in% females) %>%
  group_by(depid, timeBin) %>%
  summarize(VDBAsum = sum(VDBA,na.rm = TRUE)) %>%
  ungroup() %>% 
  group_by(timeBin) %>% 
  summarize(meanVDBA = mean(VDBAsum))

# Meanline winter males
meanlineWIm <- dataset %>%
  dplyr::filter(season == "Winter",
                depid %in% males) %>% #View
  group_by(depid, timeBin) %>%
  summarize(VDBAsum = sum(VDBA,na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(timeBin) %>% 
  summarize(meanVDBA = mean(VDBAsum))
# Meanline winter females
meanlineWIf <- dataset %>%
  dplyr::filter(season == "Winter",
                depid %in% females) %>%
  group_by(depid, timeBin) %>%
  summarize(VDBAsum = sum(VDBA,na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(timeBin) %>% 
  summarize(meanVDBA = mean(VDBAsum))

# dataset <- dataset %>%
#   mutate(dawnT = (hour(dataset$dawn) + minute(dataset$dawn)/60) - (hour(dataset$solarMidnight) + minute(dataset$solarMidnight)/60),
#          duskT = hour(dataset$dusk) + minute(dataset$dusk)/60 - (hour(dataset$solarMidnight) + minute(dataset$solarMidnight)/60))
shadeSU = data.frame(x1=c(0,20),
                     x2=c(3,23),
                     y1=c(0,0),
                     y2=c(1600,1600))
shadeWI = data.frame(x1=c(0,17),
                     x2=c(6,23),
                     y1=c(0,0),
                     y2=c(1600,1600))

# Summer total VDBA per solar hour

pVDBAsum24SU <- dataset %>%
  dplyr::filter(season == "Summer") %>% #View
  group_by(depid, timeBin,night) %>%
  summarize(VDBAsum = sum(VDBA,na.rm = TRUE)) %>%
  mutate(sex = depMeta$sex[depMeta$depid==depid]) %>%
  ggplot() +
  geom_line(aes(x=timeBin, y=VDBAsum, group=depid,linetype=sex),alpha=0.7) +
  geom_line(data=meanlineSUm,aes(x=timeBin, y=meanVDBA),linetype=1,color="black",size=1.5) +
  geom_line(data=meanlineSUf,aes(x=timeBin, y=meanVDBA),linetype=2,color="black",size=1.5) +
  scale_linetype_manual(values=c("Male"=1,"Female"=2)) +
  scale_y_continuous(breaks = seq(0,1600, by = 200), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0,23, by = 1),expand = c(0, 0)) +
  # theme(axis.text.x = element_text(size=40)) +
  # scale_x_continuous(breaks = seq(0,23, by = 1),expand = c(0, 0)) +
  labs(x="Solar Hour",
       y = NULL) + 
  geom_rect(data=shadeSU, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color='grey', alpha=0.5) +
  annotate("text", x = 0.25, y = 1400, label = "B. Summer", color="black", hjust = 0,size=6) +
  theme_classic(base_size = 13) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=20),
        plot.margin = margin(1, 1, 1, 1, "cm"))
pVDBAsum24SU

# Winter total ODBA per solar hour
pVDBAsum24WI <- dataset %>%
  dplyr::filter(season == "Winter") %>%
  group_by(depid, timeBin) %>%
  summarize(VDBAsum = sum(VDBA,na.rm = TRUE)) %>% 
  mutate(sex = depMeta$sex[depMeta$depid==depid]) %>%
  ggplot() +
  geom_line(aes(x=timeBin, y=VDBAsum, group=depid,linetype=sex),alpha=0.7)+
  geom_line(data=meanlineWIm,aes(x=timeBin, y=meanVDBA),linetype=1,color="black",size=1.5) +
  geom_line(data=meanlineWIf,aes(x=timeBin, y=meanVDBA),linetype=2,color="black",size=1.5) +
  scale_linetype_manual(values=c("Male"=1,"Female"=2)) +
  scale_y_continuous(breaks = seq(0,1600, by = 200), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0,23, by = 1),expand = c(0, 0)) +
  labs(x=NULL,
       y = NULL) + 
  geom_rect(data=shadeWI, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color='grey', alpha=0.5) +
  annotate("text", x = 0.25, y = 1400, label = "A. Winter", color="black",hjust = 0,size=6) +
  theme_classic(base_size = 13) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        plot.margin = margin(1, 1, 1, 1, "cm"))

pVDBAsum24WI
rm(Podba24Seas)
Podba24Seas <- ggarrange(pVDBAsum24WI,pVDBAsum24SU,nrow=2)
Podba24Seas <- annotate_figure(Podba24Seas,left = text_grob("VeDBA (gravitational g)", color = "black", rot = 90,size=20))

Podba24Seas
rm(meanlineSU,meanlineWI,pVDBAsum24SU,pVDBAsum24WI)
# Save a file at 300 ppi
ggsave(Podba24Seas, file="Figure3.png",width=12, height=8,dpi=300)
# Hourly mean VDBA (by season and sex) -BAD --------------------------

# Plot hourly VDBA (sum) by season and sex

# Meanline summer males
meanlineSUm <- dataset %>%
  dplyr::filter(season == "Summer",
                depid %in% males) %>%
  group_by(depid, timeBin) %>%
  summarize(VDBAmean = mean(VDBA,na.rm = TRUE)) %>%
  ungroup() %>% 
  group_by(timeBin) %>% 
  summarize(meanVDBA = mean(VDBAmean))
# Meanline summer females
meanlineSUf <- dataset %>%
  dplyr::filter(season == "Summer",
                depid %in% females) %>%
  group_by(depid, timeBin) %>%
  summarize(VDBAmean = mean(VDBA,na.rm = TRUE)) %>%
  ungroup() %>% 
  group_by(timeBin) %>% 
  summarize(meanVDBA = mean(VDBAmean))

# Meanline winter males
meanlineWIm <- dataset %>%
  dplyr::filter(season == "Winter",
                depid %in% males) %>% #View
  group_by(depid, timeBin) %>%
  summarize(VDBAmean = mean(VDBA,na.rm = TRUE)) %>%
  ungroup() %>% 
  group_by(timeBin) %>% 
  summarize(meanVDBA = mean(VDBAmean))
# Meanline winter females
meanlineWIf <- dataset %>%
  dplyr::filter(season == "Winter",
                depid %in% females) %>%
  group_by(depid, timeBin) %>%
  summarize(VDBAmean = mean(VDBA,na.rm = TRUE)) %>%
  ungroup() %>% 
  group_by(timeBin) %>% 
  summarize(meanVDBA = mean(VDBAmean))

# dataset <- dataset %>%
#   mutate(dawnT = (hour(dataset$dawn) + minute(dataset$dawn)/60) - (hour(dataset$solarMidnight) + minute(dataset$solarMidnight)/60),
#          duskT = hour(dataset$dusk) + minute(dataset$dusk)/60 - (hour(dataset$solarMidnight) + minute(dataset$solarMidnight)/60))
shadeSUmean = data.frame(x1=c(0,20),
                         x2=c(3,23),
                         y1=c(0,0),
                         y2=c(0.5,0.5))
shadeWImean = data.frame(x1=c(0,17),
                         x2=c(6,23),
                         y1=c(0,0),
                         y2=c(0.5,0.5))

# Summer total VDBA per solar hour

pVDBAsum24SU <- dataset %>%
  dplyr::filter(season == "Summer") %>% #View
  group_by(depid, timeBin,night) %>%
  summarize(VDBAmean = mean(VDBA,na.rm = TRUE)) %>%
  mutate(sex = depMeta$sex[depMeta$depid==depid]) %>%
  ggplot() +
  geom_line(aes(x=timeBin, y=VDBAmean, group=depid,linetype=sex),alpha=0.7) +
  geom_line(data=meanlineSUm,aes(x=timeBin, y=meanVDBA),linetype=1,color="black",size=1.5) +
  geom_line(data=meanlineSUf,aes(x=timeBin, y=meanVDBA),linetype=2,color="black",size=1.5) +
  scale_linetype_manual(values=c("Male"=1,"Female"=2)) +
  scale_y_continuous(breaks = seq(0,0.5, by = 0.05), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0,23, by = 1),expand = c(0, 0)) +
  # scale_x_continuous(breaks = seq(0,23, by = 1),expand = c(0, 0)) +
  labs(x="Solar Hour",
       y = NULL) + 
  geom_rect(data=shadeSUmean, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color='grey', alpha=0.5) +
  # annotate("text", x = 0.75, y = 740, label = "B. Summer", color="white", hjust = 0) +
  theme_classic(base_size = 13) +
  theme(legend.position = "none")
pVDBAsum24SU

# Winter total ODBA per solar hour
pVDBAsum24WI <- dataset %>%
  dplyr::filter(season == "Winter") %>%
  group_by(depid, timeBin) %>%
  summarize(VDBAmean = mean(VDBA,na.rm = TRUE)) %>% 
  mutate(sex = depMeta$sex[depMeta$depid==depid]) %>%
  ggplot() +
  geom_line(aes(x=timeBin, y=VDBAmean, group=depid,linetype=sex),alpha=0.7)+
  geom_line(data=meanlineWIm,aes(x=timeBin, y=meanVDBA),linetype=1,color="black",size=1.5) +
  geom_line(data=meanlineWIf,aes(x=timeBin, y=meanVDBA),linetype=2,color="black",size=1.5) +
  scale_linetype_manual(values=c("Male"=1,"Female"=2)) +
  scale_y_continuous(breaks = seq(0,0.5, by = 0.05), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0,23, by = 1),expand = c(0, 0)) +
  labs(x=NULL,
       y = NULL) + 
  geom_rect(data=shadeWImean, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color='grey', alpha=0.5) +
  # annotate("text", x = 0.75, y = 740, label = "A. Winter", color="white",hjust = 0) +
  theme_classic(base_size = 13) +
  theme(legend.position = "none")
pVDBAsum24WI

Podba24Seas <- ggarrange(pVDBAsum24WI,pVDBAsum24SU,nrow=2)
Podba24Seas <- annotate_figure(Podba24Seas,left = text_grob("Hourly VDBA Rate (gravitational g)", color = "black", rot = 90))

Podba24Seas
rm(meanlineSU,meanlineWI,pVDBAsum24SU,pVDBAsum24WI)
# Save a file at 300 ppi
ggsave(Podba24Seas, file="Seasonal total ODBA per solar hour_shaded with label.png",width=12, height=8,dpi=300)

# Hourly mean VDBA (by season and sex) -GOOD --------------------------

# Plot hourly VDBA (sum) by season and sex

# Meanline summer males
meanlineSUm <- dataset %>%
  dplyr::filter(season == "Summer",
                depid %in% males) %>%
  group_by(depid, timeBin) %>%
  summarize(VDBAsum = sum(VDBA,na.rm = TRUE)) %>%
  ungroup() %>% 
  group_by(timeBin) %>% 
  summarize(meanVDBA = mean(VDBAsum))
# Meanline summer females
meanlineSUf <- dataset %>%
  dplyr::filter(season == "Summer",
                depid %in% females) %>%
  group_by(depid, timeBin) %>%
  summarize(VDBAsum = sum(VDBA,na.rm = TRUE)) %>%
  ungroup() %>% 
  group_by(timeBin) %>% 
  summarize(meanVDBA = mean(VDBAsum))

# Meanline winter males
meanlineWIm <- dataset %>%
  dplyr::filter(season == "Winter",
                depid %in% males) %>% #View
  group_by(depid, timeBin) %>%
  summarize(VDBAsum = sum(VDBA,na.rm = TRUE)) %>%
  ungroup() %>% 
  group_by(timeBin) %>% 
  summarize(meanVDBA = mean(VDBAsum))
# Meanline winter females
meanlineWIf <- dataset %>%
  dplyr::filter(season == "Winter",
                depid %in% females) %>%
  group_by(depid, timeBin) %>%
  summarize(VDBAsum = sum(VDBA,na.rm = TRUE)) %>%
  ungroup() %>% 
  group_by(timeBin) %>% 
  summarize(meanVDBA = mean(VDBAsum))

# dataset <- dataset %>%
#   mutate(dawnT = (hour(dataset$dawn) + minute(dataset$dawn)/60) - (hour(dataset$solarMidnight) + minute(dataset$solarMidnight)/60),
#          duskT = hour(dataset$dusk) + minute(dataset$dusk)/60 - (hour(dataset$solarMidnight) + minute(dataset$solarMidnight)/60))
shadeSUmean = data.frame(x1=c(0,20),
                         x2=c(3,23),
                         y1=c(0,0),
                         y2=c(0.5,0.5))
shadeWImean = data.frame(x1=c(0,17),
                         x2=c(6,23),
                         y1=c(0,0),
                         y2=c(0.5,0.5))

# Summer total VDBA per solar hour

pVDBAsum24SU <- dataset %>%
  dplyr::filter(season == "Summer") %>% #View
  group_by(depid, timeBin,night) %>%
  summarize(VDBAsum = sum(VDBA,na.rm = TRUE)) %>%
  mutate(sex = depMeta$sex[depMeta$depid==depid]) %>%
  ggplot() +
  geom_line(aes(x=timeBin, y=VDBAmean, group=depid,linetype=sex),alpha=0.7) +
  geom_line(data=meanlineSUm,aes(x=timeBin, y=meanVDBA),linetype=1,color="black",size=1.5) +
  geom_line(data=meanlineSUf,aes(x=timeBin, y=meanVDBA),linetype=2,color="black",size=1.5) +
  scale_linetype_manual(values=c("Male"=1,"Female"=2)) +
  scale_y_continuous(breaks = seq(0,0.5, by = 0.05), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0,23, by = 1),expand = c(0, 0)) +
  # scale_x_continuous(breaks = seq(0,23, by = 1),expand = c(0, 0)) +
  labs(x="Solar Hour",
       y = NULL) + 
  geom_rect(data=shadeSUmean, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color='grey', alpha=0.5) +
  # annotate("text", x = 0.75, y = 740, label = "B. Summer", color="white", hjust = 0) +
  theme_classic(base_size = 13) +
  theme(legend.position = "none")
pVDBAsum24SU

# Winter total ODBA per solar hour
pVDBAsum24WI <- dataset %>%
  dplyr::filter(season == "Winter") %>%
  group_by(depid, timeBin) %>%
  summarize(VDBAmean = mean(VDBA,na.rm = TRUE)) %>% 
  mutate(sex = depMeta$sex[depMeta$depid==depid]) %>%
  ggplot() +
  geom_line(aes(x=timeBin, y=VDBAmean, group=depid,linetype=sex),alpha=0.7)+
  geom_line(data=meanlineWIm,aes(x=timeBin, y=meanVDBA),linetype=1,color="black",size=1.5) +
  geom_line(data=meanlineWIf,aes(x=timeBin, y=meanVDBA),linetype=2,color="black",size=1.5) +
  scale_linetype_manual(values=c("Male"=1,"Female"=2)) +
  scale_y_continuous(breaks = seq(0,0.5, by = 0.05), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0,23, by = 1),expand = c(0, 0)) +
  labs(x=NULL,
       y = NULL) + 
  geom_rect(data=shadeWImean, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color='grey', alpha=0.5) +
  # annotate("text", x = 0.75, y = 740, label = "A. Winter", color="white",hjust = 0) +
  theme_classic(base_size = 13) +
  theme(legend.position = "none")
pVDBAsum24WI

Podba24Seas <- ggarrange(pVDBAsum24WI,pVDBAsum24SU,nrow=2)
Podba24Seas <- annotate_figure(Podba24Seas,left = text_grob("Hourly VDBA Rate (gravitational g)", color = "black", rot = 90))

Podba24Seas
rm(meanlineSU,meanlineWI,pVDBAsum24SU,pVDBAsum24WI)
# Save a file at 300 ppi
ggsave(Podba24Seas, file="Seasonal total ODBA per solar hour_shaded with label.png",width=12, height=8,dpi=300)