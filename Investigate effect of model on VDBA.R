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
dataPath <- choose.dir(caption="Select the folder containing deployment raw data folders") 
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

# Set the Working Directory to the location of this File
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(current_path)
getwd()

# Set the Timezone Offset from UTC 
tzOffset <- "Etc/GMT+3"

# Load Data ------------------------------------------------------------
setwd(savePath)
load("dataset-1Hz-VDBA-ODBA-clipVal-bitFix.RData")
# load("dataset-10Hz-VDBA-ODBA-clipVal-bitFix.RData")
setwd(current_path)
  

# Investigate VDBA distributions --------------------------------------------------------

# By depid
p <- dataset %>%
  # dplyr::filter(depid=="20170212_TW1_X36") %>% 
  ggplot()+
  geom_histogram(aes(x=VDBA),binwidth=0.01) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  theme_classic() +
  labs(x = "VDBA (gravitational g)",
       y = NULL,
       title = depid) +
  facet_wrap(~depid)

# Plots to visually assess tag model differences in VDBA distributions
p2 <- dataset %>%
  group_by(depid) %>%
  ggplot()+
  geom_histogram(aes(log10(VDBA),group=depid,color=depid),binwidth = .05)+
  theme(legend.position = "none") +
  facet_wrap(~model)

p3 <- dataset %>%
  group_by(depid) %>%
  ggplot()+
  geom_histogram(aes(log10(VDBA),group=depid,color=depid),binwidth = .05)+
  theme(legend.position = "none") +
  facet_wrap(~depid)

# Save plots as .png

# ggsave(p9,file="log10VDBA_by_depid.jpeg",width=12, height=8,dpi=300)
ggsave(p, path = paste0(dataPath,"/",depMeta$depid[i],"/"), file=paste0(depid,"-AccFieldStrength.png"))

