# This function loads required packages and installs them if they are not found
pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}
pkgTest("tidyverse")
pkgTest("ggplot2")
pkgTest("lubridate")
pkgTest("scales")

# Define global variables
tzOffset <- "Etc/GMT+3" # Set the Timezone Offset from UTC 
cb <- c("#F0E442","#E69F00", "#56B4E9", "#009E73") # these are the colors for the state labels

# Load data
load("dataset_1hz.RData")

dataset_sub <- dataset %>%
  dplyr::filter(depid=="20190221_TW10_Z59") %>%
  subset(dttz >= startMain & dttz <= endMain) %>% 
  select(depid,dttz,VDBA,ODBA)

# Confirm states are treated as factor
dataset_1hz$state_classif <- as.factor(dataset_1hz$state_classif)

# Subset time series for main plot
startMain <- as.POSIXct(strptime("2019-02-21 20:20:00",format="%Y-%m-%d %H:%M:%S"),tz=tzOffset)
endMain <- as.POSIXct(strptime("2019-02-21 20:50:00",format="%Y-%m-%d %H:%M:%S"),tz=tzOffset)

pMainFull <- dataset_1hz %>% 
  # subset(dttz >= startMain & dttz <= endMain) %>% 
  ggplot() +
  geom_bar(aes(x=dttz, y=VDBA, color=state_classif),
           stat = "identity") +
  scale_color_manual(values = cb) +
  scale_y_continuous(breaks = seq(0,1.4, by = 0.2), expand = c(0, 0)) +
  # scale_x_datetime(limits=c(startMain,
  #                           as.POSIXct(strptime("2019-02-21 20:50:30",format="%Y-%m-%d %H:%M:%S"),tz=tzOffset)),
  #                  expand = c(0,0)) +
  scale_x_datetime(expand = c(0,0)) +
  labs(y=expression('observation X'[t])) +
  theme_classic(base_size = 13) +
  theme(legend.position = "none",
        axis.title=element_text(size=18),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_blank())
pMainFull
ggsave(pMain, file="timeSeries.png",width=12, height=4,dpi=300)
