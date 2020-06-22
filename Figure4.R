# Set up environment ------------------------------------------------------

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

# Confirm states are treated as factor
dataset$state_classif <- as.factor(dataset$state_classif)

# Down sample to 1 Hz -----------------------------------------------------

# datasetx<-dataset %>%
#   dplyr::filter(depid=="20190221_TW10_Z59") %>% 
#   dplyr::select(depid,dttz,VDBA,state_classif)
# datasetx$state_classif<-as.factor(datasetx$state_classif)
# # Function for getting the mode of a vector
# mode <- function(x) {
#   unique_x <- unique(x)
#   unique_x[which.max(tabulate(match(x, unique_x)))]
# }
# 
# dataset_1hz <- datasetx %>% 
#   group_by(depid) %>% 
#   # Calculate secs_since
#   dplyr::mutate(secs_since = floor(as.numeric(dttz - min(dttz)))) %>% 
#   group_by(depid,secs_since) %>% 
#   # Find the state mode per second
#   summarize(dttz=first(dttz),
#             VDBA = mean(VDBA),
#             state_classif = mode(state_classif))


# Create plots ------------------------------------------------------------


# Create shading to identify inset in main plot
# shade = data.frame(x1=as.POSIXct(strptime("2019-02-22 09:05:00",format="%Y-%m-%d %H:%M:%S"),tz=tzOffset), 
#                    x2=as.POSIXct(strptime("2019-02-22 09:06:00",format="%Y-%m-%d %H:%M:%S"),tz=tzOffset),
#                    y1=0,
#                    y2=1.4)
# 
# shade2 = data.frame(x1=as.POSIXct(strptime("2019-02-22 09:05:00",format="%Y-%m-%d %H:%M:%S"),tz=tzOffset), 
#                     x2=as.POSIXct(strptime("2019-02-22 09:06:00",format="%Y-%m-%d %H:%M:%S"),tz=tzOffset),
#                     y1=0,
#                     y2=1.4)

# Define times to subset time series for main plot
startMain <- as.POSIXct(strptime("2019-02-22 09:15:00",format="%Y-%m-%d %H:%M:%S"),tz=tzOffset)
endMain <- as.POSIXct(strptime("2019-02-22 09:25:00",format="%Y-%m-%d %H:%M:%S"),tz=tzOffset)

pMainFull <- dataset_1hz %>% 
  subset(dttz >= startMain & dttz <= endMain) %>%
  ggplot() +
  geom_bar(aes(x=dttz, y=VDBA, color=state_classif),
           stat = "identity") +
  scale_color_manual(values = cb) +
  scale_y_continuous(breaks = seq(0,1.4, by = 0.2), expand = c(0, 0)) +
  scale_x_datetime(limits=c(startMain,
                            as.POSIXct(strptime("2019-02-22 09:25:30",format="%Y-%m-%d %H:%M:%S"),tz=tzOffset)),
                   expand = c(0,0)) +
  # scale_x_datetime(expand = c(0,0)) +
  labs(y=expression("VDBA (gravitational g)")) +
  labs(x="Time") +
  theme_classic(base_size = 13) +
  theme(legend.position = "none",
        axis.title=element_text(size=18),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) #+
  # geom_rect(data=shade2, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color='grey', alpha=0.5)
pMainFull


# Subset time series for inset
startInset <-as.POSIXct(strptime("2019-02-22 09::00",format="%Y-%m-%d %H:%M:%S"),tz=tzOffset)
endInset <-as.POSIXct(strptime("2019-02-22 09:09:00",format="%Y-%m-%d %H:%M:%S"),tz=tzOffset)
# startInset2 <-as.POSIXct(strptime("2019-02-21 20:28:00",format="%Y-%m-%d %H:%M:%S"),tz=tzOffset)
# endInset2 <-as.POSIXct(strptime("2019-02-21 20:29:00",format="%Y-%m-%d %H:%M:%S"),tz=tzOffset)

addline_format <- function(x,...){
  gsub('\\s','\n',x)
}
# Plot subset time series
pInset <- dataset_1hz %>% 
  subset(dttz >= startInset & dttz <= endInset) %>% 
  ggplot() +
  geom_bar(aes(x=dttz, y=VDBA, color=state_classif, fill=state_classif),
           stat = "identity") +
  scale_color_manual(values = cb, aesthetics = c("color", "fill")) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_datetime(expand = c(0,0.2)) +
  # labs(x="Time (s)", y=expression(atop("VDBA", paste("(gravitational g)")))) +
  theme_classic(base_size = 13) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())+
  theme(legend.position = "none") #+
  # theme(axis.title=element_text(size=18),
  #       axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))

pInset

# Arrange inset within main plot. This prints to plot panel in R studio (does not print within R markdown)
grid.newpage()
vpb_ <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)  # the larger map
vpa_ <- viewport(width = 0.4, height = 0.4, x = 0.8, y = 0.8)  # the inset in upper right
print(pMain, vp = vpb_)
print(pInset, vp = vpa_)



# Save --------------------------------------------------------------------
ggsave(pMainFull, file="Figure4.png",width=6, height=4,dpi=300)
  


