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
pkgTest("gridExtra")
pkgTest("ggpubr")
pkgTest("sp")
pkgTest("adehabitatLT")
pkgTest("adehabitatHR")
pkgTest("rnaturalearth")
pkgTest("maptools")
pkgTest("geosphere")
pkgTest("ggmap")
pkgTest("maps")
pkgTest("mapdata")
pkgTest("rnaturalearthdata")

# Add season to dataset
dataset<- dataset %>%
  mutate(season=ifelse(month(dttz)=="2","Summer","Winter"))

# Load data
# load("TWgpsDataset-notRediscretized-WithExclusions.RData")
load(paste0(savePath,"/TWgpsDataset-notRediscretized-WithExclusions.RData")) # Use this for KDE analysis

# Calculate KDE -----------------------------------------------------------

GPS_SU <- dataset %>%
  subset(season=="Summer")
GPS_WI <- dataset %>%
  subset(season=="Winter")

xySU <- GPS_SU[,c(7,8)]
xyWI <- GPS_WI[,c(7,8)]
plot(xySU)

spdfSU <- SpatialPointsDataFrame(coords = xySU, data = GPS_SU,
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
spdfWI <- SpatialPointsDataFrame(coords = xyWI, data = GPS_WI,
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# transform lat/long to UTM and rename the columns for ease down the road
spdfSUutm <- spTransform(spdfSU, CRS("+proj=utm +zone=20S ellps=WGS84"))
spdfWIutm <- spTransform(spdfWI, CRS("+proj=utm +zone=20S ellps=WGS84"))

str(spdfSU)
UDSU <- kernelUD(spdfSUutm[,2],h="href", same4all = TRUE, grid = 500)
UDSUall <- kernelUD(spdfSUutm[,24],h="href", same4all = TRUE, grid = 500)
UDWI <- kernelUD(spdfWIutm[,2],h="href", same4all = TRUE, grid = 500)
UDWIall <- kernelUD(spdfWIutm[,24],h="href", same4all = TRUE, grid = 500)

# get our core and HR estimates based on the UD
kdeAreaSU <- kernel.area(UDSU, percent=c(50, 95), unin = "m", unout=c("km2"))
kdeAreaWI <- kernel.area(UDWI, percent=c(50, 95), unin = "m", unout=c("km2"))

kdeSU <- as.data.frame(t(kdeAreaSU))
kdeSU <- kdeSU %>% rownames_to_column("depid")
kdeSU$depid <- sub('.', '', kdeSU$depid) # Remove X that appeared as first character of depid string
kdeSU$season <- "Summer"

kdeWI <- as.data.frame(t(kdeAreaWI))
kdeWI <- kdeWI %>% rownames_to_column("depid")
kdeWI$depid <- sub('.', '', kdeWI$depid) # Remove X that appeared as first character of depid string
kdeWI$season <- "Winter"

kde <- rbind(kdeSU,kdeWI)

kde_sum <- kde %>%
  group_by(season) %>% 
  summarize(kde95_mean = mean(kde$`95`),# 95% KDE seasonal mean and SD
            kde95_sd = sd(kde$`95`), 
            kde50_mean = mean(kde$`50`),
            kde50_sd = sd(kde$`50`))

# Extract home range contours and transform to lat/long
homerangeSU50 <- spTransform(getverticeshr(UDSU, 50), CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
homerangeSU95 <- spTransform(getverticeshr(UDSU, 95), CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
homerangeSU50all <- spTransform(getverticeshr(UDSUall, 50), CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
homerangeSU95all <- spTransform(getverticeshr(UDSUall, 95), CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

homerangeWI50 <- spTransform(getverticeshr(UDWI, 50), CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
homerangeWI95 <- spTransform(getverticeshr(UDWI, 95), CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
homerangeWI50all <- spTransform(getverticeshr(UDWIall, 50), CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
homerangeWI95all <- spTransform(getverticeshr(UDWIall, 95), CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# Check to make sure these look right
plot(homerangeSU50)
plot(homerangeSU95)
plot(homerangeWI50)
plot(homerangeWI95)

# Fortify converts polygons into a dataframe
hrSU50df <- fortify(homerangeSU50)
hrSU95df <- fortify(homerangeSU95)
hrSU50alldf <- fortify(homerangeSU50all)
hrSU95alldf <- fortify(homerangeSU95all)

hrWI50df <- fortify(homerangeWI50)
hrWI95df <- fortify(homerangeWI95)
hrWI50alldf <- fortify(homerangeWI50all)
hrWI95alldf <- fortify(homerangeWI95all)


# Plot KDE ----------------------------------------------------------------


# Set zoom to fit all the data (20 is all the way zoomed in)
mapZoomSU <- 14 

# Plot the extent of this map (NOTE: must adjust zoom parameter)
myMapSU <-  get_googlemap(center = c(lon = mean(c(min(GPS_SU$long), max(GPS_SU$long))),  
                                     lat = mean(c(min(GPS_SU$lat),max(GPS_SU$lat)))),
                          zoom = mapZoomSU, 
                          scale = 2,
                          extent="device",
                          maptype="hybrid", #"terrain"
                          style = c(feature = "all", element = "labels", visibility = "off"))

# Plot summer KDE (50 and 95)
PkdeSU <- ggmap(myMapSU) + #ggplot() + 
  # geom_polygon(data=hrSU95df, aes(y=lat,x=long, group=group, color=id),fill=NA, linetype="blank",alpha= .25) +
  # geom_polygon(data=hrSU95df, aes(y=lat,x=long, group=group, color=id),fill=NA, alpha= .8, size=1.5,linetype="solid") +
  # geom_polygon(data=hrSU50df, aes(y=lat,x=long, group=group, color=id),fill=NA, linetype="blank",alpha= .25) +
  # geom_polygon(data=hrSU50df, aes(y=lat,x=long, group=group, color=id),fill=NA, alpha= .8, size=1.5,linetype=3) +
  # stat_density2d(aes(x = long, y = lat, group=depid, fill = ..level.., alpha = ..level..),
  #   size = 0.01, bins = 50, data = GPS_SU,
  #   geom = "polygon") +
  geom_point(data = GPS_SU, aes(y=lat,x=long, color=depid ),size = .6,alpha = .8) +
  geom_polygon(data=hrSU95alldf, aes(y=lat,x=long, group=group),fill=NA, color="white", size = 1.5) +
  geom_polygon(data=hrSU50alldf, aes(y=lat,x=long, group=group),fill=NA, color="white",size = 1.5,linetype=3) +
  # annotate("text", x = -60.22, y = -51.2915, label = "a", color="white") +
  coord_fixed(1.3) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank()) 
PkdeSU

# Plot winter KDE (50 and 95)
# First adjust zoom because different than plot by state and ID above
mapZoomWI <- 11
myMapWI <-  get_googlemap(center = c(lon = mean(c(min(GPS_WI$long), max(GPS_WI$long))),
                                     lat = mean(c(min(GPS_WI$lat),max(GPS_WI$lat)))),
                          zoom = mapZoomWI, 
                          scale = 2,
                          extent="device",
                          maptype="hybrid", #"terrain"
                          style = c(feature = "all", element = "labels", visibility = "off"))
PkdeWI <- ggmap(myMapWI) + #ggplot() + 
  # geom_polygon(data=hrWI95df, aes(y=lat,x=long, group=group, color=id),fill=NA, linetype="blank",alpha= .25) +
  # geom_polygon(data=hrWI95df, aes(y=lat,x=long, group=group, color=id),fill=NA, alpha= .8, size=1.5,linetype="solid") +
  # geom_polygon(data=hrWI50df, aes(y=lat,x=long, group=group, color=id),fill=NA, linetype="blank",alpha= .25) +
  # geom_polygon(data=hrWI50df, aes(y=lat,x=long, group=group, color=id),fill=NA, alpha= .8, size=1.5,linetype=3) +
  geom_point(data = GPS_WI, aes(y=lat,x=long, color=depid ),size = .6,alpha = .8) +
  geom_polygon(data=hrWI95alldf, aes(y=lat,x=long, group=group),fill=NA, color="white", size = 1.5) +
  geom_polygon(data=hrWI50alldf, aes(y=lat,x=long, group=group),fill=NA, color="white",size = 1.5,linetype=3) +
  # annotate("text", x = -59.95, y = -51.228, label = "b", color="white") +
  coord_fixed(1.3) +
  # geom_rect(aes(xmin = -60.255, xmax = -60.22, ymin = -51.32, ymax = -51.29),
  #           alpha = .5,fill=NA,linetype="solid",color="white") +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())
PkdeWI 

PkdeSeas <- ggarrange(PkdeSU,PkdeWI,nrow=2)
PkdeSeas
ggsave(PkdeSeas, file="Figure6.png",width=12, height=8, dpi=300)
ggsave(PkdeSU, file="Figure6a.png",width=12, height=8, dpi=300)
ggsave(PkdeWI, file="Figure6b.png",width=12, height=8, dpi=300)

# Plot Falklands map ------------------------------------------------------

world <- ne_countries(scale = "large", returnclass = "sf")

ggplot() +
  geom_sf(data = world) +
  coord_sf(xlim = c(-61.5, -57.5),
           ylim = c(-52.5, -50.75), expand = FALSE) +
  theme_classic() +
  # xlab("Longitude") +
  # ylab("Latitude") +
  theme(panel.grid.major = element_line(color = "transparent", linetype = "dashed", size = 0.5),
        panel.background = element_rect(fill = "white")) #+
  # theme(panel.border = element_blank(),
  #       panel.grid.major = element_blank(),
  #       panel.grid.minor = element_blank(),
  #       panel.background = element_blank())

  
# ggplot() +
#     geom_sf(data = world) +
#     coord_sf(xlim = c(-61.5, -57.5),
#              ylim = c(-52.5, -50.75), expand = FALSE) +
#     theme_classic() +
#     xlab("Longitude") +
#     ylab("Latitude") +
#     theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5),
#           panel.background = element_rect(fill = "aliceblue"))
  
  
southAmerica <- ne_countries(scale = "large", returnclass = "sf", continent = 'South America')
ggplot() +
  geom_sf(data = southAmerica) +
  coord_sf(xlim = c(-85, -35),
           ylim = c(-60, 15), expand = FALSE) +
  theme_classic() +
  # xlab("Longitude") +
  # ylab("Latitude") +
  theme(#panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5),
    panel.background = element_rect(fill = "white"))