# RRF Raptor Movement Workshop 2017

# Contact ross@beringiasouth.org with questions

# GPS movement data are from 2 adult golden eagles tracked in Montana from May through 
# July of 2014 and is to be used for this exercise only.  
# Data courtesy of Craighead Beringia South.  

# I apologize in advance for spelling errors.

# Step 1 is install the packages that we are going to use.  
# You likely already have these installed, but if not do so now.
install.packages(c("adehabitatHR","rgdal","sp", "ggmap", "mapproj", "maptools", "rgeos", 
                   "raster","leaflet","move","ctmm","BBMM","PBSmapping"))

# Next, we're going to load our packages
packagelist <- c("adehabitatHR","rgdal","sp", "ggmap", "mapproj","rgeos","maptools","raster","leaflet",
                 "move","ctmm","BBMM", "PBSmapping")  
lapply(packagelist, require, character.only = TRUE)

# Now, we're going to set our working directory.  It's also possible you've already done this
# as well but if not, do so now.  
# setwd("C:/ SET YOUR OWN WORKING DIRECTORY HERE")
setwd("~/Desktop/RRF")

# Now, with your dataset in your working directory, lets bring in the data and take a look at it. 
movedata <- read.csv("ExampleData.csv", header=T)
head(movedata)

# Let's plot the locations using the leaflet package.
pal <- colorFactor(c("blue", "black"), domain = unique(movedata$PTT_ID))

leaflet(data = movedata) %>% addTiles(group="Map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addCircleMarkers(~location.long, ~location.lat, popup = ~as.character(PTT_ID), 
                   label = ~as.character(PTT_ID),
                   radius = 5,
                   stroke=F,
                   fillOpacity = 0.7,
                   color = ~pal(PTT_ID)) %>%
  addLayersControl(
    baseGroups=c("Map","Satellite"),
    options=layersControlOptions(collapsed=F)
  )


# We have some important organizational things to do. 
# First, we're going to ID coordinates the in the data frame, define their 
# projection and then transform them to UTM, which we need to analyze home 
# ranges (HR) in adehabitatHR.  We're also going to do some data frame manipulations 
# to make it easier to run our home ranges and to include some columns that may be helpful, 
# such as month, year and time.  The final data frame (final_move) will be what we use for 
# our HR estimation.  

# Before we do anything, we're going to make sure our data are ordered properly.  This isn't a big
# deal with some estimators but a major issue with others and it's a good habit to be in.
movedata <- movedata[with(movedata,order(movedata$PTT_ID,movedata$timestamp)),]

# define coordinates and datum in the data frane
coordinates(movedata) <- c("location.long", "location.lat")
proj4string(movedata) <- CRS("+proj=longlat +datum=WGS84")  

# transform lat/long to UTM and rename the columns for ease down the road
utm <- spTransform(movedata, CRS("+proj=utm +zone=12 ellps=WGS84"))
utm_locs <- data.frame(as(utm, "SpatialPoints"))
colnames(utm_locs) <- c("UTM_W", "UTM_N")

# define date and time, and create month, year and time columns for the new data frame.
date_time <- as.POSIXct(strptime(as.character(movedata$timestamp),"%Y-%m-%d %H:%M:%S", tz="GMT"))
month <- as.numeric(format(date_time, "%m"))
year<- as.numeric(format(date_time, "%Y"))
time <- strftime(date_time, format="%H:%M:%S")

# create new data and improved frame for HR analyses and have a look at it
final_move <- data.frame(ID=as.factor(movedata$PTT_ID),GS=movedata$ground.speed,
                         Head=movedata$heading,Height=movedata$height.raw,UTM_W=utm_locs$UTM_W,
                         UTM_N=utm_locs$UTM_N,month,year,time, timestamp=date_time)
head(final_move)


# Define the coordinates and the projection in the new data frame
coordinates(final_move) <- c("UTM_W", "UTM_N")
projection(final_move) <- CRS("+proj=utm +zone=12 ellps=WGS84")


# Time to move on to esimating home range and core areas.  
# For the sake of ease, let's just say we want to know what the size of each individuals annual 
# home range and core areas are using different HR estimators.  We're going to define the home 
# range using 95% isopleths and the core area using 50% isopleths.  We're going to start with 
# the simplest (and most hated!) home range estimator, the Minimim Convex Polygon.  All our 
# these first HR estimators are in the package adehabitatHR.  Estimating MCPs in adehabitatHR 
# is very simple and straight forward.  For this first part, we're only going to look at estimated 
# areas, we'll map things out after we have all HR estimates.  We're creating polygons for the home 
# range scale for all estimators.  For the MCP, the command "mcp" creates the polygon and then we 
# transform it to lat/long for later mapping.  And yes, it has be metric for analyses and lat/long 
# for mapping.     

# MCPs from adehabitatHR
ge_hr_mcp <- mcp.area(final_move[,1], percent=c(50,95), unout=c("km2"), plot=F)
colnames(ge_hr_mcp) <- c("mcp_114328", "mcp_117408")

# the 'mcp' function creates the polygon, in this case what we've defined as the home range
hr_mcp_poly <- mcp(final_move[,1], percent=95)
# transform to lat long for mapping
hr_mcp_latlong <- spTransform(hr_mcp_poly, CRS("+proj=longlat +datum=WGS84"))
# look at the estimates
ge_hr_mcp


# Next is the kernel density estimator or KDE.  Estimating the KDE is a bit more complex but 
# still straight forward.  The first thing we're going to do here is to define the grid.  The 
# grid defines the spatial extent of the estimates. Sometimes, you can use the default but 
# oftentimes you'll get an error and you have to go back and define the grid.  For these data, 
# we're defining the grid with the first few lines of code.  The command "kernelUD" estimates 
# the utilization distribution (UD) using the kernel method and the "kernel.area" command gives 
# us the corresponding area estimates for our desired isopleth percentages.  We are using the 
# default smoothing paramters (href) to estimate the UD.  We often see the least square cross 
# validation algorithm used for smoothing but we often have convergence issues with raptor data 
# due to repeated use of a single location.  In this chunk, we end with a comparison in estimated 
# areas between the MCP and the KDE.   

# define the grid
x <- seq(min(final_move$UTM_W), max(final_move$UTM_W), by=30)
y <- seq(min(final_move$UTM_N), max(final_move$UTM_N), by=30)
xy <- expand.grid(x=x, y=y)
coordinates(xy) <- ~x+y
gridded(xy) <- TRUE

# create the utilization distribution
ge_hr_kde_UD <- kernelUD(final_move[,1], h="href", grid=xy)
# get our core and HR estimates based on the UD
ge_hr_kde <- kernel.area(ge_hr_kde_UD, percent=c(50, 95), unout=c("km2"))
colnames(ge_hr_kde) <- c("kde_114328", "kde_117408")

# create our polygon of the HR
hr_kde_poly <- getverticeshr(ge_hr_kde_UD, percent=95)
hr_kde_latlong <- spTransform(hr_kde_poly, CRS("+proj=longlat +datum=WGS84"))

# compare mcp and kde estimates
data.frame(ge_hr_mcp,ge_hr_kde)


# Next is the Local Convex Hull family of HR estimators (LoCoH).  We're going to use the adaptive 
# LoCoH (aLoCoH) for this example.  The estimation of the aLoCoH is again pretty straight forward 
# with the exception of choosing a value for the "a" parameter.  The "a" parameter is defined as 
# "the maximum number of nearest neighbors such that the sum of their distances is less than or equal 
# to this parameter."  In application, the max distance between any 2 locations per indvidual may 
# suffice.  If not, decrease and increase the value by 10% until it works.  The error message when 
# you need to change the value for "a" typically notes "orphaned holes" in your data.  For this example, 
# 11,817m separated the furthest 2 locations so we set "a" to equal 23,634m.  

# We use LoCoH.a to estimate UD and MCHu2hrsize to estimate home range and core area sizes.  
# We can't use the "unout" command in MCHu2hrsize therefore we have to convert from m2 to km2.  
# We also can't get polygons from the output so we have to first create a raster (then convert to 
# SpatialPixelsDataFrame) to project the home range onto.  We use the MCHu.rast function for this.  
# Next, We use the projected SpatialPixelsDataFrame home ranges and convert them to raster for each 
# individual.  At that point, we can convert the raster HR estimates to polygons.  Hey, R is free.  
# Instead of including both individuals in one polygon file, we have 2.  No big deal.  We can join 
# them using a the "union" command but weird things happen to the projected polygons so we're not 
# going to do that. We start the chunk with gc(), which simply deletes unused objects from the memory 
# to help speed things along slightly.  

gc()
# Estimate thd UD
ge_hr_alocoh_UD <- LoCoH.a(final_move[,1], a=23634, unin="m", unout="km2")
# Get our core and home range estimates
ge_hr_alocoh <- MCHu2hrsize(ge_hr_alocoh_UD, percent=c(50, 95), plotit = F)
# Convert to km2
ge_hr_alocoh_km <- ge_hr_alocoh*0.000001
colnames(ge_hr_alocoh_km) <- c("locoh_114328","locoh_117408")

# raster creation to project HR raster upon
xy <- matrix(rep(1),1275,1275)
rast <- raster(xy)
extent(rast) <- c(min(final_move$UTM_W), max(final_move$UTM_W),min(final_move$UTM_N), max(final_move$UTM_N))
projection(rast) <- CRS("+proj=utm +zone=12 ellps=WGS84")
p <- as(rast, "SpatialPixelsDataFrame")
# create HR raster
hr_alocoh <- MCHu.rast(ge_hr_alocoh_UD, spdf=p, percent=95)

# convert to SpatialPixelsDataFrame and then to polygons
hr_alocoh2 <- raster(hr_alocoh,1)
hr_alocoh3 <- raster(hr_alocoh,2)
hr_alocoh2a <- rasterToPolygons(hr_alocoh2, dissolve=TRUE)
hr_alocoh3a <- rasterToPolygons(hr_alocoh3, dissolve=TRUE)

# transform to lat/long
hr_alocoh2a_latlong <- spTransform(hr_alocoh2a, CRS("+proj=longlat +datum=WGS84"))
hr_alocoh3a_latlong <- spTransform(hr_alocoh3a, CRS("+proj=longlat +datum=WGS84"))

# union fucntion, just to show it
hr_alocoh_latlat <- union(hr_alocoh2a_latlong, hr_alocoh3a_latlong)

data.frame(ge_hr_mcp,ge_hr_kde,ge_hr_alocoh_km)


# Now we're going to plot the polygons, we have to use the 'fortify' function to 
# prep the polygons for plotting.  We're doing each HR for each individual separately
# because weird things happen to the HRs in leaflet if we don't.  

mcp_hr_1 <- fortify(subset(hr_mcp_latlong, id=="114328"))
mcp_hr_2 <- fortify(subset(hr_mcp_latlong, id=="117408"))
kde_hr_1 <- fortify(subset(hr_kde_latlong, id=="114328"))
kde_hr_2 <- fortify(subset(hr_kde_latlong, id=="117408"))
alocoh_hr1 <- fortify(hr_alocoh2a_latlong)
alocoh_hr2 <- fortify(hr_alocoh3a_latlong)

leaflet(data = as.data.frame(movedata)) %>% addTiles(group="Map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addCircleMarkers(~location.long, ~location.lat, popup = ~as.character(PTT_ID), 
                   label = ~as.character(PTT_ID),
                   radius = 5,
                   stroke=F,
                   fillOpacity = 0.7,
                   color = ~pal(PTT_ID)) %>%
  addPolygons(data=mcp_hr_1, lng=~long, lat=~lat, group="id", weight=1)%>%
  addPolygons(data=mcp_hr_2, lng=~long, lat=~lat, group="id", weight=1)%>%
  addPolygons(data=kde_hr_1, lng=~long, lat=~lat, group="id", weight=1, color="green")%>%
  addPolygons(data=kde_hr_2, lng=~long, lat=~lat, group="id", weight=1, color="green")%>%
  addPolygons(data=alocoh_hr1, lng=~long, lat=~lat, group="id", weight=1, color="yellow")%>%
  addPolygons(data=alocoh_hr2, lng=~long, lat=~lat, group="id", weight=1, color="yellow")%>%
  addLayersControl(
    baseGroups = c("Map", "Satellite"),
    overlayGroups = c("mcp_hr_1","mcp_hr_2", "kde_hr_1", "kde_hr_2","alocoh_hr1","alocoh_hr2"),
    options = layersControlOptions(collapsed=F)
  )

#############################################################
# We're going to plot the HR's in ggmap/ggplot2 as well since there are some issues with leaflet and
# just to use ggmap

# first, we're going create the map and set our extent followed by plotting all relocations
livmt <- get_map(location = c(lon = mean(movedata$location.long), lat = mean(movedata$location.lat)), 
                 zoom = 10, maptype = "hybrid", scale = 2)
ggmap(livmt) + geom_point(data=data.frame(movedata), aes(x=location.long, y=location.lat,colour=as.factor(PTT_ID)),
                          size=0.5, show.legend=F)

# We're going to look at each bird seperatey starting with 117408
d117408 <- subset(as.data.frame(movedata), PTT_ID=="117408")
m117408 <- get_map(location = c(lon = mean(d117408$location.long), lat = mean(d117408$location.lat)), zoom = 13,
maptype = "hybrid")
ggmap(m117408) + geom_point(data=d117408, aes(x=location.long, y=location.lat,colour=as.factor(PTT_ID)),size=1, show.legend=F)

# mcp
ggmap(m117408) + geom_point(data=d117408, aes(x=location.long, y=location.lat,colour=as.factor(PTT_ID)),size=1, show.legend=F)+ 
geom_polygon(aes(x=long, y=lat, group=id), data=mcp_hr_2, alpha=0.7)

# kde
ggmap(m117408) + geom_point(data=d117408, aes(x=location.long, y=location.lat,colour=as.factor(PTT_ID)),size=1, show.legend=F)+ 
geom_polygon(aes(x=long, y=lat, group=id), fill="red", data=kde_hr_2, alpha=0.7)

# aLoCoH
ggmap(m117408) + geom_point(data=d117408, aes(x=location.long, y=location.lat,colour=as.factor(PTT_ID)),size=1, show.legend=F)+ 
geom_polygon(aes(x=long, y=lat, group=id), fill="blue", data=alocoh_hr2, alpha=0.7)

# all combined
ggmap(m117408) + 
geom_point(data=d117408, aes(x=location.long, y=location.lat,colour=as.factor(PTT_ID)),size=1, show.legend=F) + 
geom_polygon(aes(x=long, y=lat, group=id), data=mcp_hr_2, alpha=0.5) + 
geom_polygon(aes(x=long, y=lat, group=id), fill="red", data=kde_hr_2, alpha=0.5) + 
geom_polygon(aes(x=long, y=lat, group=id), fill="blue", data=alocoh_hr2, alpha=0.5) 

##############################################
# We'll plot the other bird now

d114328 <- subset(as.data.frame(movedata), PTT_ID=="114328")
m114328 <- get_map(location = c(lon = mean(d114328$location.long), lat = mean(d114328$location.lat)), zoom = 13,
                   maptype = "hybrid")
ggmap(m114328) + geom_point(data=d114328, aes(x=location.long, y=location.lat,colour=as.factor(PTT_ID)),size=1, show.legend=F)

# mcp
# hr_mcp_latlong_map <- fortify(subset(hr_mcp_latlong, id=="114328"))
ggmap(m114328) + geom_point(data=d114328, aes(x=location.long, y=location.lat,colour=as.factor(PTT_ID)),size=1, show.legend=F)+ 
geom_polygon(aes(x=long, y=lat, group=id), data=mcp_hr_1, alpha=0.7)

# kde
ggmap(m114328) + geom_point(data=d114328, aes(x=location.long, y=location.lat,colour=as.factor(PTT_ID)),size=1, show.legend=F)+ 
geom_polygon(aes(x=long, y=lat, group=id), fill="red", data=kde_hr_1, alpha=0.7)


# aLoCoH
ggmap(m114328) + geom_point(data=d114328, aes(x=location.long, y=location.lat,colour=as.factor(PTT_ID)),size=1, show.legend=F)+ 
geom_polygon(aes(x=long, y=lat, group=id), fill="blue", data=alocoh_hr1, alpha=0.7)

# all
ggmap(m114328) + 
geom_point(data=d114328, aes(x=location.long, y=location.lat,colour=as.factor(PTT_ID)),size=1, show.legend=F) + 
geom_polygon(aes(x=long, y=lat, group=id), data=mcp_hr_1, alpha=0.5) + 
geom_polygon(aes(x=long, y=lat, group=id), fill="red", data=kde_hr_1, alpha=0.5) + 
geom_polygon(aes(x=long, y=lat, group=id), fill="blue", data=alocoh_hr1, alpha=0.5) 

# This function clears the plots.
dev.off()

# Now we're going to move on from the adehabitatHR package and try HR estimators from the ctmm package and the
# the BBMM package.  First, we're going to try the autocorrelated KDEs (akde) which are in the ctmm package. 
# The method described by Fleming and Calabrese (2017) is the default not in the ctmm package.  In general, 
# we have do a bit more (or different) data prep and the packages aren't quite as user friendly they do 
# offer advantages for HR estimation.  The akdes account to for autocorrelation (major issue with movement data)
# and therefore may be preferable to a standard KDE or other estimator.  

# Citations for the CTMM package:

# Calabrese, J. M., Fleming, C. H., & Gurarie, E. (2016). ctmm: an r package for analyzing animal relocation 
#           data as a continuous‐time stochastic process. Methods in Ecology and Evolution, 7(9), 1124-1132.

# Fleming, C. H., & Calabrese, J. M. (2017). A new kernel density estimator for accurate home‐range and 
#           species‐range area estimation. Methods in Ecology and Evolution, 8(5), 571-579.

# Step 1 here is to get back to our movedata data frame, which is the orginal form of our example data.  We use
# the data.frame function to convert it from a spatial object to a data frame.  We're using movedata because
# we need our lat/long back.  We also rename column 11 to ease our conversion of the data frame to a 'move'
# object, which is required for the following HR estimators.  Move objects require columns to be named 
# as they would be if you downloaded the locations from Movebank.
movedata_df <- data.frame(movedata)
head(movedata_df)
colnames(movedata_df)[11] <- "individual.local.identifier"

# Next, we convert to move object.
movedata_move <- move(x=movedata_df$location.long, y=movedata_df$location.lat,
                      time=as.POSIXct(movedata_df$timestamp, format="%Y-%m-%d %H:%M:%S", tz="GMT"), 
                      proj=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"),
                      data=movedata_df, animal=movedata_df$individual.local.identifier, 
                      sensor=movedata_df$sensor.type)

# Moving on to the akde.  To keep things simple and visualize the information that we need to estimate the HRs, 
# we're only going to use data from invidual 114328 for this exercise.  The first thing we're going to do for
# the akde is convert our move object again, this time to a telemetry object.  It is easiest to convert from 
# move object to telemetry object vs. a data frame to a telemetry object.  Our new telemetry object is a list
# and we're going to pull out our indvidual, which is the first object in the list.  We'll plot him for fun.
autokde <- as.telemetry(movedata_move) 
autokde <- autokde[[1]]
plot(autokde)

# Moving on to the nuts and bolts.  The first step for the akde is to plot the variogram. Skimming the surface here, 
# the variogram is going to give us an idea of how our data look and the possible models we can entertain 
# for our akde.  The foundation of the ctmm package is to fit competing model types to our movement data, and
# using information criteria (AICc) to choose which model best fits the movement pattern of our tracked
# individual.  We're going to go through this somewhat slowly and see what each step looks like and what
# that information means.  Once we have our top supported model, we use that model to estimate our akde.  
# First though, we start with the variogram.  We're going to start with ?variogram() just to see what this 
# thing is doing.
?variogram()
gevario <- variogram(autokde)

# We're going to plot the variogram, both zoomed in and zoomed out.  If we reference Calabrese et al. (2016), 
# we can get a better idea of what these plots show us.  
level <- c(0.5,0.95) # 50% and 95% CIs
xlim <- c(0,12 %#% "hour") # 0-12 hour window
plot(gevario,xlim=xlim,level=level)

plot(gevario,fraction=0.25,level=level)
title("Population variogram")

# Now, we're going to use the variogram.fit function.  This function allows us to alter some model parameters
# that we will use in to begin in our model selection portion, which is done using the 'ctmm.select' function.
# Using the variogram.fit function, we can play around with the sliders until we get initial paramters that seem
# acceptable.  These paramaters are stored as GUESS in your workspace.
gs <- variogram.fit(gevario, interactive=T)

# Next, we go on to the model selection portion.  We're going to use our saved parameter estimates and call them
# in the CTMM argument.  The summary of the CTMM select fucntion will show us the competing models, their rank,
# and weight.
fitmods <- ctmm.select(autokde, CTMM=GUESS, verbose = T)
summary(fitmods)

# Now, we go with our top model in the list.  It had good enough support for our purposes.
ou <- fitmods[[1]]
plot(gevario, CTMM=ou, fraction=0.25)

# Let's finally get our estimate and plot the UD.
akde.ou <- akde(autokde, CTMM=ou)
plot(autokde, UD=akde.ou)

# Now, we'll check size.  
akde.ou.sum <- summary(akde.ou)
data.frame(akde.ou.sum[2], ge_hr_kde$kde_117408[2])

# create a polygon and convert to lat/long
akde_poly <- SpatialPolygonsDataFrame.UD(akde.ou,level.UD=0.95)
akde_poly_latlong <- spTransform(akde_poly,CRS("+proj=longlat +datum=WGS84"))

# plot the akde vs. a standard kde
hr_akde <- fortify(akde_poly_latlong)

ggmap(m114328) + geom_point(data=d114328, aes(x=location.long, y=location.lat,colour=as.factor(PTT_ID)),size=1, show.legend=F)+ 
geom_polygon(aes(x=long, y=lat, group=id), fill="red", data=kde_hr_1, alpha=0.7)+
geom_polygon(aes(x=long, y=lat, group=id), fill="gray", data=hr_akde, alpha=0.25) 

################################################
# Now we'll go ahead and try another package to estimate a HR using Brownian-bridge movement models (BBMMs), 
# the BBMM package. The BBMMs have gotten a bit of flack recently but the advantage of a BBMM is they account
# for time.  More specifically, they account for the distance moved between successive relocations to better
# estimate HRs. People like them but they're not necessarily a better estimator because they account for time.

# A major issue for estimating a HR using a BBMM is time between relocations.  In a perfect world, we would have
# regular intervals and lots of locations.  But, of course, we never have that.  So, we need to restrict the 
# relocations that we use to esimtate the BBMM HR to a specific time interval.  The BBMM package makes this really
# easy, which we'll see below.  

# We can use locations from a data frame, although they must be metric so we're going to grab them from our 
# final_move object.  We're also only going to use locations from individual 114328.  We'll re-analyze the 
# BBMM home ranges with both individuals next, but it's easiest to just start with one.
final_move_df <- data.frame(final_move[which(final_move$ID=="114328"),])

# Now we estimate time lag using the diff function. We use the diff command on our time series and add 
# a 'NA' to the beginning. The BBMM package needs us to use all of our locations, including the first
# despite the fact there is no time lag associated with the first location.  
# The BBMM package uses minutes so we convert from hours to minutes.
final_move_df$lag <- c(NA, (diff(final_move_df$timestamp))*60)
head(final_move_df)

# Now we can estimate the BBMM UD.  To handle max interval lengths (ie the period between sucessive relocations
# that we feel will adequately descibe our data), we use the max.lag argument.  We are going to include all 
# successive relocations that are separated by less than or equal to 2 hours (120 minutes).
ge_bbmm <- brownian.bridge(x=final_move_df$UTM_W, y=final_move_df$UTM_N, time.lag=final_move_df$lag[-1], 
                            location.error=15, cell.size=30, max.lag = 120)
# Now we get our contours and plot them.
ge_bbmm_contours <- bbmm.contour(ge_bbmm, levels=95, locations=final_move_df[,5:6], plot=TRUE)

# Now we create the polygons to map.  Not real straightforward with this package but that's ok.  
bbmm_poly1 <- data.frame(x=ge_bbmm$x,y=ge_bbmm$y,z=ge_bbmm$probability)
bbmm_poly1.raster <- rasterFromXYZ(bbmm_poly1,crs=CRS("+proj=utm +zone=12 +datum=WGS84"),digits=2)
bbmm_poly1.raster.contour <- rasterToContour(bbmm_poly1.raster,levels=ge_bbmm_contours$Z)
bbmm_poly1.raster.contour <- spChFIDs(bbmm_poly1.raster.contour,paste(95,"% Contour Line",sep=""))

# The package creates SpatialLines and we want SpatialPolygons so we do a little conversion.
bbmm_poly_convert <- SpatialLines2PolySet(bbmm_poly1.raster.contour)
bbmm_poly <- PolySet2SpatialPolygons(bbmm_poly_convert)

# estimate area of the polygon (which is actually a number of polygons)
ur.area<-sapply(slot(bbmm_poly, "polygons"), function(x) sapply(slot(x, "Polygons"), slot, "area"))
bbmm2_km2 <- (sum(ur.area))*0.000001
bbmm2_km2

# We're not going to plot the BBMM in ggplot or leaflet because the fortify command seems to do really weird 
# things to these objects. If you want to write the object to a shapefile and check it out in a GIS, use:

# writeOGR(obj=bbmm_poly1.raster.contour,dsn=".",layer="ge_bbmm",driver="ESRI Shapefile")

################################################
# Now we'll do the BBMM estimates for both individuals.  It is going to look really similar except that 
# we are going to be using a number of apply functions in R.  These are similar to for loops but typically
# more speedy.  We don't really need the speed here but I prefer apply functions over loops.  

# Like the above example with one individual, we use final_move in data frame form.  Unlike the previous
# example, we convert our data into a list so we can use our apply functions.  The list is created with the
# the 'split' function.
final_move_df <- data.frame(final_move)
final.move.list <- split(final_move_df, list(final_move_df$ID))

# Our first apply function (lapply), we add our time lag
final.move.list <- lapply(final.move.list, function(x) {x$lag=c(NA, (diff(x$timestamp))*60)
  x
  })

# Next, we run our BBMMs for each individual
bbmm_grouped <- lapply(final.move.list, function(x) {
  brownian.bridge(x=x$UTM_W, y=x$UTM_N, time.lag=x$lag[-1], 
  location.error=15, cell.size=30, max.lag = 120)
})

# Now we estimare and plot our HRs
bbmm_grouped_contours <- mapply(x=bbmm_grouped, y=final.move.list, function(x,y) {
  bbmm.contour(x, levels=95, locations=y[,5:6] ,plot=T)
})

bbmm_grouped_contours[2,]
# Lastly, we're going to run through the steps we need to get an area estimate.  It's the same process
# as above except we're using the apply function to go through each individual.
bbmm_dfs <- lapply(bbmm_grouped, function(a){
  data.frame(x=a$x, y=a$y, z=a$probability)
})

bbmm_df.raster <- lapply(bbmm_dfs, function(x){
  rasterFromXYZ(x,crs=CRS("+proj=utm +zone=12 +datum=WGS84"),digits=2)
})

bbmm_df.raster.contour <- mapply(x=bbmm_df.raster, y=data.frame(bbmm_grouped_contours), function(x,y){
  rasterToContour(x,levels=y$Z)
})

bbmm_df.raster.contour2 <- lapply(bbmm_df.raster.contour, function(x){
  spChFIDs(x,paste(95,"% Contour Line",sep=""))
})

bbmm_poly_convert_grouped <- lapply(bbmm_df.raster.contour2, function(x){
  SpatialLines2PolySet(x)
})

bbmm_poly_grouped <- lapply(bbmm_poly_convert_grouped, function(x){
  PolySet2SpatialPolygons(x)
})  
  
ur.area.grouped <- lapply(bbmm_poly_grouped, function(a){
  sapply(slot(a, "polygons"),function(x) sapply(slot(x, "Polygons"), slot, "area"))
})

final <- lapply(ur.area.grouped, function(x){
  (sum(x))*0.000001
})

final

# Lastly, let's compare area estimates for the HRs.
data.frame(ge_hr_mcp[2,], ge_hr_kde[2,], ge_hr_alocoh_km[2,], data.frame(c("BBMM"=final[1],"BBMM"=final[2])))

######################################################################################
######################################################################################
######################################################################################
######################################################################################

# Suggested lessons (with answers provided):

# 1. Estimate the akde for the other indvidual (117408)
# 2. Estimate and compare BBMM core area contours 
# 3. Estimate and informally compare monthly space use with the estimator(s) of your choice
# 4. Go to the 'Tracking Data Map' on movebank.org and search "Movement Workshop Eagle Practice Data."
#    Download the data, prep it and estimate some home ranges.

