
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
# Select folder that contains meta data
metaPath <- choose.dir(caption="Select the folder containing meta data") 

# Set working directory to location of meta data and load depMeta
setwd(metaPath)
load("depMeta.RData")
# Filter depMeta to include only the logger platform for which you're processing
depMeta <- depMeta %>% 
  dplyr::filter(platform=="TW") %>% 
  arrange(depid)

# Set the Working Directory to the location of this File
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(current_path)
getwd()

# Set the Timezone Offset from UTC 
tzOffset <- "Etc/GMT+3"
# Create a function to allow you to check if data exists in the dataframe
`%notin%` <- Negate(`%in%`)

# Process Data ------------------------------------------------------------

# Iteratively process deployments
for(i in 1:length(depMeta$depid)) {
  # Save working directory so you can return to it at end of for loop
  current_path <- getwd()
  # Resets working directory to deployment folder being processed
  setwd(paste0(dataPath,"/",depMeta$depid[i],"/"))
  # This will tell you which file or folder is being processed (good for debugging)
  cat(paste0("\nProcessing: ",depMeta$depid[i])) 
  # Create list of filenames to be combined
  filenames <- list.files(path = paste0(dataPath,"/",depMeta$depid[i],"/Raw"), pattern = "*.csv", 
                          all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = TRUE)
  # Specify the start and end time of deployment using depMeta file loaded earlier
  startTime <- depMeta$onBirddttz[i]
  endTime <- depMeta$offBirddttz[i]
  # Combine all raw CSVs
  accdata <- do.call("rbind",sapply(paste0(dataPath,"/",depMeta$depid[i],"/Raw/",filenames), read.csv, simplify = FALSE))
  # NOTE: do.call uses read.csv. To trouble shoot, copy paste0(dataPath,"/",depMeta$depid[i],"/Raw/",filenames)
  # into console and then load each file one by one to see which causes issue
  # Add depid (aka deployment ID)
  accdata$name <- depMeta$depid[i]
  # Create proper datetime objects
  colnames(accdata)[1:2] <- c("date","time") # Rename columns to simplify
  # Create dt (datetime) column in GMT
  accdata$dt <- as.POSIXct(paste(accdata$date, accdata$time), format="%m/%d/%Y %H:%M:%S", tz=depMeta$tzOffset[i])
  attr(accdata$dt, "tzone") # Check tz
  attr(accdata$dt,"tzone") <- 'GMT'  # time will change
  # Convert GMT to local time zone of deployment (dttz)
  accdata$dttz <- accdata$dt
  attr(accdata$dttz, "tzone") <- tzOffset  # time will change; tzOffset is defined above
  # Create deployment year, because tag versions 1 and 2 need to be processed separately for the next steps
  depYear <- year(accdata$dt[1])
  if(depYear==2017){
    # Reorder columns to prepare to change column names
    accdata <- accdata[,c("name","dt","dttz","Timestamp.Ms.",
                          "ACCELX","ACCELY","ACCELZ","MAGX","MAGY","MAGZ")]
    # Change column names to more practical shorter names
    colnames(accdata)[1:10] <- c("name","dt","dttz","ts","ax","ay","az","mx","my","mz")
  }else if(depYear>2017){
    # Reorder columns to prepare to change column names
    accdata <- accdata[,c("name", "dt","dttz","Timestamp.Ms.",
                          "Temp.Raw.","ACCELX","ACCELY","ACCELZ","MAGX","MAGY","MAGZ","Sats",
                          "HDOP","Latitude","Longitude","FixAge","DateUTC","TimeUTC","DateAge","Altitude","Course","Speed")]
    # Change column names to more practical shorter names
    colnames(accdata)[1:22] <- c("name","dt","dttz","ts","temp","ax","ay","az","mx","my","mz",
                                 "Sats","HDOP","Lat","Long","FixAge","DateUTC","TimeUTC","DateAge","Altitude","Course","Speed")
  }

  rownames(accdata) <- c()
  
  # Run subset() function to extract data for the selected timerange (e.g. remove pre-deployment data)
  accdata <- subset(accdata, accdata$dttz >= startTime & accdata$dttz <= endTime)
  cat(paste0("\nDeployment length: ",round(max(accdata$dttz)-min(accdata$dttz),2)))

  rm(filenames)

# Extract GPS -------------------------------------------------------------
    
    # Check if tag logged GPS data
    if("Lat" %in% colnames(accdata)){
    # Extract fields related to GPS
    gpsData <- accdata[,c("dttz","dt","Sats","HDOP","Lat","Long","FixAge","DateUTC","TimeUTC","DateAge","Altitude","Course","Speed")]
    # Remove blank lines
    gpsData <- gpsData[!is.na(gpsData["Sats"]),]
    # Confirm there is GPS data before moving forward
    if(dim(gpsData)[1]!=0){
    # Convert values
    gpsData$Lat <- gpsData$Lat/1000000 # TWLogger records in an integer version of decimal degrees. Dividing by 1000000 creates correct decimal degree value
    gpsData$Long <- gpsData$Long/1000000
    # Remove obvious bad hits
    gpsData <- gpsData %>% 
      arrange(dttz) %>% 
      dplyr::filter(between(Lat,-90,90),
                    Sats > 2,
                    between(Long, -180,180),
                    Lat != 0, Long != 0)
    tryCatch({
      #filter by speed and angle
      gpsData <- gpsData %>% 
        dplyr::filter(sdafilter(Lat,Long,dt,rep(3,length(gpsData$Lat)), vmax = 30, ang = c(15, 25), distlim = c(2500, 5000))!= "removed")}, 
      # warning = function(war) {
      #   # warning handler picks up where error was generated
      #   print(paste("WARNING:  ",war))},
      error=function(err){
        print(paste("SDA Filter Error:  ",err,", using Distance-only Filter instead"))
      }, finally = {
        # filter by speed only (30 m/s)
        gpsData <- gpsData %>% 
          dplyr::filter(vmask(Lat,Long,dt, vmax = 30)!= "removed")
      }) # END tryCatch
    
    # Create proper datetime objects from GPS datetimes (convert GMT to local)
    gpsData$dtGPS <- as.POSIXct(paste(gpsData$DateUTC, gpsData$TimeUTC), format="%m/%d/%Y %H:%M:%S", tz='GMT')
    # attr(gpsData$dtGPS, "tzone") # check that dt is in GMT
    gpsData$dttzGPS <- gpsData$dtGPS # set dttz to dt
    attr(gpsData$dttzGPS, "tzone") <- tzOffset # change the timezone to tzOffset
    # attr(gpsData$dttzGPS, "tzone") # check that dttz is in local time
    # check to make sure data is reasonably located
    plot(gpsData$Long,gpsData$Lat)
    rownames(gpsData) <- c()
    # Save GPS as .RData file
    save(gpsData, file=paste0(depMeta$depid[i],"-GPSData.RData"))
    rm(gpsData)
    }else{cat(paste0("\nNo GPS data found for ",depMeta$depid[i]))}  
  }
  

# Rediscretize ------------------------------------------------------------

  # Create temp column if it doesn't exist
  if("temp" %notin% colnames(accdata)){ # This uses function defined above when setting up the environment
    accdata$temp <- as.numeric(NA)
  }
  # Define sampling rate. Confirm the sampling rate you define aligns with the programmed frequency.
  resampleRate = 50
  # Select only the columns related to acc data (i.e., remove GPS fields) and create new dataframe
  data <- accdata[, c("name","ts","temp","ax","ay","az","mx","my","mz","dt","dttz")] 
  # Create a dataframe with period and frequency 
  data2 <- data %>%
    # seconds since the beginning
    dplyr::mutate(secs_since = as.numeric(dttz - min(dttz))) %>% 
    # Filter out first and last seconds because they're partial
    dplyr::filter(secs_since > 0,
                  secs_since < max(secs_since)) %>% 
    # reset seconds since the beginning (could just subtract 1?)
    dplyr::mutate(secs_since = secs_since-1) %>%
    #mutate(secs_since = as.numeric(dttz - min(dttz))) %>%
    # group into within-seconds blocks
    dplyr::group_by(secs_since) %>%
    # frequency and period of sampling
    dplyr::mutate(freq = n(),
                  period = 1 / resampleRate,
                  # fraction of a second since beginning of second (i.e. 0-1)
                  frac_sec = (row_number() - 1) / resampleRate,
                  # seconds since beginning (e.g. 9.456)
                  true_since = secs_since + frac_sec) %>%
    ungroup %>%
    # Remove any greater than resampleRate 
    dplyr::filter(frac_sec<=.98) %>%
    # true time down to fractional second (e.g. 2018-06-07 16:57:12.1234)
    dplyr::mutate(true_time = min(dttz) + true_since,
                  tsDif = c(0, diff(ts)))
  #create a dataframe with regular sampling
  data3 <- data.frame(true_time = seq(from = min(data2$true_time),
                                      to = max(data2$true_time),
                                      by = 1 / resampleRate)) %>%
    merge(data2,all=TRUE) #Merge with data2 (fills unmatched with NA)
  
  #fill name into newly created NA rows
  data3$name <- data3$name[1]
  
  data3 <- data3[, c("true_time", "name","ts","temp","ax","ay","az","mx","my","mz","secs_since","true_since")]
  colnames(data3)[1] <- c("dttz")
  colnames(data3)[2] <-c("depid")
  
  data3$temp <- na.fill(na.approx(data3$temp, data3$dttz, na.rm = FALSE),"extend")
  data3$ax <- na.fill(na.approx(data3$ax, data3$dttz, na.rm = FALSE),"extend")
  data3$ay <- na.fill(na.approx(data3$ay, data3$dttz, na.rm = FALSE),"extend")
  data3$az <- na.fill(na.approx(data3$az, data3$dttz, na.rm = FALSE),"extend")
  data3$mx <- na.fill(na.approx(data3$mx, data3$dttz, na.rm = FALSE),"extend")
  data3$my <- na.fill(na.approx(data3$my, data3$dttz, na.rm = FALSE),"extend")
  data3$mz <- na.fill(na.approx(data3$mz, data3$dttz, na.rm = FALSE),"extend")
  data3$true_since <- na.fill(na.approx(data3$true_since, data3$dttz, na.rm = FALSE),"extend")
  
  data <- data3
  
  # Save as .RData file
  save(data, file=paste0(depMeta$depid[i],"_onBird-50Hz-noCal.RData"))
    
  # Clean global environment
  rm(accdata,data,data2,data3,resampleRate)
  
  setwd(current_path)
  rm(current_path)
  
}
