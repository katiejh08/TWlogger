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

# Set the Timezone Offset from UTC 
tzOffset <- "Etc/GMT+3" # Falklands time in the winter
# Set the Working Directory to the location of this File
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(current_path)
getwd()

# Create a function to allow you to check if data exists in the dataframe
`%notin%` <- Negate(`%in%`)

# Load depMeta
load("depMeta.RData")
depMeta <- depMeta %>% 
  dplyr::filter(platform=="TW",
                island=="SDI")

# Select parent folder that contains all deployment folders
dataPath <- choose.dir(caption="Select the folder containing deployment raw data folders") 

# Process Data ------------------------------------------------------------

# Iteratively process deployments
for(i in 1:length(depMeta$depid)) {
  # Save working directory so you can return to it at end of for loop
  current_path <- getwd()
  # Resets working directory to deployment folder being processed
  setwd(paste0(dataPath,"/",depMeta$depid[i],"/"))
  # This will tell you which file or folder is being processed (good for debugging)
  cat(paste0("\nProcessing: ",depMeta$depid[i])) 
  load(paste0(dataPath,"/",depMeta$depid[i],"/",depMeta$depid[i],"-50Hz-benchCal-24hr-clipVal.RData"))
  depid <- data$depid[1]
  
  # Down sample data to 10Hz ------------------------------------------------
  
  # Define sampling rate  
  fs <- 50 
  data <- subset(data, select = -c(ts,temp,Mx,My,Mz,secs_since,true_since,tsDif))
  # Reorder columns
  data <- data[,c("depid","dttz","Ax","Ay","Az")]
  data <- data %>% 
    dplyr::mutate(secs_since = floor(as.numeric(dttz - min(dttz)))) %>% # seconds since start of deployment View
    dplyr::mutate(frac_sec = (row_number() - 1) / fs, # seconds since beginning (e.g. 9.4) 
                  dec = round(frac_sec%%1,2)) %>% # extracts decimal
    ungroup %>%
    dplyr::mutate(bin_10hz = ifelse(dec<0.1,0,
                                    ifelse(dec>=0.1 & dec<0.2,1,
                                           ifelse(dec>=0.2 & dec<0.3,2,
                                                  ifelse(dec>=0.3 & dec<0.4,3,
                                                         ifelse(dec>=0.4 & dec<0.5,4,
                                                                ifelse(dec>=0.5 & dec<0.6,5,
                                                                       ifelse(dec>=0.6 & dec<0.7,6,
                                                                              ifelse(dec>=0.7 & dec<0.8,7,
                                                                                     ifelse(dec>=0.8 & dec<0.9,8,
                                                                                            ifelse(dec>=0.9 & dec<1,9,10)))))))))),
                  bin = paste0(secs_since,"-",bin_10hz))
  
  # Calculate mean of each axis (i.e., down sample from 50 to 10Hz)
  data <- data %>% 
    group_by(bin) %>% 
    summarize(dttz=first(dttz),
              Ax = mean(Ax),
              Ay = mean(Ay),
              Az = mean(Az))
  
  sapply(data, function(x) sum(is.na(x)))
  data$bin<-NULL
  
  # Calculate VDBA & ODBA ----------------------------------------------------------
  
  # Redefine fs
  fs <- 10
  # Calculate static (running mean)
  windowSize=fs*3 # 2-sec window 
  data <- data %>% 
    arrange(dttz) %>% 
    mutate(static_Ax=roll_mean(Ax,windowSize,fill=NA),
           static_Ay=roll_mean(Ay,windowSize,fill=NA),
           static_Az=roll_mean(Az,windowSize,fill=NA)) %>%
    ungroup
  # Replace the NA's at the beginning with the first good value
  data <- data %>%
    arrange(dttz) %>%
    mutate(naUp = ifelse(row_number()<=windowSize/2,1,0),
           naDown = ifelse(row_number() > n() - (windowSize/2+1),1,0)) %>% #View
    group_by(naUp) %>% 
    mutate(static_Ax = ifelse(naUp == 1,last(static_Ax),static_Ax),
           static_Ay = ifelse(naUp == 1,last(static_Ay),static_Ay),
           static_Az = ifelse(naUp == 1,last(static_Az),static_Az)) %>% 
    ungroup %>% 
    group_by(naDown) %>% 
    mutate(static_Ax = ifelse(naDown == 1,first(static_Ax),static_Ax),
           static_Ay = ifelse(naDown == 1,first(static_Ay),static_Ay),
           static_Az = ifelse(naDown == 1,first(static_Az),static_Az)) %>% 
    ungroup
  data$naUp <- NULL
  data$naDown <- NULL
  # Calculate dynamic (raw-static) and VDBA (sum abs dynamic)
  data <- data %>%
    arrange(dttz) %>%
    mutate(dyn_Ax=Ax-static_Ax,
           dyn_Ay=Ay-static_Ay,
           dyn_Az=Az-static_Az,
           VDBA=sqrt(dyn_Ax^2+dyn_Ay^2+dyn_Az^2),
           ODBA=abs(dyn_Ax)+abs(dyn_Ay)+abs(dyn_Az))
  data$VDBA <- data$VDBA/9.81
  data$ODBA <- data$ODBA/9.81
  data <- subset(data, select = -c(static_Ax,static_Ay,static_Az,dyn_Ax,dyn_Ay,dyn_Az))
  # Add depid back to data
  data$depid <- depid
  
  # Save as .RData file
  save(data, file=paste0(depid,"-10Hz-VDBA-ODBA-clipVal.RData"))
  
  # Down sample to 1Hz ------------------------------------------------------
  data <- data %>% 
    dplyr::mutate(secs_since = floor(as.numeric(dttz - min(dttz)))) # seconds since start of deployment
  # Calculate mean VDBA (down sample from 50 to 1Hz)
  data <- data %>% 
    group_by(secs_since) %>%
    summarize(dttz=first(dttz),
              Ax=mean(Ax),
              Ay=mean(Ay),
              Az=mean(Az),
              VDBA = mean(VDBA),
              ODBA = mean(ODBA))
  data$secs_since <- NULL
  
  # Add depid back to data
  data$depid <- depid
  # Reorder columns
  data <- data[,c("depid","dttz","Ax","Ay","Az","VDBA","ODBA")]
  
  # Save as .RData file
  save(data, file=paste0(depid,"-1Hz-VDBA-ODBA-clipVal.RData"))
  setwd(current_path)
  rm(current_path)
}
