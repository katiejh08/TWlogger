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
pkgTest("zoo")
pkgTest("dplyr")
pkgTest("tagtools")
pkgTest("plotly")
pkgTest("RcppRoll")
pkgTest("car")
pkgTest("gridExtra")
pkgTest("signal")
pkgTest("ggpubr")

# Select parent folder that contains all deployment folders
dataPath <- choose.dir(caption="Select the folder containing deployment raw data folders") 
# Select folder that contains meta data
metaPath <- choose.dir(caption="Select the folder containing meta data") 
# Select folder that contains calibration files
calPath <- choose.dir(caption="Select the folder containing calibration files")
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

# Load clip values to control for tag versioning
load("sumClipVals303.RData")
load("sumClipVals9DS0.RData")

# Set the Timezone Offset from UTC 
tzOffset <- "Etc/GMT+3"

# Process Data ------------------------------------------------------------

# Iteratively process deployments
for(i in 1:length(depMeta$depid)) {
  # Save working directory so you can return to it at end of for loop
  current_path <- getwd()
  # Resets working directory to deployment folder being processed
  setwd(paste0(dataPath,"/",depMeta$depid[i],"/"))
  # This will tell you which file or folder is being processed (good for debugging)
  cat(paste0("\nProcessing: ",depMeta$depid[i])) 
  load(paste0(dataPath,"/",depMeta$depid[i],"/",depMeta$depid[i],"_onBird-50Hz-noCal.RData"))
  depid <- depMeta$depid[i]
  model <- depMeta$model[i]

  # Bit fix -----------------------------------------------------------------
  
  # Deal with "least significant bit"
  if(model=="LSM9DS0"){
    data$ax <- (data$ax * 0.061) / 1000
    data$ay <- (data$ay * 0.061) / 1000
    data$az <- (data$az * 0.061) / 1000
  }else{
    data$ax <- (data$ax * 0.00025)
    data$ay <- (data$ay * 0.00025)
    data$az <- (data$az * 0.00025)
  }
  
  # Apply benchCal values ---------------------------------------------------
  
  # Define sampling rate  
  fs <- 50 
  # Transform axes to North East Up sensor orientation used in Tag Tools
  # Acc	 [x -y z]
  data$axT <- data$ax * (1.0)
  data$ayT <- data$ay * (-1.0)
  data$azT <- data$az * (1.0)
  # Create a matrix with Acc data
  At <- cbind(data$axT,data$ayT,data$azT)
  # Check for NA
  sum(is.na(At))
  # If NAs, remove using linear approximation
  if(sum(is.na(At)>0)){
    Atnarm <- At
    Atnarm <- na.approx(Atnarm, na.rm = FALSE)
  } else {
    Atnarm <- At
  }
  # Check again for NAs
  sum(is.na(Atnarm))
  # Transform axes of Mag to orientation used in Tag Tools
  data$mxT <- data$mx * 1.0
  data$myT <- data$my * (-1.0)
  data$mzT <- data$mz * 1.0
  # Create a matrix with Mag data
  Mt <- cbind(data$mxT,data$myT,data$mzT)
  # Check for NA
  sum(is.na(Mt))
  # If NAs, remove using linear approximation
  if(sum(is.na(Mt))>0){
    Mtnarm <- Mt
    Mtnarm <- na.approx(Mtnarm, na.rm = FALSE)
  } else {
    Mtnarm <- Mt
  }
  # Check again for NAs
  sum(is.na(Mtnarm))
  # Import calibration file for logger
  # This will tell you which calibration file is being used
  cat(paste0("\nSelect calibration file named: ",depMeta$calCrossBitFix[depMeta$depid==depid][1]))
  cal <- read_csv(paste0(calPath,"/",depMeta$calCrossBitFix[i],".csv"))
  AccCal <- list(poly = cbind(cal$AccPoly1,cal$AccPoly2), cross = cbind(cal$AccCross1,cal$AccCross2,cal$AccCross3))
  MagCal <- list(poly = cbind(cal$MagPoly1,cal$MagPoly2), cross = cbind(cal$MagCross1,cal$MagCross2,cal$MagCross3))
  AtCal <- apply_cal(Atnarm,cal = AccCal, T = NULL)
  MtCal <- apply_cal(Mtnarm,cal = MagCal, T = NULL)
  AMcheck <- check_AM(A=AtCal,M=MtCal,fs=fs) # Check field strengths
  # If it's a good calibration the mean should be around 9.81 ms2-
  median<-median(AMcheck$fstr[,1]) # Checks acc field strength (column 1)
  mean<-mean(AMcheck$fstr[,1]) # Checks acc field strength (column 1) # This could be higher than 10 because it's not static only. Dynamic is also included.
  tempx <- data.frame("model" = model,"depid" = depid, "mean" = mean, "median" = median)
  tempx$depid <-as.character(depid)
  # Combine meanfstr into one dataset for later comparison
  if(i==1){ 
    checkMean <- tempx 
    }else{
      checkMean<- rbind(checkMean,tempx)
    }
  # Plot acc field strength
  p <- ggplot()+
    geom_histogram(aes(AMcheck$fstr[,1]),binwidth = 0.01) +
    geom_vline(aes(xintercept=9.81),colour='red') +
    scale_y_continuous(expand = c(0, 500000)) +
    scale_x_continuous(breaks = seq(9,11, 0.2), limits = c(9,11), expand = c(0, 0)) +
    theme_classic() +
    labs(x = "Acc Field Strength (ms-2)",
         y = NULL,
         title = paste0(depid,": ",round(mean,2)," mean"))
  # save plots as .png
  ggsave(p, path = paste0(dataPath,"/",depMeta$depid[i],"/"), file=paste0(depid,"-AccFieldStrength-bitFix.png"))
  
  # Save Calibrated Data Back To Dataframe
  benchCal <-cbind(data[,1:4],AtCal,MtCal,data[12:14])
  # Rename Columns
  colnames(benchCal) <- c("dttz","depid","ts","temp","Ax","Ay","Az",
                       "Mx","My","Mz","secs_since","true_since","tsDif")
  save(benchCal, file=paste0(depid,"_onBird-50Hz-benchCal-bitFix.RData"))
  data <- benchCal
  # Clean up environment
  rm(cal,At,Atnarm,AtCal,AccCal,Mt,Mtnarm,MtCal,MagCal,AMcheck,benchCal,p)
  
  # Subset data to 24hr ----------------------------------------------------------
  
  data <- data %>% arrange(dttz)
  data <- subset(data, data$dttz >= depMeta$startTime[i] & data$dttz < depMeta$endTime[i])
  # Check to see if it worked
  difftime(max(data$dttz),min(data$dttz),units="hours")
  # Save as .RData file
  save(data, file=paste0(depid,"-50Hz-benchCal-bitFix-24hr.RData"))

  # Clip vals for version control -------------------------------------------
  
  if(model=="LSM9DS0"){
    # Process LSM9DS0 deployments
    # Identify positive and negative values 
    data <- data %>% 
      mutate(pos_Ax = ifelse(Ax>0,1,0),
             pos_Ay = ifelse(Ay>0,1,0),
             pos_Az = ifelse(Az>0,1,0))
    # Use LSM303 clipVals to clip "inner" values of each vector
    # Clip minimum (magnitude) values of each vector (i.e., "inners")
    data$Ax[data$Ax >0 & data$Ax < sumClipVals303$min_pos_Ax[1]] <- sumClipVals303$min_pos_Ax[1]
    data$Ax[data$Ax <0 & data$Ax > sumClipVals303$min_neg_Ax[1]] <- sumClipVals303$min_neg_Ax[1]
    
    data$Ay[data$Ay >0 & data$Ay < sumClipVals303$min_pos_Ay[1]] <- sumClipVals303$min_pos_Ay[1]
    data$Ay[data$Ay <0 & data$Ay > sumClipVals303$min_neg_Ay[1]] <- sumClipVals303$min_neg_Ay[1]
    
    data$Az[data$Az >0 & data$Az < sumClipVals303$min_pos_Az[1]] <- sumClipVals303$min_pos_Az[1]
    data$Az[data$Az <0 & data$Az > sumClipVals303$min_neg_Az[1]] <- sumClipVals303$min_neg_Az[1]
    
    # Round axes to match the sampling precision of LSM303
    data$Ax <- round(data$Ax,6)
    data$Ay <- round(data$Ay,6)
    data$Az <- round(data$Az,6)
    
  }else{
    # Process LSM303 deployments
    # Clip maximum (magnitude) values of each vector (i.e., outers)
    data$Ax[data$Ax>sumClipVals9DS0$max_pos_Ax[1]] <- sumClipVals9DS0$max_pos_Ax[1]
    data$Ax[data$Ax<sumClipVals9DS0$max_neg_Ax[1]] <- sumClipVals9DS0$max_neg_Ax[1]
    
    data$Ay[data$Ay>sumClipVals9DS0$max_pos_Ay[1]] <- sumClipVals9DS0$max_pos_Ay[1]
    data$Ay[data$Ay<sumClipVals9DS0$max_neg_Ay[1]] <- sumClipVals9DS0$max_neg_Ay[1]      
    
    data$Az[data$Az>sumClipVals9DS0$max_pos_Az[1]] <- sumClipVals9DS0$max_pos_Az[1]
    data$Az[data$Az<sumClipVals9DS0$max_neg_Az[1]] <- sumClipVals9DS0$max_neg_Az[1]
    
    # Round axes to match the sampling capacity (e.g. +/- 2G) of LSM9DS0
    data$Ax <- round(data$Ax,6)
    data$Ay <- round(data$Ay,6)
    data$Az <- round(data$Az,6)
  }
  
  # Save as .RData
  save(data, file=paste0(depid,"-50Hz-benchCal-bitFix-24hr-clipVal.RData"))
  
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
  
  # # Confirm time bins were properly created
  # temp <- data %>%
  #   group_by(bin) %>%
  #   dplyr::mutate(freq = n()) %>%
  #   ungroup %>%
  #   {table(.$freq)} -> freqCount
  # # Count of number of occurrances of each freq
  # freqCount / as.numeric(names(freqCount))
  # rm(temp,freqCount)
  
  # Calculate mean of each axis (i.e., down sample from 50 to 10Hz)
  data <- data %>% 
    group_by(bin) %>% 
    summarize(dttz=first(dttz),
              Ax = mean(Ax),
              Ay = mean(Ay),
              Az = mean(Az))
  
  sapply(data, function(x) sum(is.na(x)))
  data$bin<-NULL
  
  # Calculate VDBA ----------------------------------------------------------
  
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
  save(data, file=paste0(depid,"-10Hz-VDBA-ODBA-clipVal-bitFix.RData"))
  
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
  save(data, file=paste0(depid,"-1Hz-VDBA-ODBA-clipVal-bitFix.RData"))
  setwd(current_path)
  rm(current_path)
}

# Save acc field strength means
setwd(savePath)
write.csv(checkMean, file="bitFix_meanAccFstr.csv", row.names = FALSE)
