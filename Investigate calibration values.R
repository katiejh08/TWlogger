# Setup Environment -------------------------------------------------------

# Set the Timezone Offset from UTC 
tzOffset <- "Etc/GMT+3" # Falklands time in the winter
# Set the Working Directory to the location of this File
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(current_path)
getwd()

# Create a function to allow you to check if data exists in the dataframe
`%notin%` <- Negate(`%in%`)

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


# Plot calibration values -------------------------------------------------

# Select parent folder that contains all deployment folders
dataPath <- choose.dir(caption="Select the folder containing deployment raw data folders") 
# Select the first RData of the group you want to process
filename <- file.choose()
# Import data
pathChoice <- dirname(filename)
# This imports all RData files in the directory chosen
filenames <- list.files(path = pathChoice, pattern = "*.csv", all.files = FALSE, full.names = TRUE, recursive = FALSE, ignore.case = TRUE)

# Iteratively process deployments
for(i in 1:length(filenames)) {
  # This will tell you which file or folder is being processed (good for debugging)
  cat(paste0("\nProcessing: ",basename(filenames[i]))) 
  # Import csv
  caldata <- read_csv(filenames[i])
  caldata <- caldata[c(1,2)]
  AccPoly1 <- as.data.frame(caldata[1,1])
  AccPoly1$type <-"AccPoly1"
  AccPoly1$axis <- "AccPoly1"
  colnames(AccPoly1)[1:3] <- (c("value","type","axis"))
  AccPoly2 <- as.data.frame(caldata[,2])
  AccPoly2$type <-"AccPoly2"
  AccPoly2$axis <-c("x","y","z")
  colnames(AccPoly2)[1:3] <- (c("value","type","axis"))
  caldata <- as.data.frame(rbind(AccPoly1,AccPoly2))
  # Add calibration name
  name <- basename(filenames[i])
  name <- tools::file_path_sans_ext(name)
  caldata$model <- unique(depMeta$model[depMeta$calCross==name])
  caldata$name <- name
  caldata$name <- sapply(strsplit(name,"-"), `[`, 1)
  # Reorder columns to prepare to change column names
  caldata <- caldata[,c("name", "model","type","axis","value")]
  # Combine into Dataset
  if(i==1) dataset <- caldata else dataset <- rbind(dataset,caldata)
}  

# Plot results ------------------------------------------------------------

p1 <- dataset %>% 
  dplyr::filter(type=="AccPoly1") %>%
  ggplot() + 
  geom_jitter(aes(x=type, y=value,color=name)) + 
  facet_wrap(~model)
p1


p2 <- dataset %>% 
  dplyr::filter(type=="AccPoly2") %>%
  ggplot() + 
  geom_jitter(aes(x=axis, y=value,color=name)) + 
  facet_wrap(~model)
p2


# Interpretation ----------------------------------------------------------

# Plot AccPoly (LSM303 seems to be a factor of 4 higher, because of the scaling factor of the sensor. 
# When we changed from 2G to 8G, all values changed by a factor of 4. The important thing is they're similar within a group.)
# Because of the scaling issues, you would expect everything to be amplified in the one versus the other. 
# For the same reason there was a factor of 4 difference in AccPoly1, you're still compensating for that same thing. 
# The larger difference in values because of scaling differences. X-values in relation to y-values.