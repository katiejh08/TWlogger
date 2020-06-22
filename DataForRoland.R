
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

# Load Data ------------------------------------------------------------
setwd(savePath)
load("dataset-1Hz-VDBA-ODBA-clipVal-bitFix.RData")

# Subset data for Roland 20200410
dataset <- dataset %>%
  mutate(log10VDBA=log10(VDBA))
  select(dttz,depid,model,VDBA,log10VDBA)
save(dataset,file="dataset_for_Roland-1Hz.RData")



