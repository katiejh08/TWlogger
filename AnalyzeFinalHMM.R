library(dplyr)
library(maptools)

setwd("~/Projects/R/TWlogger")
tzOffset <-"Etc/GMT+3"

# Load R workspace
load("~/Projects/R/TWlogger/data_for_Katie.RData")

# Combine list into one dataframe
data <-bind_rows(Amag_rollmean_states)

########################################
####    Calculate Sunrise/Sunset    ####
########################################

## Create Spatial Points object of Sakhalvasho coordinates
long <- rep(-60.09, times=nrow(data))
lat <- rep(-51.37, times=nrow(data))
data$long <- long
data$lat <- lat

# OR leave as list and add columns using mapply
# However, when I tried bind_rows after mapply, I lost the newly added long/lat variables
#data <- mapply(cbind, Amag_rollmean_states, "long"=-60.09, "lat"=-51.37,SIMPLIFY=F)

# Check dttz. Dttz has not been retaining time zone when writing to CSV  
attr(data$dttz_down, "tzone") #Check tz
attr(data$dttz_down, "tzone") <- tzOffset

# Run the sunrise sunset script
source(SunriseSunsetTimesCrepuscular.r)

########################################
####   Calculate Activity Budget    ####
########################################
# Subset by individual (OR maintain list from start and run list through
# for loop to calculate srise and sset; that way birds are still individual dataframes)
# Try first with one individual
datax <- subset(data,data$ID == "X36")

# OPTION 1: Calculate proportion active vs rest per bird
# if(data$night=="day"){
#   high <- count(data$state_classif==4)
#   low <- count(data$state_classif==3)
#   rest <- count(data$state_classif<3)
# } 

# OPTION 2: Show a table with the frequency of states
X36 %>%
  # group into day and night
  group_by(night) %>%
  # states
  dplyr::mutate(state_classif = n()) %>%
  ungroup %>%
  {table(.$state_classif)} -> stateCount
# Count of number of occurrances of each state
stateCount / as.numeric(names(stateCount))
# Percentage of total of each state
format((stateCount / as.numeric(names(stateCount)))/sum((stateCount / as.numeric(names(stateCount)))),scientific=FALSE)

# Show a table with the frequency of states
# I think this gives frequency per second
X36$state_classif %>% table -> stateCount
stateCount / as.numeric(names(stateCount))

# Control for day length
# First calculate day length (in seconds)
dlength <- difftime(data$srise,data$sset,tz=tzOffset, units="sec")*-1
dlengthX36 <- difftime(X36$srise,X36$sset,tz=tzOffset, units="sec")*-1
hist(as.numeric(dlengthX36))

# Divide by day length to normalize
stateCount <- as.data.frame(stateCount)
stateCount$dlength <-cbind(stateCount$freq,dlength)
stateDayNorm

########################################
####        Plot HMM states         ####
########################################