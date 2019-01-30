library(plyr)

setwd("~/Projects/R/TWlogger")
tzOffset <-"Etc/GMT+3"

# Select the first CSV of the group you want to apply function
filename <- file.choose("Select the first CSV of the group")
pathChoice <- dirname(filename)

# This imports all CSV files in the directory chosen
filenames <- list.files(path = pathChoice, pattern = "*.csv", 
                        all.files = FALSE, full.names = FALSE, 
                        recursive = FALSE, ignore.case = TRUE)
import.list <- llply(paste(pathChoice,"/",filenames,sep = ""), read.csv)
down <- do.call("rbind", sapply(paste(pathChoice,"/",filenames,sep = ""), read.csv, simplify = FALSE))

# Order of functions to be applied
attr(data$dttz, "tzone") <- tzOffset # Correct tz from UTC to GMT-3
depid <- basename(filename) # Create deployment ID
depid <- strsplit(depid,'-')
depid <- depid[[1]][1]
data2 <- data[,c("dttz","true_since","Ax","Ay","Az")] # Remove unwanted vectors
df <- 10 # Set decimation factor df
fs <- 10 # Set original sampling rate
dttz <- data2$dttz
a <- dttz
dttz_down <- a[seq(1, length(a), df)] # For datetime select every nth value
true_since <- data2$true_since
a <- true_since
true_since_down <- a[seq(1, length(a), df)] # For true_since select every nth value
Ax <- data2$Ax
Ay <- data2$Ay
Az <- data2$Az
Ax_mat <- matrix(Ax,ncol=1) # Convert vectors to numeric matrix
Ay_mat <- matrix(Ay,ncol=1)
Az_mat <- matrix(Az,ncol=1)
Ax_down <- decdc(Ax_mat,df)
Ay_down <- decdc(Ay_mat,df)
Az_down <- decdc(Az_mat,df)
data2_down <- cbind.data.frame(dttz_down,true_since_down,Ax_down,Ay_down,Az_down) # Combine down sampled vectors into one dataframe
down <-data2_down
depid2 <- strsplit(depid,'_')
depid2[[1]][3]
down$ID <- depid2[[1]][3] # Add bird ID
fs = fs/df # Reset sampling rate to down sampled rate
fs # Confirm sampling rate matches decimated data
sw <- fs # Specify sampling window (if equivalent to fs, then 1s window)
Ax <-down$Ax
Ay <-down$Ay
Az <-down$Az
Ax2 <-Ax^2
Ay2 <-Ay^2
Az2 <-Az^2
Amag <- Ax2 + Ay2 + Az2
Amag <-sqrt(Amag) # Calculate magnitude of acceleration
Amag_rollmean <- roll_mean(Amag,fs,fill=NA)
Amag_rollmean[seq(1,24)] <- Amag_rollmean[25]
Amag_rollmean[seq(length(Amag_rollmean)-24,length(Amag_rollmean))] <- Amag_rollmean[length(Amag_rollmean)-25] # Running mean of magnitude of acceleration
A <- cbind(down$Ax_down,down$Ay_down,down$Az_down)
odba <- odba(A, sampling_rate = fs,method="wilson",n = sw) # Calculate ODBA
startTime <- as.POSIXct(strptime("2018-07-8 18:30:00",format="%Y-%m-%d %H:%M:%S"),tz=tzOffset)
endTime <- as.POSIXct(strptime("2018-07-09 18:30:00",format="%Y-%m-%d %H:%M:%S"),tz=tzOffset)
down <- subset(down, down$dttz_down >= startTime & down$dttz_down <= endTime)

# Check to see if worked
str(down)
#rm(pathChoice)
#rm(filenames)
#rm(import.list)

# # Sample for loop below
# files <- list.files(path="path/to/dir", pattern="*.txt", full.names=TRUE, recursive=FALSE)
# lapply(files, function(x) {
#   t <- read.table(x, header=TRUE) # load file
#   # apply function
#   out <- function(t)
#     # write to file
#     write.table(out, "path/to/output", sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE)
# })