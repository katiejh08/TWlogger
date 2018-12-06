library(tidyverse)
# Import file
# Specify the column type when importing using read_csv: check here (http://r4ds.had.co.nz/import.html)
filename <- file.choose()
data <- read_csv(filename,
                 col_types = cols(
                   dttz = col_datetime(),
                   name = col_character(),
                   ts = col_double(),
                   temp = col_double(),
                   Ax = col_double(),
                   Ay = col_double(),
                   Az = col_double(),
                   Mx = col_double(),
                   My = col_double(),
                   Mz = col_double(),
                   freq = col_double(),
                   secs_since = col_double(),
                   true_since = col_double(),
                   tsDiff = col_double()))
str(data)
head(data)
# Create date and time columns
data$date <- as.Date(data$dttz)
str(data$date)
data$time <- format(data$dttz,format = "%H:%M:%S") 
# data$time <- format(data$dttz,"%H:%M:%S")
# str(data$time)
# data$time <- strptime(data$dttz, format="%H:%M:%S", tz = "GMT")
# data$time <- strftime(data$dttz, format="%H:%M:%S", tz = "GMT")
# data$time <- as.POSIXct(format(data$dttz), format="%H:%M:%S")
str(data$time)
head(data$time)
# Re-order data
data <- data[,c("date", "time","Ax","Ay",
                "Az","Mx","My","Mz")]
head(data)
depid <- basename(filename)
depid <- strsplit(depid,'-')
deploymentName <- depid[[1]][(length(depid[[1]]))-1]
write.csv(data, file=paste0(deploymentName,"-fw4.csv"),row.names=FALSE)
