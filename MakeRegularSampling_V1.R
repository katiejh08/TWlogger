library(tidyverse)
library(zoo)
setwd("/Users/jamesfahlbusch/Documents/Projects/R/TWlogger")
tzOffset <- "Etc/GMT+3" # Falklands time in the winter
filename <- file.choose()
# Load the Combined Data File
accdata <- read_csv(filename)

#  assumes that data has already been processed as accData
data <- accdata[, c("name","ts","temp","ax","ay","az","mx","my","mz","dt","dttz")]

# show a table with the frequency of frequencies
data %>%
  # seconds since the beginning
  mutate(secs_since = as.numeric(dttz - min(dttz))) %>%
  # group into within-seconds blocks
  group_by(secs_since) %>%
  # frequency and period of sampling
  dplyr::mutate(freq = n()) %>%
  ungroup %>%
  {table(.$freq)} -> freqCount
# Count of number of occurrances of each freq
freqCount / as.numeric(names(freqCount))
# Percentage of total of each freq
format((freqCount / as.numeric(names(freqCount)))/sum((freqCount / as.numeric(names(freqCount)))),scientific=FALSE)
# Find the actual number of samples that will be interpolated
#JAF Todo


#Make sure the sampling rate you choose aligns with the frequency
resampleRate = 50





#makeRegularWGPS <- function(filenameA, resampleRate){
  # Create a dataframe with period and frequency 
  data2 <- data %>%
    # seconds since the beginning
    mutate(secs_since = as.numeric(dttz - min(dttz))) %>%
    # Filter out first and last seconds because they're partial
    filter(secs_since > 0,
           secs_since < max(secs_since)) %>%
    # reset seconds since the beginning (could just subtract 1?)
    mutate(secs_since = secs_since-1) %>%
    #mutate(secs_since = as.numeric(dttz - min(dttz))) %>%
    # group into within-seconds blocks
    group_by(secs_since) %>%
    # frequency and period of sampling
    dplyr::mutate(freq = n(),
                  period = 1 / resampleRate,
                  # fraction of a second since beginning of second (i.e. 0-1)
                  frac_sec = (row_number() - 1) / resampleRate,
                  # seconds since beginning (e.g. 9.456)
                  true_since = secs_since + frac_sec) %>%
    ungroup %>%
    # Remove any greater than resampleRate 
    filter(frac_sec<=.98) %>%
    # true time down to fractional second (e.g. 2018-06-07 16:57:12.1234)
    mutate(true_time = min(dttz) + true_since,
           tsDif = c(0, diff(ts)))
  
  # show a table with the frequency of frequencies
  data2$freq %>% table -> freqCount
  freqCount / as.numeric(names(freqCount))
  
  #create a dataframe with regular sampling
  data3 <- data.frame(true_time = seq(from = min(data2$true_time),
                                 to = max(data2$true_time),
                                 by = 1 / resampleRate)) %>%
    merge(data2,all=TRUE) #Merge with data2 (fills unmatched with NA)
  
  #fill name into Newly created NA rows
  data3$name <- data3$name[1]
  
  data3 <- data3[, c("true_time", "name","ts","temp","ax","ay","az","mx","my","mz","freq","secs_since","true_since", "tsDif")]
  colnames(data3)[1] <- c("dttz")
  
  data3$temp <- na.fill(na.approx(data3$temp, data3$dttz, na.rm = FALSE),"extend")
  data3$ax <- na.fill(na.approx(data3$ax, data3$dttz, na.rm = FALSE),"extend")
  data3$ay <- na.fill(na.approx(data3$ay, data3$dttz, na.rm = FALSE),"extend")
  data3$az <- na.fill(na.approx(data3$az, data3$dttz, na.rm = FALSE),"extend")
  data3$mx <- na.fill(na.approx(data3$mx, data3$dttz, na.rm = FALSE),"extend")
  data3$my <- na.fill(na.approx(data3$my, data3$dttz, na.rm = FALSE),"extend")
  data3$mz <- na.fill(na.approx(data3$mz, data3$dttz, na.rm = FALSE),"extend")
  data3$true_since <- na.fill(na.approx(data3$true_since, data3$dttz, na.rm = FALSE),"extend")
  
  #simple plot
  # library(ggplot2)
  # ggplot() +
  #   geom_line(data = data3, aes(x = dttz, y = ax,color = 'AX')) +
  #   geom_line(data = data3, aes(x = dttz, y = ay,color = 'AY')) +
  #   geom_line(data = data3, aes(x = dttz, y = az,color = 'AZ')) +
  #   scale_colour_manual(name="Axis",
  #                       values=c(AX="red", AY="blue", AZ="green")) +
  #   ylab("Raw ACC") + 
  #   xlab("Time") 
  
  
  # check results
  x11()
  plot(data2$true_time,data2$mx)
  x11()
  plot(data3$dttz,data3$mx)
  # data2 %>%
  #   slice(1e6:(1e6+100)) %>%
  #   ggplot(aes(x = true_time,
  #              y = mx)) +
  #   geom_line() +
  #   geom_point(size = 1) +
  #   labs(title = 'Original data')
  # 
  # time_rng <- range(data2$true_time[1e6:(1e6+100)])
  # x11()
  # data3 %>%
  #   filter(between(dttz, time_rng[1], time_rng[2])) %>%
  #   ggplot(aes(x = dttz,
  #              y = mx)) +
  #   geom_line() +
  #   geom_point(size = 1) +
  #   labs(title = 'Rediscretized data')
  # 
  write_csv(data3, file.path(dirname(filename), paste(resampleRate,"HZ-",basename(filename),sep="")))
#} 
  