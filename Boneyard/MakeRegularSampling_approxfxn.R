# Run this function (all the way through create_csv) before running import.list
makeRegularApprox <- function(filenameA, resampleRate){
  # read acc file
  data <- read_csv(filenameA)
  data2 <- data %>%
    # seconds since the beginning
    mutate(secs_since = as.numeric(dt - min(dt))) %>%
    # group into within-seconds blocks
    group_by(secs_since) %>%
    # frequency and period of sampling
    dplyr::mutate(freq = n(),
                  period = 1 / n(),
                  # fraction of a second since beginning of second (i.e. 0-1)
                  frac_sec = (row_number() - 1) / n(),
                  # seconds since beginning (e.g. 9.456)
                  true_since = secs_since + frac_sec) %>%
    ungroup %>%
    # Filter out first and last seconds because they're partial
    filter(secs_since > 0,
           secs_since < max(secs_since)) %>%
    # true time down to fractional second (e.g. 2018-06-07 16:57:12.1234)
    mutate(true_time = min(dttz) + true_since)
  # show a table with the frequency of frequencies
  data2$freq %>% table
  # use frequency table to judge best resampling rate
  new_freq <- resampleRate
  # "- 1" b/c true_since starts at 1 (we drop second 0)
  temp_approx <- approxfun(data2$true_since - 1, 
                           data2$temp, 
                           method = 'constant')
  ax_approx <- approxfun(data2$true_since - 1, 
                         data2$ax, 
                         method = 'constant')
  ay_approx <- approxfun(data2$true_since - 1, 
                         data2$ay, 
                         method = 'constant')
  az_approx <- approxfun(data2$true_since - 1, 
                         data2$az, 
                         method = 'constant')
  mx_approx <- approxfun(data2$true_since - 1, 
                         data2$mx, 
                         method = 'constant')
  my_approx <- approxfun(data2$true_since - 1, 
                         data2$my, 
                         method = 'constant')
  mz_approx <- approxfun(data2$true_since - 1, 
                         data2$mz, 
                         method = 'constant')
  gx_approx <- approxfun(data2$true_since - 1, 
                         data2$gx, 
                         method = 'constant')
  gy_approx <- approxfun(data2$true_since - 1, 
                         data2$gy, 
                         method = 'constant')
  gz_approx <- approxfun(data2$true_since - 1, 
                         data2$gz, 
                         method = 'constant')
  data3 <- data.frame(dttz = seq(from = min(data2$true_time),
                                 to = max(data2$true_time),
                                 by = 1 / new_freq)) %>%
    mutate(num_time = as.numeric(dttz - min(dttz)),
           temp = temp_approx(num_time),
           ax = ax_approx(num_time),
           ay = ay_approx(num_time),
           az = az_approx(num_time),
           mx = mx_approx(num_time),
           my = my_approx(num_time),
           mz = mz_approx(num_time),
           gx = gx_approx(num_time),
           gy = gy_approx(num_time),
           gz = gz_approx(num_time))
  
  # check results
  # x11()
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
  
  write_csv(data3, file.path(dirname(filenameA), paste("48HZ-",basename(filenameA),sep="")))
} 