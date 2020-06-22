##########################################
####        Create NC file           #####
##########################################

# Import rediscretized data
filename <- file.choose()
data48hz <- read_csv(filename)
depid <- basename(filename) # add code to remove "48HZ-" and "-COMBINED.csv"
