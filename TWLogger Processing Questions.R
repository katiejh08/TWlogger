# Examine factors that affect VDBA, an index of activity-specific energy
# Do sexes differ in how and when they allocate energy?
# Are males or females more active per unit of time?
# Do males and females have similar rest patterns (e.g., Are males more fragmented in their rest patterns?)
# Are there patterns in who leaves the roost first (e.g., Per depid, when is the steepest prolonged slope in VDBA values and is there a sex-dependent pattern?)
# Differences in time spent in categorical behaviors?

# Aggregate activity over the course of the day and also how it shook out by hour

# LSMDS0 is registering far more low values (i.e., 303 peak is spread over more lower values). 
# If you'd stack all those low values onto the 0.025 region, there's be a peak. This is clipping on the bottom end of the 303. It's bottoming out at its low value. 
# Any VDBA value less than 0.003 on 9DS0 is not being captured by the 303. 
# How many decimal places does it store per measurement.
# Set to 2Gs, it can register all values up to +/- 2G, but when it stores that value, it stores a bunch of extra decimal places of precision.
# One tag is able to get closer to zero because it has more precision. When we calculate VDBA, we subtract out static. 
 
# Clip inners of the DS0 using the values of the 303 (because the 303 had lower sensitivities)
