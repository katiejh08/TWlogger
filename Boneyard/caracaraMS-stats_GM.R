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
pkgTest("dplyr")
pkgTest("ggpubr")
pdgTest("car")
pkgTest("rsq")
pkgTest("signal")

# Load data
load("C:/Users/gitte/Documents/R/caracaraMS-data.RData")

# This dataframe (named "sum") contains: 
# Useful identifiers/variables: depid (deployment ID), yr, dayLengthHr (hours of daytime)
# Predictors: season, sex, model (device model)
# Response: VDBA24h (24-hr VDBA sum), VDBAday (daytime VDBA sum), VDBAdayRateHr (daytime hourly VDBA rate), and 
# state1, state2, state3, state4 (proporations of daytime spent in each state)  

# 24-hr VDBA --------------------------------------------------------------

# Is there a seasonal difference in 24-h VDBA?

sum %>% 
  ggplot() +
  geom_boxplot(aes(season,VDBA24h,fill=sex)) +
  labs(x=NULL,
       y="24-h VDBA (gravitational g)") #+
  # facet_wrap(~model)

shapiro.test(sum$VDBA24h)
var.test(VDBA24h ~ season,data=sum)

# Analysis of Variance

m1 <- glm(VDBA24h ~ season + sex + model , data = sum)
summary(m1)

m2 <- glm(VDBA24h ~ season:sex + model, data = sum)
anova(m1, m2, test = "F") # compare models
# Model with interaction is not better. Keep m1.

m3 <- glm(VDBA24h ~ sex + model, data = sum) # remove season
anova(m1, m3, test = "F") # compare models
# No effect of season (i.e., p>0.05) so keep m3

m3a <- glm(VDBA24h ~ model, data = sum) # remove season
anova(m1, m3, test = "F") # compare models
# No effect of season (i.e., p>0.05) so keep m3

m4 <- glm(VDBA24h ~ sex, data = sum) # remove model
anova(m3a, m3, test = "F") #compare models
anova (m3, m4, test = "F")
# Effect of model (i.e., p<0.05) and sex so keep m3 

m5 <- glm(VDBA24h ~ 1, data = sum) # compared your sex model to a model with no slope
anova(m3, m5, test = "F")

# The last two models are not similar, so keep m3. Look at adj R2 to see how much variation is explained by model.

# Compare R2 of models with and without tag model. 
with(summary(m3), 1 - deviance/null.deviance) # R2 = 0.45
with(summary(m4), 1 - deviance/null.deviance) # R2 = 0.32
# Compare adjusted R2  GITTE NOTE: THIS WAS NOT WORKING FOR ME _ TRIED HELP BUT CANT EASILY FIGURE OUT
rsq(m3,adj=TRUE) #type=c('v','kl','sse','lr','n'))
rsq(m4,adj=TRUE)#type=c('v','kl','sse','lr','n'))

mean(sum$VDBA24h[sum$sex=="Female"])
mean(sum$VDBA24h[sum$sex=="Male"])
sd(sum$VDBA24h[sum$sex=="Female"])
sd(sum$VDBA24h[sum$sex=="Male"])

#plot to evaluate residiuals - they look good so models okay
plot(m3)
plot(m4)

# Results:
# There is a slight effect of model, but it only explains 10% of variation in the data.
# Sex is more significant at explaining variance in the data. 
# Year-round, males have 21% higher 24-hr VDBA than females (males: 62,092 +- 9174 g, females: 51,249 +- 7237 g; p<0.01, t=3.239).

# GITTE NOTE - should compare unadjusted R2 - when selecting best model use adj r2, but if want to talk abou the amount of variation they explain then need to use R2
# you explain pretty well.  I might start on focusing on sex - stating something about males having 21% higher VDBA (stats in parentheses). 
# then maybe say even though the best model had both tag version and sex, tag version only explained an additional 13% of the variation (stats). 



# Daytime Hourly VDBA Rate ------------------------------------------------

# Is there a seasonal difference in daytime hourly rate of VDBA?
sum %>% 
  ggplot() +
  geom_boxplot(aes(season,VDBAdayRateHr,fill=sex)) +
  labs(x=NULL,
       y="Hourly VDBA (gravitational g)") #+
  # facet_wrap(~model)

#GITTE NOTE:  below was refer to data I do not have - changed it to sum and it worked
shapiro.test(sum$VDBAdayRateHr)
var.test(VDBAdayRateHr ~ season,data=sum)

m1 <- glm(VDBAdayRateHr ~ season + sex + model, data = sum)
summary(m1)
m2 <- glm(VDBAdayRateHr ~ season:sex + model, data = sum)
summary(m2)
anova(m1, m2, test = "F") # compare models 
# No effect of interation (i.e., p>0.05) so keep m1
m3 <- glm(VDBAdayRateHr ~ season + sex, data = sum)
anova(m1, m3, test = "F") # compare models 
# Effect of model (i.e., p<0.05) so keep m1
m4 <- glm(VDBAdayRateHr ~ season + model, data = sum)
anova(m1, m4, test = "F") # compare models 
# Effect of sex (i.e., p<0.05) so keep m1
m4a <- glm(VDBAdayRateHr ~ sex + model, data = sum)
anova(m1, m4a, test = "F") # compare models 
# all are important
m5 <- glm(VDBAdayRateHr ~ 1, data = sum)
anova(m1, m5, test = "F") # compare models

# The model with season sex and device model best explains the data. Though look at adj R2 to see how much variation is explained by model.

# Compare R2 of models with and without tag model. 
with(summary(m1), 1 - deviance/null.deviance) # R2 = 0.45  #GM NOTE: think left over form above I got .78
with(summary(m3), 1 - deviance/null.deviance) # R2 = 0.32  #DITTO: I got 0.72  so only 6% difference
# Compare adjusted R2 _ GM NOTE _ NOT WORKING FOR ME BUT SHOULD COMPARE R2
rsq(m1,adj=TRUE,type=c('v','kl','sse','lr','n'))
rsq(m3,adj=TRUE,type=c('v','kl','sse','lr','n'))

# There is a slight effect of model, though it only explains 5% of variation. (GITTE NOTE I got 6%)
# Daytime hourly VDBA is higher in winter (p<0.001,t=6.65).
# Year round males have X% higher daytime hourly VDBA rates (males: XX +- xx g, females: XX +- xx g; p<0.01,t=3.07).



# Daytime State Proportions -----------------------------------------------

# NOTE: the sum dataframe contains fields state1, state2, state3, state4, which are the proportion of daytime spent in each state.

# Is there a seasonal difference in the proportion of time spent in each state? Does sex have an effect?
# I'm not sure how to model this? It seems like it should still be a GLM  following the same process as above, 
# but these are proportions, so I'm not sure.

 
