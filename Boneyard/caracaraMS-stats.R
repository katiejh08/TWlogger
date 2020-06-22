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
load("caracaraMS-data.RData")

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
m1 <- glm(VDBA24h ~ season + sex + model, data = sum)
summary(m1)
m2 <- glm(VDBA24h ~ season:sex + model, data = sum) # Add interaction
anova(m1, m2, test = "F") # compare models
# Model with interaction is not better. Keep m1.
m3 <- glm(VDBA24h ~ sex + model, data = sum) # remove season
anova(m1, m3, test = "F") # compare models
# No effect of season (i.e., p>0.05) so keep m3
m3a <- glm(VDBA24h ~ model, data = sum) # remove sex
anova(m3a, m3, test = "F") #compare models
# Effect of sex (i.e., p<0.05) so keep m3
m4 <- glm(VDBA24h ~ sex, data = sum) # remove model
anova(m3, m4, test = "F") #compare models
# Effect of model (i.e., p<0.05) so keep m3 
m5 <- glm(VDBA24h ~ 1, data = sum) # compare sex model to a model with no slope
anova(m3, m5, test = "F")
# The last two models are not similar, so keep m3. Look at R2 to see how much variation is explained by model.

# Compare R2 of models with and without tag model. 
with(summary(m3), 1 - deviance/null.deviance) # R2 = 0.45
with(summary(m4), 1 - deviance/null.deviance) # R2 = 0.32
# Compare adjusted R2
rsq(m3,adj=TRUE,type=c('v','kl','sse','lr','n')) # adj R2 = 0.40
rsq(m4,adj=TRUE,type=c('v','kl','sse','lr','n')) # adj R2 = 0.29

# Plot to evaluate residiuals - they look good so models okay
plot(m3)
plot(m4)

summary(m3)
mean(sum$VDBA24h[sum$season=="Winter"])
mean(sum$VDBA24h[sum$season=="Summer"])
sd(sum$VDBA24h[sum$season=="Winter"])
sd(sum$VDBA24h[sum$season=="Summer"])
mean(sum$VDBA24h[sum$sex=="Female"])
mean(sum$VDBA24h[sum$sex=="Male"])
sd(sum$VDBA24h[sum$sex=="Female"])
sd(sum$VDBA24h[sum$sex=="Male"])

# Results (from m3):
# While we found no seasonal difference in 24-h VDBA, 
# year-round, males have 21% higher 24-hr VDBA than females (males: 62,092 +- 9174 g, females: 51,249 +- 7237 g; p<0.01, t=3.35).
# Even though the best model had both tag version and sex, tag version only explained an additional 13% of the variation (p < 0.05, t=2.20). 

# Daytime Hourly VDBA Rate ------------------------------------------------

# Is there a seasonal difference in daytime hourly rate of VDBA?
sum %>% 
  ggplot() +
  geom_boxplot(aes(season,VDBAdayRateHr,fill=sex)) +
  labs(x=NULL,
       y="Hourly VDBA (gravitational g)") #+
  # facet_wrap(~model)

shapiro.test(sum$VDBAdayRateHr)
var.test(VDBAdayRateHr ~ season,data=sum)

m1 <- glm(VDBAdayRateHr ~ season + sex + model, data = sum)
summary(m1)
m2 <- glm(VDBAdayRateHr ~ season:sex + model, data = sum)
summary(m2)
anova(m1, m2, test = "F") # compare models 
# No effect of interaction (i.e., p>0.05) so keep m1
m3 <- glm(VDBAdayRateHr ~ season + sex, data = sum)
anova(m1, m3, test = "F") # compare models 
# Effect of model (i.e., p<0.05) so keep m1
m4 <- glm(VDBAdayRateHr ~ season + model, data = sum)
anova(m1, m4, test = "F") # compare models 
# Effect of sex (i.e., p<0.05) so keep m1
m4a <- glm(VDBAdayRateHr ~ sex + model, data = sum)
anova(m1, m4a, test = "F") # compare models 
# All are important
m5 <- glm(VDBAdayRateHr ~ 1, data = sum)
anova(m1, m5, test = "F") # compare models

# The model with season sex and device model best explains the data. Though look at R2 to see how much variation is explained by model.

# Compare R2 of models with and without tag model. 
with(summary(m1), 1 - deviance/null.deviance) # R2 = 0.78
with(summary(m3), 1 - deviance/null.deviance) # R2 = 0.72
# Compare adjusted R2
rsq(m1,adj=TRUE,type=c('v','kl','sse','lr','n')) # adj R2 = 0.74
rsq(m3,adj=TRUE,type=c('v','kl','sse','lr','n')) # adj R2 = 0.69

summary(m1)
mean(sum$VDBAdayRateHr[sum$season=="Winter"])
mean(sum$VDBAdayRateHr[sum$season=="Summer"])
sd(sum$VDBAdayRateHr[sum$season=="Winter"])
sd(sum$VDBAdayRateHr[sum$season=="Summer"])
mean(sum$VDBAdayRateHr[sum$sex=="Female"])
mean(sum$VDBAdayRateHr[sum$sex=="Male"])
sd(sum$VDBAdayRateHr[sum$sex=="Female"])
sd(sum$VDBAdayRateHr[sum$sex=="Male"])

# Results (from m1):
# Winter daytime hourly VDBA rates are 65% higher than in summer (winter: 491.50 +- 109.35 g, summer: 297.38 +- 46.73 g; p<0.001, t=7.05). 
# Moreover, year-round, males have a 25% higher daytime hourly VDBA rate than females (males: 447.46 +- 143.89 g, females: 356.57 +- 106.37 g; p <0.01, t=3.19). 
# Even though the best model had both tag version and sex, tag version only explained an additional 6% of the variation (p = 0.04, t=2.24). 


# Daytime State Duratoins -------------------------------------------------
# State 3
shapiro.test(state3day$stateCountHour)
var.test(stateCountHour ~ season,data=state3day)

m1 <- glm(stateCountHour ~ season + sex + model, data = state3day)
summary(m1)
m2 <- glm(stateCountHour ~ season:sex + model, data = state3day)
summary(m2)
anova(m1, m2, test = "F") # compare models 
# No effect of interaction (i.e., p>0.05) so keep m1
m3 <- glm(stateCountHour ~ season + sex, data = state3day)
anova(m1, m3, test = "F") # compare models 
# No effect of model (i.e., p>0.05) so keep m3
m4 <- glm(stateCountHour ~ season, data = state3day)
anova(m3, m4, test = "F") # compare models 
# Effect of sex (i.e., p<0.05) so keep m3
m4a <- glm(stateCountHour ~ sex, data = state3day)
anova(m3, m4a, test = "F") # compare models 
# Effect of season (i.e., p<0.05) so keep m3
m5 <- glm(stateCountHour ~ 1, data = state3day)
anova(m3, m5, test = "F") # compare models

summary(m3)
# Results (from m3):
# During winter, caracaras spent less time in state 3 (p<0.001, t=-4.63); 
# and across both seasons males spent more time in state 3 (p<0.01, t=3.26). 


# State 4
shapiro.test(state4day$stateCountHour)
var.test(stateCountHour ~ season,data=state4day)

m1 <- glm(stateCountHour ~ season + sex + model, data = state4day)
summary(m1)
m2 <- glm(stateCountHour ~ season:sex + model, data = state4day)
summary(m2)
anova(m1, m2, test = "F") # compare models 
# No effect of interaction (i.e., p>0.05) so keep m1
m3 <- glm(stateCountHour ~ season + sex, data = state4day)
anova(m1, m3, test = "F") # compare models 
# No effect of model (i.e., p>0.05) so keep m3
m4 <- glm(stateCountHour ~ season, data = state4day)
anova(m3, m4, test = "F") # compare models 
# Effect of sex (i.e., p<0.05) so keep m3
m4a <- glm(stateCountHour ~ sex, data = state4day)
anova(m3, m4a, test = "F") # compare models 
# No effect of season (i.e., p>0.05) so keep m4a
m5 <- glm(stateCountHour ~ 1, data = state4day)
anova(m4a, m5, test = "F") # compare models

summary(m4a)
mean(state4day$stateCountHour[state4day$sex=="Male"])
sd(state4day$stateCountHour[state4day$sex=="Male"])
mean(state4day$stateCountHour[state4day$sex=="Female"])
sd(state4day$stateCountHour[state4day$sex=="Female"])

# Results (from m4a):
# We found no seasonal difference in time spent in state 4, however, 
# across both seasons males spent more time in state 4 (males: 3.64 +- 0.47 h, females: 2.74 +- 0.58 h; p<0.001, t=4.05).

# Daytime State Proportions -----------------------------------------------

#NOTE: the sum dataframe contains fields state1, state2, state3, state4, which are the proportion of daytime spent in each state.

sum %>% 
  ggplot() +
  geom_boxplot(aes(state1))

# Plot daytime absolute state durations
PdayDur4state <- stateSum2period %>%
  group_by(depid,season,dayNight) %>% 
  filter(dayNight == "day") %>%
  ggplot() +
  geom_boxplot(aes(x=state_classif,y=stateCountHour, color = season)) +
  annotate("text", x = 0.6, y = 14, label = "B", color="black") +
  annotate("text", x = 1, y = 10, label = "*", color="black") +
  annotate("text", x = 2, y = 10, label = "*", color="black") +
  annotate("text", x = 3, y = 10, label = "*", color="black") +
  annotate("text", x = 4, y = 10, label = "*", color="black") +
  scale_color_discrete(name = "Season") +
  scale_y_continuous(breaks = seq(0, 14, by = 2)) +
  scale_x_discrete(labels = c("Rest", "Rest with Noise", "Low Activity","High Activity")) +
  theme_classic(base_size = 13) + 
  theme(legend.position = "none") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  # theme(axis.text.x = element_blank()) +
  theme(axis.text.y = element_text(size=18),
        axis.text.x = element_text(size=18))
PdayDur4state
# Save a file at 300 ppi
# ggsave(PdayDur, file="Seasonal daytime state absolute durations.png",width=12, height=8, dpi=300)

# Is there a seasonal difference in the proportion of time spent in each state? Does sex have an effect?
# I'm not sure how to model this? It seems like it should still be a GLM  following the same process as above, 
# but these are proportions, so I'm not sure.

 
