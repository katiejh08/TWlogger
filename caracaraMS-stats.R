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
load("caracaraMSmass-data.RData")

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
names(sum)
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
sd(sum$VDBA24h[sum$sex=="Female"])

mean(sum$VDBA24h[sum$sex=="Male"])
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
mean(sum$VDBAdayRateHr[sum$season=="Winter"& sum$sex=="Male"])
sd(sum$VDBAdayRateHr[sum$season=="Winter" & sum$sex=="Male"])

mean(sum$VDBAdayRateHr[sum$season=="Summer"& sum$sex=="Male"])
sd(sum$VDBAdayRateHr[sum$season=="Summer"& sum$sex=="Male"])

mean(sum$VDBAdayRateHr[sum$season=="Winter"& sum$sex=="Female"])
sd(sum$VDBAdayRateHr[sum$season=="Winter"& sum$sex=="Female"])

mean(sum$VDBAdayRateHr[sum$season=="Summer"& sum$sex=="Female"])
sd(sum$VDBAdayRateHr[sum$season=="Summer"& sum$sex=="Female"])

# Results (from m1):
# Winter daytime hourly VDBA rates are 65% higher than in summer (winter: 491.50 +- 109.35 g, summer: 297.38 +- 46.73 g; p<0.001, t=7.05). 
# Moreover, year-round, males have a 25% higher daytime hourly VDBA rate than females (males: 447.46 +- 143.89 g, females: 356.57 +- 106.37 g; p <0.01, t=3.19). 
# Even though the best model had both tag version and sex, tag version only explained an additional 6% of the variation (p = 0.04, t=2.24). 


# Daytime State Duratoins -------------------------------------------------

# State 3
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
mean(state3day$stateCountHour[state3day$sex=="Male"])
sd(state3day$stateCountHour[state3day$sex=="Male"])
mean(state3day$stateCountHour[state3day$sex=="Female"])
sd(state3day$stateCountHour[state3day$sex=="Female"])

# Results (from m3): see below

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
# Across both seasons males spent more time in state 3 (males: 3.98 +- 0.90 h, females: 3.25 +- 0.63 h; p<0.01, t=3.26) 
# and in state 4 (males: 3.64 +- 0.47 h, females: 2.74 +- 0.58 h; p<0.001, t=4.05). 
# During winter, all caracaras spent less time in state 3 (p<0.001, t=-4.63), while we found no seasonal difference in time spent in state 4.

# Alternative phrasing (KJH: UPDATE TO ACCURATELY REFLECT):
# Caracaras spent more time in low intensity activity during the long summer days compared to the short winter days, 
# and proportionately more time active and less time resting in summer compared to winter.
# 

# Daytime State Proportions -----------------------------------------------

# State 4
# state4dayProp <- stateSum2period %>% 
#   group_by(season,depid,dayNight,state_classif,stateProp) %>%
#   filter(dayNight=="day",
#          state_classif=="4") %>%
#   mutate(sex=depMeta$sex[depMeta$depid==depid],
#          model=depMeta$model[depMeta$depid==depid]) %>% 
#   ungroup 

# State 4
# shapiro.test(state4dayProp$stateProp)
# var.test(stateProp ~ season,data=state4dayProp)

m1 <- glm(state4 ~ season + sex + model, data = sum)
summary(m1)
m2 <- glm(state4 ~ season:sex + model, data = sum)
summary(m2)
anova(m1, m2, test = "F") # compare models 
# No effect of interaction (i.e., p>0.05) so keep m1
m3 <- glm(state4 ~ season + sex, data = sum)
anova(m1, m3, test = "F") # compare models 
# No effect of model (i.e., p>0.05) so keep m3
m4 <- glm(state4 ~ season, data = sum)
anova(m3, m4, test = "F") # compare models 
# Effect of sex (i.e., p<0.05) so keep m3
m4a <- glm(state4 ~ sex, data = sum)
anova(m3, m4a, test = "F") # compare models 
# Effect of season (i.e., p<0.05) so keep m3
m5 <- glm(state4 ~ 1, data = sum)
anova(m3, m5, test = "F") # compare models

summary(m3)
# The best model had both sex and season. 
# During winter, males spent X% more daylight hours in state 4 than in summer and females spent x% more. 
# Moreover, in winter males spent a greater proportion of daylight hours in state 4 than females (males: 0.35 +- 0.04, females: 0.25 +- 0.06; ANOVA, p<0.01, t=3.77).
# glm(formula = stateProp ~ season + sex, data = state4dayProp)
# 
# Deviance Residuals: 
#   Min         1Q     Median         3Q        Max  
# -0.102011  -0.032664  -0.001863   0.032577   0.086835  
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   0.15106    0.01531   9.864 2.46e-09 ***
#   seasonWinter  0.11385    0.01859   6.124 4.47e-06 ***
#   sexMale       0.07105    0.01885   3.768  0.00113 ** 

# State 4 proportion stats
# mean(state4dayProp$stateProp[state4dayProp$season=="Winter" & state4dayProp$sex=="Male"])
# # 0.35156
# sd(state4dayProp$stateProp[state4dayProp$season=="Winter" & state4dayProp$sex=="Male"])
# # 0.04871297
# mean(state4dayProp$stateProp[state4dayProp$season=="Winter" & state4dayProp$sex=="Female"])
# # 0.2537714
# sd(state4dayProp$stateProp[state4dayProp$season=="Winter" & state4dayProp$sex=="Female"])
# # 0.05876818
# mean(state4dayProp$stateProp[state4dayProp$season=="Summer" & state4dayProp$sex=="Male"])
# # 0.20652
# sd(state4dayProp$stateProp[state4dayProp$season=="Summer" & state4dayProp$sex=="Male"])
# # 0.02667334
# mean(state4dayProp$stateProp[state4dayProp$season=="Summer" & state4dayProp$sex=="Female"])
# # 0.1622
# sd(state4dayProp$stateProp[state4dayProp$season=="Summer" & state4dayProp$sex=="Female"])
# # 0.03245705

# State 4
mean(sum$state4[sum$season=="Winter" & sum$sex=="Male"])
sd(sum$state4[sum$season=="Winter" & sum$sex=="Male"])
mean(sum$state4[sum$season=="Winter" & sum$sex=="Female"])
sd(sum$state4[sum$season=="Winter" & sum$sex=="Female"])

mean(sum$state4[sum$season=="Summer" & sum$sex=="Male"])
sd(sum$state4[sum$season=="Summer" & sum$sex=="Male"])
mean(sum$state4[sum$season=="Summer" & sum$sex=="Female"])
sd(sum$state4[sum$season=="Summer" & sum$sex=="Female"])

# State 3
# state3dayProp <- stateSum2period %>% 
#   group_by(season,depid,dayNight,state_classif,stateProp) %>%
#   filter(dayNight=="day",
#          state_classif=="3") %>%
#   mutate(sex=depMeta$sex[depMeta$depid==depid],
#          model=depMeta$model[depMeta$depid==depid]) %>% 
#   ungroup 
# 
# # State 3
# shapiro.test(state3dayProp$stateProp)
# var.test(stateProp ~ season,data=state3dayProp)

m1 <- glm(state3 ~ season + sex + model, data = sum)
summary(m1)
m2 <- glm(state3 ~ season:sex + model, data = sum)
summary(m2)
anova(m1, m2, test = "F") # compare models 
# No effect of interaction (i.e., p>0.05) so keep m1
m3 <- glm(state3 ~ season + sex, data = sum)
anova(m1, m3, test = "F") # compare models 
# No effect of model (i.e., p>0.05) so keep m3
m4 <- glm(state3 ~ season, data = sum)
anova(m3, m4, test = "F") # compare models 
# Effect of sex (i.e., p<0.05) so keep m3
m4a <- glm(state3 ~ sex, data = sum)
anova(m3, m4a, test = "F") # compare models 
# Effect of season (i.e., p<0.05) so keep m3
m5 <- glm(state3 ~ 1, data = sum)
anova(m3, m5, test = "F") # compare models

summary(m3)
# Call:
#   glm(formula = stateProp ~ season + sex, data = state3dayProp)
# 
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -0.09589  -0.01596   0.00154   0.02781   0.07891  
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   0.22031    0.01462  15.072 9.81e-13 ***
#   seasonWinter  0.03998    0.01774   2.253   0.0351 *  
#   sexMale       0.04942    0.01800   2.746   0.0121 *  

# # Model with both season and sex best explains the data.
# # State 3 proportion stats
# mean(state3dayProp$stateProp[state3dayProp$season=="Winter" & state3dayProp$sex=="Male"])
# # 0.30204
# sd(state3dayProp$stateProp[state3dayProp$season=="Winter" & state3dayProp$sex=="Male"])
# # 0.04794891
# mean(state3dayProp$stateProp[state3dayProp$season=="Winter" & state3dayProp$sex=="Female"])
# # 0.2657714
# sd(state3dayProp$stateProp[state3dayProp$season=="Winter" & state3dayProp$sex=="Female"])
# # 0.06225319
# mean(state3dayProp$stateProp[state3dayProp$season=="Summer" & state3dayProp$sex=="Male"])
# # 0.2774
# sd(state3dayProp$stateProp[state3dayProp$season=="Summer" & state3dayProp$sex=="Male"])
# # 0.03226035
# mean(state3dayProp$stateProp[state3dayProp$season=="Summer" & state3dayProp$sex=="Female"])
# # 0.2148286
# sd(state3dayProp$stateProp[state3dayProp$season=="Summer" & state3dayProp$sex=="Female"])
# # 0.01849646

# State 3
mean(sum$state3[sum$season=="Winter" & sum$sex=="Male"])
sd(sum$state3[sum$season=="Winter" & sum$sex=="Male"])
mean(sum$state3[sum$season=="Winter" & sum$sex=="Female"])
sd(sum$state3[sum$season=="Winter" & sum$sex=="Female"])

mean(sum$state3[sum$season=="Summer" & sum$sex=="Male"])
sd(sum$state3[sum$season=="Summer" & sum$sex=="Male"])
mean(sum$state3[sum$season=="Summer" & sum$sex=="Female"])
sd(sum$state3[sum$season=="Summer" & sum$sex=="Female"])

# Mass --------------------------------------------------------------------

mass$yr<-as.factor(mass$yr)
mass <- mass %>%
  dplyr::filter(island=="SDI",
                age=="JUV",
                yr%in%c("2017","2018","2019"))

ggplot() +
  stat_summary(data = mass, aes(sex, mass, fill = season),
               fun.data = fun, geom = "boxplot", position = position_dodge(width = 1)) +
  scale_fill_manual("season", values = c("goldenrod2", "dodgerblue4")) +
  theme_classic()+
  theme(
    text = element_text(size = 16),
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.spacing = unit(2, "lines"),
    legend.position = "none") +
  ylab(expression("Mass (g)")) + 
  xlab("")

# Test for seasonal differences (no a priori sex difference)
shapiro.test(mass$mass)
var.test(mass ~ season,data=mass)

m1<-glm(mass~sex+season+yr,data=mass)
summary(m1)
m2 <- glm(mass~sex:season+yr,data=mass)
summary(m2)
anova(m1, m2, test = "F") # compare models 
# No effect of interaction (i.e., p>0.05) so keep m1
m3 <- glm(mass~sex+season,data=mass)
anova(m1, m3, test = "F") # compare models 
# No effect of yr (i.e., p>0.05) so keep m3
m4 <- glm(mass~sex,data=mass)
anova(m3, m4, test = "F") # compare models 
# Effect of season (i.e., p<0.05) so keep m3
m4a <- glm(mass~season,data=mass)
anova(m3, m4a, test = "F") # compare models 
# Effect of season and sex
m5 <- glm(mass~1,data=mass)
anova(m3, m5, test = "F") # compare models
summary(m3)

massF <- mass %>% dplyr::filter(sex=="Female")
massM <- mass %>% dplyr::filter(sex=="Male")

# Sexually dimorphic a priori
# Test females
shapiro.test(massF$mass)
var.test(mass ~ season,data=massF)
t.test(mass ~ season,data=massF)

f1 <- glm(mass ~ season+yr, data=massF)
summary(f1)
f2 <- glm(mass ~ season:yr, data=massF)
summary(f2)
anova(f1, f2, test = "F") # compare models 
# No effect of interaction (i.e., p>0.05) so keep m2
f3 <- glm(mass~season,data=massF)
anova(f1, f3, test = "F") # compare models 
# No effect of yr (i.e., p>0.05) so keep m3
f4 <- glm(mass~yr,data=massF)
anova(f1, f4, test = "F") # compare models 
# No effect of season (i.e., p>0.05) so keep m3
f5 <- glm(mass~1,data=massF)
anova(f1, f5, test = "F") # compare models

# Test males
shapiro.test(massM$mass)
var.test(mass ~ season,data=massM)
wilcox.test(mass ~ season,data=massM)

t1 <- glm(mass ~ season+yr, data=massM)
summary(t1)
t2 <- glm(mass ~ season:yr, data=massM)
summary(t2)
anova(t1, t2, test = "F") # compare models 
# No effect of interaction (i.e., p>0.05) so keep m2
t3 <- glm(mass~season,data=massM)
anova(t1, t3, test = "F") # compare models 
# No effect of yr (i.e., p>0.05) so keep m3
t4 <- glm(mass~yr,data=massM)
anova(t1, t4, test = "F") # compare models 
# No effect of season (i.e., p>0.05) so keep m3
t5 <- glm(mass~1,data=massM)
anova(t1, t5, test = "F") # compare models

plot(t5)

mean(massM$mass[massM$season=="Winter"])
mean(massM$mass[massM$season=="Summer"])
sd(massM$mass[massM$season=="Winter"])
sd(massM$mass[massM$season=="Summer"])

# Juvenile male mass decreases by 9.8% in winter (summer: 1578 +- 147 g , n= 28, winter: 1424 +- 95 g, n=43).
# sample sizes
mass %>% group_by(season,sex) %>% summarize(n=n()) %>% View
hist(massM$mass)

mass %>%
  group_by(sex,season) %>%
  summarize(n=n(),
            min=min(mass),
            max=max(mass),
            mean=mean(mass),
            sd=sd(mass)) %>% View
