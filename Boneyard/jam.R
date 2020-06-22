# Calculate total time (h) spent in resting state per season
JAM <- stateSummaryIND2period2state %>% 
  group_by(season,ID,state) %>%
  filter(state=="rest") %>% 
  summarize(stateTotHr = sum(stateCount2state)/60/60)

# Test for seasonal difference in time spent resting
shapiro.test(JAM$stateTotHr) #Results: W = 0.96779, p-value = 0.6129
var.test(stateTotHr ~ season,data=JAM) #Results: F = 1.2561, num df = 11, denom df = 11, p-value = 0.7119
t.test(stateTotHr ~ season, data = JAM,var.equal = TRUE)

sd(JAM$stateTotHr[JAM$season=="Winter"])
sd(JAM$stateTotHr[JAM$season=="Summer"])

# Test for seasonal difference in time spent in state 1
JAMdeux <- stateSummaryIND2period %>% 
  group_by(season,ID,state_classif) %>%
  filter(state_classif=="1") %>% 
  summarize(stateTotHr = sum(stateCount)/60/60)

t.test(stateTotHr ~ season, data = JAMdeux)


# Calculate proportion of day and night spent in state 4
