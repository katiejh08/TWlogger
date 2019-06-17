JAM <- stateSummaryIND2period2state %>% 
  group_by(season,ID,state) %>%
  filter(state=="rest") %>% 
  summarize(stateTotHr = sum(stateCount2state)/60/60)

t.test(stateTotHr ~ season, data = JAM)

sd(JAM$stateTotHr[JAM$season=="Winter"])

JAMdeux <- stateSummaryIND2period %>% 
  group_by(season,ID,state_classif) %>%
  filter(state_classif=="2") %>% 
  summarize(stateTotHr = sum(stateCount)/60/60)

t.test(stateTotHr ~ season, data = JAMdeux)

