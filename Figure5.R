stateSum2period <- dataset %>% 
  group_by(season,yr,depid,dayNight) %>%
  mutate(periodLength = n()) %>%
  ungroup %>% 
  group_by(season,yr,depid,dayNight,state_classif,periodLength) %>%
  summarize(stateCount = n(),
            VDBAsum = sum(VDBA)) %>%
  mutate(stateCountMin = round(stateCount/10/60,2),
         stateCountHour = round(stateCount/10/60/60,2),
         stateProp = round(stateCount/periodLength,4),
         sex = depMeta$sex[depMeta$depid==depid]) %>%
  ungroup

stateSum2period$state_classif <- as.factor(stateSum2period$state_classif)
# Plot daytime absolute state durations
new_scale <- function(new_aes) {
  structure(ggplot2::standardise_aes_names(new_aes), class = "new_aes")
}
colsSeas <- c("Summer" = "red", "Winter" = "blue")
colsSex <- c("Male" = "darkgreen", "Female" = "orange")
shapeSex <-c("Male" = 1, "Female" = 17)

PdayDur4state <- stateSum2period %>%
  filter(dayNight == "day") %>%
  ggplot() +
  geom_point(aes(x=state_classif,y=stateCountHour,shape=sex),alpha=0.8,position = "jitter",size=3) +
  scale_shape_manual(values=shapeSex) +
  # scale_color_manual(values=colsSex) +
  # new_scale_color() +
  # new_scale_fill()+
  geom_boxplot(aes(x=state_classif,y=stateCountHour,linetype=season),alpha=0.3) +
  scale_linetype_manual(values=c("Summer" = 1, "Winter" = 2)) +
  # scale_color_manual(values=colsSeas) +
  # scale_color_discrete(name = "Season") +
  scale_y_continuous(breaks = seq(0, 8, by = 2)) +
  # scale_x_discrete(labels = c("Rest", "Rest with Noise", "Low Activity","High Activity")) +
  theme_classic(base_size = 13) + 
  theme(legend.position = "none") +
  # theme(axis.title.x = element_blank(),
  #       axis.title.y = element_blank()) +
  labs(y="Duration (h)") +
  theme(axis.text.y = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())
        # axis.text.x = element_text(size=20),)
# labs(x="Season")# +
# theme(axis.text.y = element_text(size=18))
# facet_wrap(season~sex)
# facet_wrap(sex~season)
# facet_wrap(~season)

PdayDur4state
# Save a file at 300 ppi
ggsave(PdayDur4state, file="Figure5.png",width=12, height=8, dpi=300)

# Plot daytime state proportions ------------------------------------------

# Plot daytime absolute state proportions
new_scale <- function(new_aes) {
  structure(ggplot2::standardise_aes_names(new_aes), class = "new_aes")
}
colsSeas <- c("Summer" = "red", "Winter" = "blue")
colsSex <- c("Male" = "darkgreen", "Female" = "orange")
shapeSex <-c("Male" = 1, "Female" = 17)
PdayProp4state <- stateSum2period %>%
  filter(dayNight == "day") %>%
  ggplot() +
  geom_point(aes(x=state_classif,y=stateProp,shape=sex),alpha=0.8,position = "jitter",size=3) +
  scale_shape_manual(values=shapeSex) +
  geom_boxplot(aes(x=state_classif,y=stateProp,linetype=season),alpha=0.3) +
  scale_linetype_manual(values=c("Summer" = 1, "Winter" = 2)) +
  scale_y_continuous(breaks = seq(0, 0.5, by = 0.1)) +
  scale_x_discrete(labels = c("Rest", "Rest with Noise", "Low Activity","High Activity")) +
  theme_classic(base_size = 13) + 
  theme(legend.position = "none") +
  labs(y="Proportion") +
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.title.x = element_blank())

PdayProp4state
# Save a file at 300 ppi
ggsave(PdayProp4state, file="Figure5prop.png",width=12, height=8, dpi=300)

dayDurProp<-ggarrange(PdayDur4state,PdayProp4state,nrow=2,align="v")
ggsave(dayDurProp, file="Figure5-combined.png",width=12, height=8, dpi=300)


# State-season plot -------------------------------------------------------

prop2 <- stateSum2period %>% 
  dplyr::mutate(stateSeas = paste0(state_classif,"-",season)) %>%
  dplyr::filter(dayNight == "day") %>%
  ggplot() +
  geom_point(aes(x=stateSeas,y=stateProp,shape=sex),alpha=0.8,position = "jitter",size=3) +
  scale_shape_manual(values=shapeSex) +
  geom_boxplot(aes(x=stateSeas,y=stateProp,linetype=season),alpha=0.3) +
  scale_linetype_manual(values=c("Summer" = 1, "Winter" = 2)) +
  scale_y_continuous(breaks = seq(0, 0.5, by = 0.1)) +
  theme_classic(base_size = 13) + 
  theme(legend.position = "none") +
  labs(y="Proportion") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank())
prop2

dur2 <- stateSum2period %>%
  dplyr::mutate(stateSeas = paste0(state_classif,"-",season)) %>%
  dplyr::filter(dayNight == "day") %>%
  ggplot() +
  geom_point(aes(x=stateSeas,y=stateCountHour,shape=sex),alpha=0.8,position = "jitter",size=3) +
  scale_shape_manual(values=shapeSex) +
  geom_boxplot(aes(x=stateSeas,y=stateCountHour,linetype=season),alpha=0.3) +
  scale_linetype_manual(values=c("Summer" = 1, "Winter" = 2)) +
  scale_y_continuous(breaks = seq(0, 8, by = 2)) +
  theme_classic(base_size = 13) + 
  theme(legend.position = "none") +
  labs(y="Duration (h)") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank())

dur2
# Save a file at 300 ppi
dayDurProp2<-ggarrange(dur2,prop2,nrow=2,align="v")
ggsave(dayDurProp2, file="Figure5-combined-v2.png",width=12, height=8, dpi=300)
