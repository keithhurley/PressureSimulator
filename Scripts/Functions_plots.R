t<-myResults$myInteractions %>%
  #as.data.frame %>%
  group_by(simId, fishId) %>%
  summarise(numInteractions=n()) %>%
  right_join(expand.grid(fishId=seq(1, myParamsObject$numberFish,1), simId=seq(1,mySimsObject$numberSimulations,1)), by=c("simId", "fishId")) %>%
  mutate(numInteractions=ifelse(is.na(numInteractions), 0, numInteractions))
  
t %>%
  group_by(simId) %>%
  summarise(meanInteractions=mean(numInteractions, na.rm=TRUE),
            sdInteractions=sd(numInteractions, na.rm=TRUE),
            numInteractions=n()) %>%
  mutate(CI=1.96*(sdInteractions/sqrt(numInteractions)),
         upperCI=meanInteractions+CI,
         lowerCI=meanInteractions-CI) %>%
  ggplot() +
  geom_point(aes(x=simId, y=meanInteractions), size=3) +
  geom_errorbar(aes(x=simId, ymin=lowerCI, ymax=upperCI), size=.5, width=0.25) +
  theme_minimal() +
  labs(x="Simulation Id", y="Mean Interactions")

t %>%
  group_by(fishId) %>%
  summarise(meanInteractions=mean(numInteractions, na.rm=TRUE),
            sdInteractions=sd(numInteractions, na.rm=TRUE),
            numInteractions=n()) %>%
  mutate(CI=1.96*(sdInteractions/sqrt(numInteractions)),
         upperCI=meanInteractions+CI,
         lowerCI=meanInteractions-CI) %>%
  ggplot() +
  geom_point(aes(x=fishId, y=meanInteractions), size=3) +
  geom_errorbar(aes(x=fishId, ymin=lowerCI, ymax=upperCI), size=.5, width=0.25) +
  theme_minimal() +
  labs(x="Fish Id", y="Mean Interactions")

t %>%
  group_by(fishId) %>%
  summarise(meanInteractions=mean(numInteractions, na.rm=TRUE),
            sdInteractions=sd(numInteractions, na.rm=TRUE),
            numInteractions=n()) %>%
  mutate(CI=1.96*(sdInteractions/sqrt(numInteractions)),
         upperCI=meanInteractions+CI,
         lowerCI=meanInteractions-CI) %>%
  mutate(results=ifelse(meanInteractions>=3, "High", "Low")) %>%
  ggplot() +
  geom_sf(data=lakeGeom, fill="skyblue") +
  geom_sf(aes(size=results, color=results, fill=results)) +
  scale_size_manual(values=c(3.5,2)) +
  scale_color_manual(values=c("navy", "yellow")) +
  scale_fill_manual(values=c("navy", "yellow")) +
  labs(color="", fill="", size="")





a1<-myResults$myInteractions %>%
  select(-geometry) %>%
  filter(fishId==2) %>%
  left_join(myResults$myFish, by=c("fishId")) %>%
  st_as_sf()

a2<-myResults$myInteractions %>%
  select(-geometry) %>%
  filter(fishId==2) %>%
  left_join(myResults$myCasts, by=c("anglerId", "castId")) %>%
  st_as_sf()

a3<-myResults$myInteractions %>%
  select(-geometry) %>%
  filter(fishId==2) %>%
  left_join(myResults$myAnglers, by=c("anglerId")) %>%
  st_as_sf()

ggplot() +
  geom_sf(data=myLakeObject$lakeGeom, color="navy", fill="skyblue") +
  geom_sf(data=a2, size=1, color="red") +
  geom_sf(data=a3, size=2, color="red") +
  geom_sf(data=a1, size=4, color="blue") 



ggplot() +
  geom_sf(data=myLakeObject$lakeGeom, fill="skyblue") +
  geom_sf(data=myLakeObject$restrictionsBoat, fill="white", color="transparent") +
  geom_sf(data=myLakeObject$lakeGeom, fill="transparent", color="black") +
  geom_sf(data=st_intersection(myLakeObject$restrictionsShore, myLakeObject$lakeGeom), fill="white", color="white", size=2) +
  geom_sf(data=myResults$myCasts, alpha=0.2) +
  geom_sf(data=myResults$myFish %>% st_as_sf(), color="yellow", size=0.5) +
  facet_wrap(~simId,ncol=3, labeller="label_both")
