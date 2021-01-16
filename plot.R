ggplot() +
  geom_sf(data=myLakeObject$lakeGeom, fill="skyblue") +
  geom_sf(data=myLakeObject$restrictionsBoat, fill="white", color="transparent") +
    geom_sf(data=myLakeObject$lakeGeom, fill="transparent", color="black") +
  geom_sf(data=st_intersection(myLakeObject$restrictionsShore, myLakeObject$lakeGeom), fill="white", color="white", size=2) +
  geom_sf(data=myResults$myCasts, alpha=0.2) +
  geom_sf(data=myResults$myFish %>% st_as_sf(), color="yellow", size=0.5) +
  facet_wrap(~simId,ncol=3, labeller="label_both")



myResults$myAnglers %>% group_by(.$anglerType, .$simId) %>% tally()


ggplot() +
  geom_sf(data=myLakeObject$lakeGeom, fill="skyblue") +
  geom_sf(data=myResults$myAnglers %>% filter(anglerType=="Boat" & partyAnglerId==1), size=2, color="black") +
  facet_wrap(~simId, ncol=3)


ggplot() +
  geom_sf(data=myLakeObject$lakeGeom, fill="skyblue") +
  geom_sf(data=myLakeObject$restrictionsBoat, fill="white", color="transparent") +
  geom_sf(data=myLakeObject$lakeGeom, fill="transparent", color="black") +
  geom_sf(data=st_intersection(myLakeObject$restrictionsShore, myLakeObject$lakeGeom), fill="white", color="white", size=2) +
  geom_sf(data=myResults$myCasts, alpha=0.2, color="gray35") +
  geom_sf(data=myResults$myFish %>% st_as_sf(), color="yellow", size=0.5) +
  geom_sf(data=myResults$myAnglers, alpha=1) +
  facet_wrap(~simId,ncol=3, labeller="label_both")
