source("./SimulationScripts/Functions_prep.R")

#load lake object
myLakeObject<-obj_create_default_lake_object()

#create parameter object
myParamsObject<-obj_create_default_parameters_object()

#create simulations object
mySimsObject<-obj_create_default_simulations_object()

#run simulations
myResults<-sims_runSimulations(myLakeObject,
               myParamsObject,
               mySimsObject)

#save output
  #include runtime
  #include flag if full run completed?
save_simulation_run(fileNameAndPath=paste(mySimsObject$saveNamePath,
                                          mySimsObject$saveNameBase,
                                          save_create_timestamp(),
                                          sep=""),
                    myLakeObject,
                    myParamsObject,
                    mySimsObject,
                    myResults)










#   # ggplot() +
#   #   geom_sf(data=lakes_round_base, fill="lightskyblue") +
#   #   geom_sf(data=myFish) +
#   #   labs(title="Fish") 

#   # ggplot() +
#   #   geom_sf(data=lake, fill="lightskyblue") +
#   #   #geom_sf(data=st_buffer(myAnglers, 20), fill="red", alpha=0.25) +
#   #   geom_point(data=myAnglers %>% st_coordinates() %>% data.frame(), aes(x=X, y=Y),  color="black", size=0.5) +
#   #   #geom_sf(data=myFish, color="black", size=1.5) +
#   #   labs(title="Anglers")

#   # ggplot() +
#   #   geom_sf(data=lake, fill="navy") +
#   #   geom_sf(data=myCasts) +
#   #   geom_sf(data=myFish, color="red", size=1.5)

#   # ggplot() +
#   #   geom_sf(data=myInteractions[c(1,3),]) +
#   #   geom_sf(data=myCasts[myCasts %>% filter(anglerId %in% unique(myInteractions$anglerId) & castId %in% unique(myInteractions$castId)),])
   
#   # ggplot() +
#   #   #geom_sf(data=lakes_round_base, fill="lightskyblue") +
#   #   geom_sf(data=myCasts %>% filter(anglerId == myCasts[3195,]$anglerId)) +
#   #   geom_sf(data=st_buffer(myFish[tmp %>% lengths>0,],1), fill="red", color="transparent", alpha=0.4) +
#   #   geom_sf(data=myFish[tmp %>% lengths>0,], size=1, color="black") 
