#setwd("C:/Users/keith.hurley/Documents/GitHub/PressureSimulator")
source("./SimulationScripts/Functions_prep.R")

#load lake object
myLakeObject<-obj_create_default_lake_object()

#create parameter object
myParamsObject<-obj_create_default_parameters_object()

#create simulations object
mySimsObject<-obj_create_default_simulations_object()

#run simulations
tic("run MyResults")
myResults<-sims_runSimulations(myLakeObject=myLakeObject,
                myParamsObject=myParamsObject,
                mySimsObject=mySimsObject)
toc()

myResults$interactionCounts


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








  ggplot() +
    geom_sf(data=myLakeObject$lakeGeom, fill="lightskyblue") +
    geom_sf(data=myResults$myFish %>% st_as_sf()) +
    labs(title="Fish")

  ggplot() +
    geom_sf(data=myLakeObject$lakeGeom, fill="lightskyblue") +
    #geom_sf(data=st_buffer(myAnglers, 20), fill="red", alpha=0.25) +
    geom_point(data=myResults$myAnglers %>% st_coordinates() %>% data.frame(), aes(x=X, y=Y),  color="black", size=0.75) +
    #geom_sf(data=myFish, color="black", size=1.5) +
    labs(title="Anglers")

  ggplot() +
    geom_sf(data=myLakeObject$lakeGeom, fill="navy") +
    geom_sf(data=myResults$myCasts) +
    geom_sf(data=myResults$myFish %>% st_as_sf(), color="red", size=1.5)

  ggplot() +
    #geom_sf(data=myResults$myInteractions[c(1,3),]) +
    geom_sf(data=myResults$myCasts[myResults$myCasts %>% filter(anglerId %in% unique(myResults$myInteractions$anglerId) & castId %in% unique(myResults$myInteractions$castId)),])
   
