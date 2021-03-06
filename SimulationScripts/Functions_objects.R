
# lake object -------------------------------------------------------------

obj_create_lake_object<-function(lake_object=NA, #allows to modify existing lake object
                             lakeGeom_path=NA,
                             restrictionsShore_path=NA,
                             restrictionsLake_path=NA,
                             restrictionsFish_path=NA,
                             probsShore_path=NA,
                             probsLake_path=NA,
                             probsFish_path=NA){
  
  if (is.na(lake_object)) {
    myLake<-list()
    myLake$lakeGeom=NA
    myLake$lakeName=NA
    myLake$lakeAcres=NA
    myLake$restrictionsShore=NA
    myLake$restrictionsLake=NA
    myLake$restrictionsFish=NA
    myLake$probsShore=NA
    myLake$probsLake=NA
    myLake$probsFish=NA
  }
  
  
  if (!is.na(lakeGeom_path)) {
    load(file=lakeGeom_path)
    myLake$lakeGeom=lake
    myLake$lakeName=lake$name[1]
    myLake$lakeAcres=as.numeric(round(st_area(lake)/4046.86,1))
    rm(lake)
  }

  if (!is.na(restrictionsShore_path)) {
    load(file=restrictionsShore_path)
    myLake$restrictionsShore<-lake_restrictions_shore
    rm(lake_restrictions_shore)
  }

  if (!is.na(restrictionsLake_path)) {
    load(file=restrictionsLake_path)
    myLake$restrictionslake<-lake_restrictions_lake
    rm(lake_restrictions_lake)
  }

  if (!is.na(restrictionsFish_path)) {
    load(file=restrictionsFish_path)
    myLake$restrictionsFish<-lake_restrictions_fish
    rm(lake_restrictions_fish)
  }

  if (!is.na(probsShore_path)) {
    load(file=probsShore_path)
    myLake$probsShore<-lake_probs_shore
    rm(lake_probs_shore)
  }

  if (!is.na(probsLake_path)) {
    load(file=probsLake_path)
    myLake$probsLake<-lake_probs_lake
    rm(lake_probs_lake)
  }
  
  if (!is.na(probsFish_path)) {
    load(file=probsFish_path)
    myLake$probsFish<-lake_probs_fish
    rm(lake_probs_fish)
  }
    
  return(myLake)
}

obj_create_default_lake_object<-function(){
  d<-obj_create_lake_object(lakeGeom_path = "./data/lakes/round_1/lake.rData",
                        restrictionsShore_path = "./data/lakes/round_1/restrictions/shore/Shore_Restrictions.rData",
                        probsShore_path = "./data/lakes/round_1/probs/shore/OverlappingProbs.rData",
                        restrictionsLake_path = "./data/lakes/round_1/restrictions/lake/LakeRestrictions1.rData",
                        probsLake_path = "./data/lakes/round_1/probs/lake/LakeProbs1Split.rData",
                        restrictionsFish_path = NA,
                        probsFish_path = NA)
  return(d)
}

# obj_create_default_lake_object<-function(){
#   d<-obj_create_lake_object(lakeGeom_path = "./data/lakes/round_1/lake.rData")
#   return(d)
# }
# parameters object -------------------------------------------------------

#create parameter object
obj_create_parameters_object<-function(acres,
                                   hoursPerAcre,
                                   tripLengthMean,
                                   tripLengthSd,
                                   castsPerHourMean,
                                   castsPerHourSd,
                                   castDistanceMean,
                                   castDistanceSd,
                                   numberFish,
                                   fishShorelineBuffer,
                                   fishDistribution,
                                   fishMeanNumberPerSchool,
                                   fishSchoolingDistance,
                                   anglerBoatDistribution,
                                   anglerBankDistribution,
                                   anglerBoatPartyRadius,
                                   anglerBankPartyRadius,
                                   meanPartySizeBoat,
                                   maxPartySizeBoat,
                                   meanPartySizeBank,
                                   maxPartySizeBank,
                                   boatShorelineBuffer,
                                   percentBank) 
  {
    myParamObject<-list()
    
    myParamObject$acres=acres 
    myParamObject$hoursPerAcre=hoursPerAcre 
    myParamObject$tripLengthMean=tripLengthMean 
    myParamObject$tripLengthSd=tripLengthSd 
    myParamObject$castsPerHourMean=castsPerHourMean 
    myParamObject$castsPerHourSd=castsPerHourSd 
    myParamObject$castDistanceMean=castDistanceMean 
    myParamObject$castDistanceSd=castDistanceSd
    myParamObject$numberFish =numberFish
    myParamObject$fishShorelineBuffer =fishShorelineBuffer
    myParamObject$fishDistribution=fishDistribution
    myParamObject$fishMeanNumberPerSchool=fishMeanNumberPerSchool
    myParamObject$fishSchoolingDistance=fishSchoolingDistance
    myParamObject$anglerBoatDistribution=anglerBoatDistribution 
    myParamObject$anglerBankDistribution=anglerBankDistribution 
    myParamObject$anglerBoatPartyRadius=anglerBoatPartyRadius 
    myParamObject$anglerBankPartyRadius=anglerBankPartyRadius 
    myParamObject$meanPartySizeBoat=meanPartySizeBoat 
    myParamObject$maxPartySizeBoat=maxPartySizeBoat 
    myParamObject$meanPartySizeBank=meanPartySizeBank 
    myParamObject$maxPartySizeBank=maxPartySizeBank 
    myParamObject$boatShorelineBuffer=boatShorelineBuffer 
    myParamObject$percentBank=percentBank 


    #create pressure params
    myParamObject$totalAnglers<-ceiling((hoursPerAcre*acres)/tripLengthMean)
    myParamObject$bankAnglers<-ceiling(myParamObject$totalAnglers*(percentBank/100))
    myParamObject$boatAnglers<-floor(myParamObject$totalAnglers*((100-percentBank)/100))
    myParamObject$projectedNumberOfCasts<-(hoursPerAcre*acres)*castsPerHourMean
    
    return(myParamObject)
  }


#create default parameter object
obj_create_default_parameters_object<-function(){
  d<-obj_create_parameters_object(
    acres=1,
    hoursPerAcre=100,
    tripLengthMean=3,
    tripLengthSd=1,
    castsPerHourMean=60,
    castsPerHourSd=20,
    castDistanceMean=7,
    castDistanceSd=3,
    numberFish=100,
    fishShorelineBuffer=0.5,
    fishDistribution = "Schooling",
    fishMeanNumberPerSchool=10,
    fishSchoolingDistance=0.5,
    anglerBoatDistribution="Clustered By Party",
    anglerBankDistribution="Clustered By Party",
    anglerBoatPartyRadius=1.5,
    anglerBankPartyRadius=3,
    meanPartySizeBoat=2,
    maxPartySizeBoat=4,
    meanPartySizeBank=2.2,
    maxPartySizeBank=5,
    boatShorelineBuffer=5,
    percentBank=50
  )
  
  return(d)
}


#
# simulations object ------------------------------------------------------

obj_create_simulations_object<-function(numberSimulations,
                                    runName,
                                    runDescription,
                                    saveNamePath,
                                    saveNameBase,
                                    seed,
                                    parGroupSize=10, #must be at least 3?
                                    parNumberCores=4 
)
  {
  
  mySimsObject<-list()
  
  mySimsObject$seed=seed
  mySimsObject$numberSimulations=numberSimulations
  mySimsObject$runName=runName
  mySimsObject$runDescription=runDescription
  mySimsObject$saveNamePath=saveNamePath
  mySimsObject$saveNameBase=saveNameBase
  mySimsObject$parGroupSize=parGroupSize
  mySimsObject$parNumberCores=parNumberCores
  return(mySimsObject)
  
}

obj_create_default_simulations_object<-function(){
  d<-obj_create_simulations_object(numberSimulations=10,
                               runName="default",
                               runDescription="basic dev run",
                               saveNamePath="./outputs/",
                               saveNameBase="delete_me_dev",
                               seed=12345,
                               parGroupSize=10, #must be at least 3?
                               parNumberCores=12)
  return(d)
  }


# output object -----------------------------------------------------------


