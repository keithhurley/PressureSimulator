
# lake object -------------------------------------------------------------

obj_create_lake_object<-function(lake_object=NA, #allows to modify existing lake object
                             lakeGeom_path=NA,
                             restrictionsShore_path=NA,
                             restrictionsBoat_path=NA,
                             probsShore_path=NA,
                             probsBoat_path=NA){
  
  if (is.na(lake_object)) {
    myLake<-list()
    myLake$lakeGeom=NA
    myLake$lakeName=NA
    myLake$lakeAcres=NA
    myLake$restrictionsShore=NA
    myLake$restrictionsBoat=NA
    myLake$probsShore=NA
    myLake$probsBoat=NA
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
  
  if (!is.na(restrictionsBoat_path)) {
    load(file=restrictionsBoat_path)
    myLake$restrictionsBoat<-lake_restictions_boat
    rm(lake_restictions_boat)
  }
  
  if (!is.na(probsShore_path)) {
    load(file=probsShore_path)
    myLake$probsShore<-lake_probs_shore
    rm(lake_probs_shore)
  }

  if (!is.na(probsBoat_path)) {
    load(file=probsBoat_path)
    myLake$probsBoat<-lake_probs_boat
    rm(lake_probs_boat)
  }
  
  return(myLake)
}

obj_create_default_lake_object<-function(){
  d<-obj_create_lake_object(lakeGeom_path = "./data/lakes/round_1/lake.rData",
                        restrictionsShore_path = "./data/lakes/round_1/restrictions/shore/70percent.rData")
  return(d)
}


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
                                   anglerBoatDistribution,
                                   anglerBankDistribution,
                                   anglerBoatPartyRadius,
                                   anglerBankPartyRadius,
                                   meanPartySizeBoat,
                                   maxPartySizeBoat,
                                   meanPartySizeBank,
                                   maxPartySizeBank,
                                   boatShorelineBuffer,
                                   anglerBankRestrictions,
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
    myParamObject$totalAnglers<-round((hoursPerAcre*acres)/tripLengthMean, 0)
    myParamObject$bankAnglers<-myParamObject$totalAnglers*(percentBank/100)
    myParamObject$boatAnglers<-myParamObject$totalAnglers*((100-percentBank)/100)
    myParamObject$projectedNumberOfCasts<-(hoursPerAcre*acres)*castsPerHourMean
    
    return(myParamObject)
  }


#create default parameter object
obj_create_default_parameters_object<-function(){
  d<-obj_create_parameters_object(
    acres=1,
    hoursPerAcre=100,
    tripLengthMean=3.4,
    tripLengthSd=1,
    castsPerHourMean=60,
    castsPerHourSd=20,
    castDistanceMean=7,
    castDistanceSd=3,
    numberFish=100,
    fishShorelineBuffer=0.5,
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
                                    seed)
  {
  
  mySimsObject<-list()
  
  mySimsObject$seed=seed
  mySimsObject$numberSimulations=numberSimulations
  mySimsObject$runName=runName
  mySimsObject$runDescription=runDescription
  mySimsObject$saveNamePath=saveNamePath
  mySimsObject$saveNameBase=saveNameBase
  return(mySimsObject)
  
}

obj_create_default_simulations_object<-function(){
  d<-obj_create_simulations_object(numberSimulations=1,
                               runName="default",
                               runDescription="basic dev run",
                               saveNamePath="./outputs/",
                               saveNameBase="delete_me_dev",
                               seed=12345)
  return(d)
  }


# output object -----------------------------------------------------------


