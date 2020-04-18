create_lake_object<-function(lakeGeom,
                             lakeName,
                             lakeAcres,
                             restrictionsShore=NA,
                             restrictionsBoat=NA,
                             probsShore=NA,
                             probsBoat=NA){
  
  myLake<-list()
  myLake$lakeGeom=lakeGeom
  myLake$lakeName
  myLake$lakeAcres
  myLake$restrictionsShore
  myLake$restrictionsBoat
  myLake$probsShore
  myLake$probsBoat
  
  return(myLake)
}

create_parameters_object<-function(randomSeed,
                                   acres,
                                   hoursPerAcre,
                                   tripLengthMean,
                                   tripLengthSd,
                                   castsPerHourMean,
                                   castsPerHourSd,
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
                                   percentBank,
                                   meanCastDistance,
                                   sdCastDistance,
                                   meanCastsPerHour,
                                   sdCastsPerHour) 
{
  myParamObject<-list()
  
  myParamObject$randomSeed=randomSeed
  myParamObject$acres=acres 
  myParamObject$hoursPerAcre=hoursPerAcre 
  myParamObject$tripLengthMean=tripLengthMean 
  myParamObject$tripLengthSd=tripLengthSd 
  myParamObject$castsPerHourMean=castsPerHourMean 
  myParamObject$castsPerHourSd=castsPerHourSd 
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
  myParamObject$meanCastDistance=meanCastDistance 
  myParamObject$sdCastDistance=sdCastDistance 
  myParamObject$meanCastsPerHour=meanCastsPerHour 
  myParamObject$sdCastsPerHour=sdCastsPerHour
  
  #create pressure params
  myParamObject$totalAnglers<-round((hoursPerAcre*acres)/tripLengthMean, 0)
  myParamObject$bankAnglers<-myParamObject$totalAnglers*(percentBank/100)
  myParamObject$boatAnglers<-myParamObject$totalAnglers*((100-percentBank)/100)
  myParamObject$projectedNumberOfCasts<-(hoursPerAcre*acres)*castsPerHourMean
  
  return(myParamObject)
}
