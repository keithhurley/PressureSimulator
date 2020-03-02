require(sf)
require(tidyverse)

#angler distributions:
#  1) random - assumes single angler party
#  2) clustered - clusters by party size



anglers_place_bank<-function(lakeGeom, 
                             numberAnglers=100,
                             anglerBankDistribution="random", 
                             anglerBankRestrictions, 
                             anglerBankProbs){
  
  #set seed
  set.seed(round(myRandomSeed*0.5475/0.213234,0))
  
  if (anglerBankDistribution=="random") {
    myAnglers<-lakes_round_base %>% st_cast("LINESTRING") %>% st_line_sample(n=numberAnglers, type="random")
    myAnglers<-myAnglers %>% st_cast("POINT")
    myAnglers<-st_set_crs(myAnglers, 5514)
    myAnglers<-st_cast(myAnglers) %>%
      as.data.frame() %>%
      st_as_sf(crs = 5514)
    myAnglers$anglerType="Bank"
  }
  
  return(myAnglers)
}


anglers_place_boat<-function(lakeGeom, 
                             numberAnglers=100,
                             anglerBoatDistribution="random", 
                             anglerBoatRestrictions, 
                             anglerBoatProbs){
  
  #set seed
  set.seed(round(myRandomSeed*0.356/0.85324,0))
  
  if (anglerBoatDistribution=="random") {
    myAnglers<-st_sample(st_buffer(lakeGeom, -100), size=numberAnglers) %>%
      as.data.frame() %>%
      st_as_sf(crs = 5514) 
    myAnglers$anglerType="Boat"
  }
  
  return(myAnglers)
}

anglers_assign_method<-function(tmpAnglers=myBankAnglers,
                                percentLure=50) {
  
  #calculate percent bait anglers
  percentLure<-(percentLure/100)
  percentBait<-((100-percentLure)/100)
  
  #set seed
  set.seed<-round(myRandomSeed*0.12345/0.2134,0)
  
  #assign methods
  tmpAnglers$anglerMethod<-sample(c("Lure", "Bait"), size=nrow(tmpAnglers), replace=TRUE, prob=c(percentLure, percentBait))
  
  #return
  return(tmpAnglers)
}

anglers_place<-function(lakeGeom,
                        totalAnglers=100,
                        percentBank=100,
                        anglerBankDistribution="random",
                        anglerBoatDistribution="random",
                        anglerBankRestrictions=NA,
                        anglerBankProbs=NA,
                        anglerBoatRestrictions=NA,
                        anglerBoatProbs=NA,
                        anglerBankLureProb=100,
                        anglerBoatLureProb=100){
  
  #calculate remaining precentages
  percentBoat<-(100-percentBank)

  #create dataset of bank anglers with starting position
  if(percentBank>0){
    myBankAnglers<-anglers_place_bank(lakeGeom,
                                  numberAnglers=totalAnglers*(percentBank/100),
                                  anglerBankDistribution = "random",
                                  anglerBankRestrictions = anglerBankRestrictions,
                                  anglerBankProbs=anglerBankProbs)
    #assign angler method types
    myBankAnglers<-anglers_assign_method(myBankAnglers, anglerBankLureProb-50)
  } else {
    myBankAnglers<-NA
  }
  
  #create dataset of boat anglers with starting position
  if(percentBoat>0){
    myBoatAnglers<-anglers_place_boat(lakeGeom,
                                      numberAnglers=totalAnglers*(percentBoat/100),
                                      anglerBoatDistribution = "random",
                                      anglerBoatRestrictions = anglerBoatRestrictions,
                                      anglerBoatProbs=anglerBoatProbs)
    #assign angler method types
    myBoatAnglers<-anglers_assign_method(myBoatAnglers, anglerBoatLureProb-50)
  } else {
    myBoatAnglers<-NA
  }
  
  #alter starting points for anglers on the move
  
  #combine angler datasets
  if(any(!is.na(myBankAnglers)) & any(!is.na(myBoatAnglers))){
    myAnglers<-rbind(myBankAnglers %>% select(anglerType, anglerMethod), myBoatAnglers %>% select(anglerType, anglerMethod))
  } else if (any(is.na(myBankAnglers)) & any(!is.na(myBoatAnglers))) {
    myAnglers<-myBoatAnglers
  } else if (any(!is.na(myBankAnglers)) & any(is.na(myBoatAnglers))) {
    myAnglers<-myBankAnglers
  }
  
  #add angler Id number
  myAnglers<-myAnglers %>% 
    mutate(anglerId=row_number())
  
  #return
  return(myAnglers)
  
}
