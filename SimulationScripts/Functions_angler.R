require(sf)
require(tidyverse)
require(tictoc)
require(foreach)
library(doParallel)


#angler distributions:
#  1) random - assumes single angler party
#  2) clustered - clusters by party size

anglers_distributeAnglersIntoParties<-function(numberAnglers=1000,
                                  meanPartySize=2,
                                  maxPartySize=4,
                                  mySeed){
  #set seed
  set.seed(round(mySeed*55,0))
  
  #create parties of anglers
  numberOfParties=round(numberAnglers/meanPartySize)
  parties<-data.frame(partyId=1:numberOfParties) 
  
  #do next step in one call to rnorm for efficiency...not in mutate statement
  parties$numberInParty=round(rpois(nrow(parties), meanPartySize),0)
  
  #eliminate anything under zero and over maxPartySize
  parties <- parties %>% 
    filter(numberInParty>0) %>%
    filter(numberInParty<=maxPartySize)
  
  sum(parties$numberInParty)
  table(parties$numberInParty)
  
  #trim total numbers to no more than requested number of anglers
  while (sum(parties$numberInParty)>numberAnglers) {
    parties <- parties[-nrow(parties),]
  }
  
  sum(parties$numberInParty)
  
  #add parties to get to total number of anglers
    #calculate how many parties of mean size can be used and add them
    currentSum<-sum(parties$numberInParty)
    if (numberAnglers-currentSum>=round(meanPartySize,0)) {
      numberNeeded=floor((numberAnglers-currentSum)/round(meanPartySize,0))
      parties<-rbind(parties,
                     data.frame(partyId=c(seq(from=max(parties$partyId)+1,
                                              to=max(parties$partyId)+numberNeeded, 
                                              by=1)),
                       numberInParty=rep(round(meanPartySize,0),numberNeeded)))
    } 

    sum(parties$numberInParty)
    
    #if short less than one mean party size...add parties of one until you get there
    while(sum(parties$numberInParty)<numberAnglers){
      parties<-rbind(parties,
                     c(partyId=max(parties$partyId)+1,
                       numberInParty=1))
    }
    
    sum(parties$numberInParty)
    table(parties$numberInParty)
    
    parties<-parties %>%
      select(-partyId) %>% 
      rowid_to_column(var="partyId")
    
    return(parties)
    
}


anglers_place_bank<-function(lakeGeom, 
                             numberAnglers=100,
                             meanPartySizeBank=2.3,
                             maxPartySizeBank=4,
                             anglerBankDistribution="Random", 
                             anglerBankRestrictions, 
                             anglerBankProbs,
                             mySeed){
  
  #set seed
  set.seed(round(mySeed*0.5475/0.213234,0))
  
  if (anglerBankDistribution=="Random") {
    myAnglers<-lakeGeom %>% 
      st_cast("LINESTRING") %>% 
      st_line_sample(n=numberAnglers, type="random")
    myAnglers<-myAnglers %>% st_cast("POINT")
    myAnglers<-st_set_crs(myAnglers, 5514)
    myAnglers<-st_cast(myAnglers) %>%
      as.data.frame() %>%
      st_as_sf(crs = 5514) %>%
      rownames_to_column("partyId")
    myAnglers$anglerType="Bank"
  }
  
  if (anglerBankDistribution=="Clustered") {

  #create parties of anglers
  partyList<-anglers_distributeAnglersIntoParties(numberAnglers=numberAnglers,
                                                  meanPartySize=meanPartySizeBank,
                                                  maxPartySize = maxPartySizeBank,
                                                  mySeed=mySeed)
  
  
  #convert lake polygon to a linestring to get just bank
  lakeGeom_line<-lakeGeom %>% 
    st_cast("LINESTRING") 
  
  #get random points for each bank party
    myAnglers<-lakeGeom_line %>% 
    st_line_sample(n=nrow(partyList), type="random")
  
  myAnglers<-myAnglers %>% 
    st_cast("POINT") %>%
    st_set_crs(5514) %>% 
    st_cast() %>%
    as.data.frame() %>%
    st_as_sf(crs = 5514) %>%
    rownames_to_column("partyId") %>%
    mutate(partyId=as.integer(partyId)) %>%
    mutate(anglerType="Bank") %>%
    left_join(partyList)
  
  #alter row location so subsequent anglers in a party have a different location
  #...within a buffered circle (i.e. a boat)
  #MUST PARELLALIZE this for st_buffer
  cl<-makeCluster(2)
  registerDoParallel(cl)  
  myAnglers2<-foreach(i=1:nrow(myAnglers), .combine="rbind") %dopar% {
    require(dplyr)
    require(sf)
    #don't do expensive buffer and intersection calculations when not necessary
    if(myAnglers$numberInParty[i]==1){
      myTmp<-myAnglers$geometry[i] %>%
        as.data.frame() %>%
        st_as_sf(crs=5514)
    } else {
    myTmp<-st_buffer(myAnglers[i,], 3 * myAnglers$numberInParty[i])  
    myTmp<-st_intersection(lakeGeom_line, myTmp)
    myTmp<-myTmp %>% 
      st_line_sample(n=myAnglers$numberInParty[i], type="random") %>% 
      st_cast("POINT") %>%
      st_set_crs(5514) %>% 
      st_cast() %>%
      as.data.frame() %>%
      st_as_sf(crs = 5514)
    }
    return(myTmp)
  }
  stopCluster(cl)
  
  myAnglers2<-myAnglers2 %>%
    st_as_sf() %>%
    bind_cols(partyList[rep(1:nrow(partyList), partyList$numberInParty),] %>% 
                group_by(partyId) %>% 
                mutate(partyAnglerId=row_number()) %>%
                ungroup() %>%
                mutate(anglerId=row_number()))
  
  #by resampling partyAngler #1 it "shifts" the whole boat, potentially outside
  #the boundaries of the lake/area, therefore replace the original partyAngler #1 coordinates
  myAnglers2[myAnglers2$partyAnglerId==1,]$geometry<-myAnglers$geometry

  # ggplot() +
  #   geom_sf(data=lakeGeom)+
  #   geom_sf(data=st_buffer(myAnglers2 %>% filter(partyAnglerId==1),50), fill="red", alpha=.5, color="red") +
  #   geom_sf(data=myAnglers2, size=1)
  
  myAnglers<-myAnglers2
  myAnglers$anglerType="Bank"
  
}
  
  return(myAnglers)
}


anglers_place_boat<-function(lakeGeom, 
                             numberAnglers=1000,
                             meanPartySizeBoat=2,
                             maxPartySizeBoat=4,
                             anglerBoatDistribution="Random", 
                             anglerBoatRestrictions=NA, 
                             anglerBoatProbs=NA,
                             mySeed){
  
  #set seed
  set.seed(round(mySeed*0.356/0.85324,0))
  
  if (anglerBoatDistribution=="Random") {
    myAnglers<-st_sample(st_buffer(lakeGeom, -10), size=numberAnglers) %>%
      as.data.frame() %>%
      st_as_sf(crs = 5514) %>%
      rownames_to_column("partyId")
    myAnglers$anglerType="Boat"
  }
  
  if (anglerBoatDistribution=="Clustered") {
    
    #create parties of anglers
    partyList<-anglers_distributeAnglersIntoParties(numberAnglers=numberAnglers,
                                         meanPartySize=meanPartySizeBoat,
                                         maxPartySize = maxPartySizeBoat,
                                         mySeed=mySeed)

    
    #get random points for each boat
    myAnglers<-st_buffer(lakeGeom, -10) %>%
      st_sample(size=nrow(partyList)) %>%
      as.data.frame() %>%
      st_as_sf(crs = 5514) %>%
      rownames_to_column("partyId") %>%
      mutate(partyId=as.integer(partyId)) %>%
      mutate(anglerType="Boat") %>%
      left_join(partyList)

    
  #alter row location so subsequent anglers in a party have a different location
  #...within a buffered circle (i.e. a boat)
  #MUST PARELLALIZE this for st_buffer
  cl<-makeCluster(2)
  registerDoParallel(cl)  
  myAnglers2<-foreach(i=1:nrow(myAnglers), .combine="rbind") %dopar% {
    require(dplyr)
    require(sf)
    
    #don't do expensive buffer and intersection calculations when not necessary
    if(myAnglers$numberInParty[i]==1){
      myTmp<-myAnglers$geometry[i] %>%
        as.data.frame() 
    } else {
    myTmp<-st_buffer(myAnglers[i,], 5) %>% 
      st_sample(size=myAnglers[i,]$numberInParty) %>%
      data.frame()
    }
    return(myTmp)
    }
  stopCluster(cl)

  myAnglers2<-myAnglers2 %>%
    st_as_sf() %>%
    bind_cols(partyList[rep(1:nrow(partyList), partyList$numberInParty),] %>% 
        group_by(partyId) %>% 
        mutate(partyAnglerId=row_number()) %>%
        ungroup() %>%
        mutate(anglerId=row_number()))

  #by resampling partyAngler #1 it "shifts" the whole boat, potentially outside
  #the boundaries of the lake/area, therefore replace the original partyAngler #1 coordinates
  myAnglers2[myAnglers2$partyAnglerId==1,]$geometry<-myAnglers$geometry
                 
  ggplot() +
    geom_sf(data=lakeGeom)+ 
    geom_sf(data=st_buffer(myAnglers2 %>% filter(partyAnglerId==1),5), fill="red", alpha=.5, color="red") +
    geom_sf(data=myAnglers2, size=1) 
                
  myAnglers<-myAnglers2
  myAnglers$anglerType="Boat"
  
  }
  return(myAnglers)
}

anglers_assign_method<-function(tmpAnglers=myBankAnglers,
                                percentLure=50,
                                mySeed) {
  
  #calculate percent bait anglers
  percentLure<-(percentLure/100)
  percentBait<-((100-percentLure)/100)
  
  #set seed
  set.seed<-round(mySeed*0.12345/0.2134,0)
  
  #assign methods
  tmpAnglers$anglerMethod<-sample(c("Lure", "Bait"), size=nrow(tmpAnglers), replace=TRUE, prob=c(percentLure, percentBait))
  
  #return
  return(tmpAnglers)
}

anglers_place<-function(lakeGeom,
                        totalAnglers,
                        percentBank,
                        meanPartySizeBank=2.3,
                        meanPartySizeBoat,
                        maxPartySizeBoat,
                        maxPartySizeBank=10,
                        anglerBankDistribution="Random",
                        anglerBoatDistribution="Random",
                        anglerBankRestrictions=NA,
                        anglerBankProbs=NA,
                        anglerBoatRestrictions=NA,
                        anglerBoatProbs=NA,
                        anglerBankLureProb=100,
                        anglerBoatLureProb=100,
                        mySeed){
  
  #calculate remaining precentages
  percentBoat<-(100-percentBank)

  #create dataset of bank anglers with starting position
  if(percentBank>0){
    myBankAnglers<-anglers_place_bank(lakeGeom,
                                  numberAnglers=totalAnglers*(percentBank/100),
                                  meanPartySizeBank=meanPartySizeBank,
                                  maxPartySizeBank=maxPartySizeBank,
                                  anglerBankDistribution = anglerBankDistribution,
                                  anglerBankRestrictions = anglerBankRestrictions,
                                  anglerBankProbs=anglerBankProbs,
                                  mySeed)
    #assign angler method types
    myBankAnglers<-anglers_assign_method(myBankAnglers, 
                                         anglerBankLureProb-50,
                                         mySeed)
  } else {
    myBankAnglers<-NA
  }
  
  #create dataset of boat anglers with starting position
  if(percentBoat>0){
    myBoatAnglers<-anglers_place_boat(lakeGeom,
                                      numberAnglers=totalAnglers*(percentBoat/100),
                                      meanPartySizeBoat=meanPartySizeBoat,
                                      maxPartySizeBoat=maxPartySizeBoat,
                                      anglerBoatDistribution=anglerBoatDistribution,
                                      anglerBoatRestrictions = anglerBoatRestrictions,
                                      anglerBoatProbs=anglerBoatProbs,
                                      mySeed)
    #assign angler method types
    myBoatAnglers<-anglers_assign_method(myBoatAnglers, 
                                         anglerBoatLureProb-50, 
                                         mySeed)
  } else {
    myBoatAnglers<-NA
  }
  
  #alter starting points for anglers on the move
  
  #combine angler datasets
  if(any(!is.na(myBankAnglers)) & any(!is.na(myBoatAnglers))){
    myAnglers<-rbind(myBankAnglers %>% select(partyId, anglerType, anglerMethod), myBoatAnglers %>% select(partyId,anglerType, anglerMethod))
  } else if (any(is.na(myBankAnglers)) & any(!is.na(myBoatAnglers))) {
    myAnglers<-myBoatAnglers %>% select(partyId, anglerType, anglerMethod)
  } else if (any(!is.na(myBankAnglers)) & any(is.na(myBoatAnglers))) {
    myAnglers<-myBankAnglers %>% select(partyId, anglerType, anglerMethod)
  }
  
  #add angler Id number
  myAnglers<-myAnglers %>% 
    mutate(anglerId=row_number())
  
  #return
  return(myAnglers)
  
}
