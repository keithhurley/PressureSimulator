#angler distributions:
#  1) random - assumes single angler party
#  2) clustered - clusters by party size

anglers_distributeAnglersIntoParties<-function(numberAnglers=1000,
                                  meanPartySize=2,
                                  maxPartySize=4,
                                  mySeed,
                                  numberSims=500,
                                  parNumberCores){

  #set seed
  set.seed(round(mySeed*55,0))
  
  #create parties of anglers
  numberOfParties=round(numberAnglers/meanPartySize)*numberSims
  
  #adjust if number of parties is 0
  if(numberOfParties==0){
    numberAnglers=1
    
    numberOfParties=1*numberSims
  
    parties<-data.frame(simId=sort(rep(1:numberSims, numberOfParties/numberSims))) %>%
      group_by(simId) %>%
      mutate(partyId=row_number()) %>%
      ungroup()
    
    parties$numberInParty<-1
  } else { 
      parties<-data.frame(simId=sort(rep(1:numberSims, numberOfParties/numberSims))) %>%
        group_by(simId) %>%
        mutate(partyId=row_number()) %>%
        ungroup()

      #do next step in one call to rnorm or rpois for efficiency...not in mutate statement
      parties$numberInParty=round(rpois(nrow(parties), meanPartySize),0)
  }
  
  #eliminate anything under zero and over maxPartySize
  #convert 0 parties to 1
  #convert over parties to max party size
  parties <- parties %>% 
    mutate(numberInParty=ifelse(numberInParty==0, 1, numberInParty)) %>%
    mutate(numberInParty=ifelse(numberInParty>maxPartySize, maxPartySize, numberInParty))

  #parties %>% group_by(simId) %>% summarise(tot=sum(numberInParty))
  #sum(parties$numberInParty)
  #table(parties$numberInParty)

  #trim total numbers to no more than requested number of anglers
  parties<-parties %>% mutate(rowId=row_number())

  for(i in 1:numberSims)  {
    while (sum(parties$numberInParty[parties$simId==i])>numberAnglers) {
      parties <- parties[!(parties$rowId==max(parties$rowId[parties$simId==i])),] 
      }
  }
  parties<-parties %>% select(-rowId)

  #add parties to get to total number of anglers

  suppressMessages({
  print(paste(getDoParWorkers(), " Cores Will Be Used For Creating Parties", sep=""))

  cl<-makeCluster(parNumberCores, type="PSOCK")
  registerDoParallel(cl)
  registerDoRNG(round(mySeed*165,0))
  
  #sort so they can resort following dopar
  parties<-parties %>% arrange(simId, partyId)
  
  parties2<-foreach(i = 1:numberSims,.combine="rbind") %dopar%  {
    require(dplyr)

    tmpParties<-parties[parties$simId==i,]
    #calculate how many parties of mean size can be used and add them
    currentSum<-sum(tmpParties$numberInParty)
    if (numberAnglers-currentSum>=round(meanPartySize,0)) {
      numberNeeded=floor((numberAnglers-currentSum)/round(meanPartySize,0))
      tmpParties<-rbind(tmpParties,
                     data.frame(simId=i,
                                partyId=c(seq(from=max(tmpParties$partyId)+1,
                                              to=max(tmpParties$partyId)+numberNeeded,
                                              by=1)),
                                numberInParty=rep(round(meanPartySize,0),numberNeeded)))
    }
    #if short less than one mean party size...add parties of one until you get there
    while(sum(tmpParties$numberInParty[tmpParties$simId==i])<numberAnglers){
      tmpParties<-rbind(tmpParties,
                     c(simId=i,
                       partyId=max(tmpParties$partyId)+1,
                       numberInParty=1))
    }
                      
    return(tmpParties)
  }
  
stopCluster(cl)
})
  
  parties<-parties2
  rm(parties2)
  
  #sort following dopar
  parties<-parties %>% arrange(simId, partyId)
  
  #renumber parties so they are consecutive
  parties<-parties %>%
    select(-partyId) %>% 
    group_by(simId) %>%
    mutate(partyId=row_number())
  
  #numberSims=3000
  return(parties)
}


anglers_place_bank<-function(lakeGeom,
                             lakeName, 
                             numberAnglers,
                             meanPartySizeBank,
                             maxPartySizeBank,
                             anglerBankDistribution,
                             anglerBankPartyRadius,
                             anglerBankRestrictions=NA, 
                             anglerBankProbs=NA,
                             mySeed,
                             numberSims,
                             parGroupSize,
                             parNumberCores){
  

  #set seed
  set.seed(round(mySeed*0.5475/0.213234,0))
  
  suppressMessages({  
    
  #convert lake polygon to a linestring to get just bank
  if (anglerBankRestrictions == "None" | is.na(anglerBankRestrictions)) {
    lakeGeom_line<-lakeGeom %>% 
      st_cast("LINESTRING", warn=FALSE)
  }
  else {
    load(file=paste("../data/lakes/",lakeName,"/restrictions/shore/",anglerBankRestrictions, sep=""))
    lakeGeom_line<-lake_restrictions_shore %>%
      st_cast("LINESTRING", warn=FALSE)
    rm(lake_restrictions_shore)
  }
  
  

    
  if (anglerBankDistribution=="Random") {
    
    myAnglers<-lakeGeom_line %>% 
      st_line_sample(n=as.integer(numberAnglers)*numberSims, type="random")
    
    myAnglers<-myAnglers %>% st_cast("POINT", warn=FALSE)
    myAnglers<-st_set_crs(myAnglers, 6343)
    myAnglers<-st_cast(myAnglers, warn=FALSE) %>%
      as.data.frame() %>%
      st_as_sf(crs = 6343) %>%
      mutate(simId=sort(rep(1:numberSims, numberAnglers))) %>%
      group_by(simId) %>%
      mutate(partyId=row_number()) %>%
      ungroup()
    myAnglers$anglerType="Bank"
  }
  
  if (anglerBankDistribution=="Clustered By Party") {
    #create parties of anglers
    partyList<-anglers_distributeAnglersIntoParties(numberAnglers=numberAnglers,
                                                    meanPartySize=meanPartySizeBank,
                                                    maxPartySize = maxPartySizeBank,
                                                    mySeed=mySeed,
                                                    numberSims=numberSims,
                                                    parNumberCores = parNumberCores)
    #get random points for each bank party
    myAnglers<-lakeGeom_line %>% 
      st_line_sample(n=nrow(partyList), type="random") %>% 
      st_cast("POINT", warn=FALSE) %>%
      st_set_crs(6343) %>% 
      st_cast() %>%
      as.data.frame() %>%
      st_as_sf(crs = 6343) %>%
      bind_cols(partyList) 
    
    #alter row location so subsequent anglers in a party have a different location
    #...within a buffered circle (i.e. a boat)
    myAnglers_buf<-st_buffer(myAnglers, anglerBankPartyRadius * ifelse(myAnglers$numberInParty-1==0, 1, myAnglers$numberInParty))  

    suppressWarnings(myAnglers_segment<-st_intersection(myAnglers_buf, lakeGeom_line) %>% 
      st_cast("LINESTRING", warn=FALSE))

myAnglers2<-myAnglers %>% 
      st_drop_geometry() %>% 
      st_as_sf(geometry=st_line_sample(myAnglers_segment, n=myAnglers$numberInParty)) %>% 
      st_cast("POINT", warn=FALSE) %>%
      group_by(simId,partyId) %>% 
      mutate(partyAnglerId=row_number()) %>%
      ungroup() %>%
      mutate(anglerId=row_number())
    #by resampling partyAngler #1 it "shifts" the whole boat, potentially outside
    #the boundaries of the lake/area, therefore replace the original partyAngler #1 coordinates
    myAnglers2$geometry[myAnglers2$partyAnglerId==1]<-myAnglers$geometry
    
    # ggplot() +
    #   geom_sf(data=lakeGeom)+
    #   geom_sf(data=st_buffer(myAnglers[myAnglers$partyId==22,] %>% filter(partyAnglerId==1),12), fill="red", alpha=.5, color="red") +
    #   geom_sf(data=myAnglers[myAnglers$partyId==22,], size=1)
    myAnglers<-myAnglers2
    rm(myAnglers2)
    myAnglers$anglerType="Bank"
    

  }
  
  })
  
  return(myAnglers)
}


anglers_place_boat<-function(lakeGeom,
                             numberAnglers,
                             meanPartySizeBoat,
                             maxPartySizeBoat,
                             anglerBoatDistribution,
                             anglerBoatPartyRadius,
                             anglerBoatRestrictions=NA, 
                             anglerBoatProbs=NA,
                             boatShorelineBuffer,
                             mySeed,
                             numberSims=numberSims,
                             parGroupSize,
                             parNumberCores){
  
  #set seed
  set.seed(round(mySeed*0.356/0.85324,0))
  
  if (anglerBoatDistribution=="Random") {
    myAnglers<-st_sample(st_buffer(lakeGeom, (-1*boatShorelineBuffer)), size=as.integer(numberAnglers)*numberSims) %>%
      as.data.frame() %>%
      st_as_sf(crs = 6343) %>%
      mutate(simId=sort(rep(1:numberSims, numberAnglers))) %>%
      group_by(simId) %>%
      mutate(partyId=row_number()) %>%
      ungroup()
    myAnglers$anglerType="Boat"
  }
  
  if (anglerBoatDistribution=="Clustered By Party") {
    
    #create parties of anglers
    partyList<-anglers_distributeAnglersIntoParties(numberAnglers=numberAnglers,
                                         meanPartySize=meanPartySizeBoat,
                                         maxPartySize = maxPartySizeBoat,
                                         mySeed=mySeed,
                                         numberSims = numberSims,
                                         parNumberCores=parNumberCores)

    
    #get random points for each boat
    myAnglers<-st_buffer(lakeGeom, (-1*boatShorelineBuffer)) %>%
      st_sample(size=nrow(partyList)) %>%
      as.data.frame() %>%
      st_as_sf(crs = 6343) %>%
      bind_cols(partyList) %>%
      mutate(anglerType="Boat")


  #alter row location so subsequent anglers in a party have a different location
  #...within a buffered circle (i.e. a boat)
    suppressMessages({
      print(paste(getDoParWorkers(), " Cores Will Be Used For Placing Boat Anglers", sep=""))
      
      cl<-makeCluster(parNumberCores, type="PSOCK")
      registerDoParallel(cl)
      registerDoRNG(round(mySeed*412,0))
      
      #sort prior to dopar to ensure order
      myAnglers<-myAnglers %>% arrange(simId, partyId)
      
    myAnglers2<-foreach(i=seq(1,floor(nrow(myAnglers)/parGroupSize)*parGroupSize,by=parGroupSize), .combine="rbind") %dopar% {
      
      require(dplyr)
      require(sf)
      require(foreach)
      
      #do full groups using parallel processing
      k<-foreach(j=0:(parGroupSize-1), .combine="rbind") %do% {
        
        #don't do expensive buffer and intersection calculations when not necessary
        if(myAnglers$numberInParty[i+j]==1){
          myTmp<-myAnglers[i+j,] %>%
            mutate(simId=myAnglers$simId[i+j],
                   numberInParty=myAnglers$numberInParty[i+j],
                   partyId=myAnglers$partyId[i+j])  %>%
            select(simId, partyId, numberInParty)
        } else {
          myTmp<-st_buffer(myAnglers[i+j,], anglerBoatPartyRadius) %>% 
            st_sample(size=myAnglers[i+j,]$numberInParty) %>%
            data.frame()%>%
            st_as_sf(crs=6343) %>%
            mutate(simId=myAnglers$simId[i+j],
                   numberInParty=myAnglers$numberInParty[i+j],
                   partyId=myAnglers$partyId[i+j])  %>%
            select(simId, partyId, numberInParty)
        }
        return(myTmp)
      }
      
      return(k)
    }
    
    
    
    #do the last group which may or may not be partial
    #create proper number of for loop to do last items
    #get number of odd items of partial group
    o=nrow(myAnglers) - (floor(nrow(myAnglers)/parGroupSize)*parGroupSize)
    i=floor(nrow(myAnglers)/parGroupSize)*parGroupSize
    l<-foreach(j=1:o, .combine="rbind") %do% {
      
      #don't do expensive buffer and intersection calculations when not necessary
      if(myAnglers$numberInParty[i+j]==1){
        myTmp<-myAnglers[i+j,]  %>%
          mutate(numberInParty=myAnglers$numberInParty[i+j],
                 partyId=myAnglers$partyId[i+j]) %>%
          select(simId, partyId, numberInParty)
      } else {
        myTmp<-st_buffer(myAnglers[i+j,], anglerBoatPartyRadius) %>% 
          st_sample(size=myAnglers[i+j,]$numberInParty) %>%
          data.frame()%>%
          st_as_sf(crs=6343) %>%
          mutate(simId=myAnglers$simId[i+j],
                 numberInParty=myAnglers$numberInParty[i+j],
                 partyId=myAnglers$partyId[i+j])  %>%
          select(simId, partyId, numberInParty)
      }
      
      return(myTmp)
    }
    
    stopCluster(cl)
    
    myAnglers2<-rbind(myAnglers2,l)
    rm(l)  
    
    #sort to ensure order after dopar
    myAnglers2<-myAnglers2 %>% arrange(simId, partyId)
    
    
    myAnglers2<-myAnglers2 %>% 
      group_by(simId, partyId) %>%
      mutate(partyAnglerId=row_number()) %>%
      ungroup() %>%
      mutate(anglerId=row_number())
    
    })


    
# #############################################################
# ### This method is cleaner...but much slower on large datasets
# ################################################################
#     myAnglers_buf<-st_buffer(myAnglers, anglerBoatPartyRadius) %>% 
#       uncount(numberInParty)
#     #these have to be separate as the second one calls the results of the first.
#     #s<-rep(1,nrow(myAnglers_buf))
#     myAnglers2<- myAnglers_buf %>%
#       st_drop_geometry() %>%
#       st_as_sf(geometry=st_sample(myAnglers_buf, size=rep(1,nrow(myAnglers_buf)), type="random", exact=TRUE)) %>%
#       group_by(simId,partyId) %>% 
#       mutate(partyAnglerId=row_number()) %>%
#       ungroup() %>%
#       mutate(anglerId=row_number()) %>%
#       st_as_sf() %>%
#       bind_cols(partyList[rep(1:nrow(partyList), partyList$numberInParty),] %>% 
#         group_by(simId, partyId) %>% 
#         mutate(partyAnglerId=row_number()) %>%
#         ungroup() %>%
#         mutate(anglerId=row_number()))
# ######################################################################
    
  #by resampling partyAngler #1 it "shifts" the whole boat, potentially outside
  #the boundaries of the lake/area, therefore replace the original partyAngler #1 coordinates
  myAnglers2[myAnglers2$partyAnglerId==1,]$geometry<-myAnglers[myAnglers$partyAnglerId==1]$geometry
                 
  # ggplot() +
  #   geom_sf(data=lakeGeom)+
  #   geom_sf(data=st_buffer(myAnglers2 %>% filter(partyAnglerId==1),2.5), fill="red", alpha=.5, color="red") +
  #   geom_sf(data=myAnglers2, size=1)
                
  myAnglers<-myAnglers2
  rm(myAnglers2)
  myAnglers$anglerType="Boat"
  
  }

    return(myAnglers)

}

# anglers_assign_method<-function(tmpAnglers=myBankAnglers,
#                                 percentLure=50,
#                                 mySeed) {
#   
#   #calculate percent bait anglers
#   percentLure<-(percentLure/100)
#   percentBait<-((100-percentLure)/100)
#   
#   #set seed
#   set.seed<-round(mySeed*0.12345/0.2134,0)
#   
#   #assign methods
#   tmpAnglers$anglerMethod<-sample(c("Lure", "Bait"), size=nrow(tmpAnglers), replace=TRUE, prob=c(percentLure, percentBait))
#   
#   #return
#   return(tmpAnglers)
# }

anglers_place<-function(lakeGeom,
                        lakeName,
                        totalAnglers,
                        percentBank,
                        meanPartySizeBank,
                        meanPartySizeBoat,
                        maxPartySizeBoat,
                        maxPartySizeBank,
                        bankAnglers,
                        boatAnglers,
                        anglerBankDistribution="Clustered",
                        anglerBoatDistribution="Clustered",
                        anglerBankPartyRadius,
                        anglerBoatPartyRadius,
                        anglerBankRestrictions=NA,
                        anglerBankProbs=NA,
                        anglerBoatRestrictions=NA,
                        anglerBoatProbs=NA,
                        boatShorelineBuffer,
                        #anglerBankLureProb=100,
                        #anglerBoatLureProb=100,
                        mySeed,
                        numberSims,
                        parGroupSize,
                        parNumberCores){

  #calculate remaining precentages
  percentBoat<-(100-percentBank)

  #create dataset of bank anglers with starting position
  if(percentBank>0){
    myBankAnglers<-anglers_place_bank(lakeGeom=lakeGeom,
                                      lakeName=lakeName,
                                      numberAnglers=bankAnglers,
                                      meanPartySizeBank=meanPartySizeBank,
                                      maxPartySizeBank=maxPartySizeBank,
                                      anglerBankDistribution = anglerBankDistribution,
                                      anglerBankPartyRadius=anglerBankPartyRadius,
                                      anglerBankRestrictions = anglerBankRestrictions,
                                      anglerBankProbs= anglerBankProbs,
                                      mySeed=mySeed,
                                      numberSims=numberSims,
                                      parGroupSize,
                                      parNumberCores)

    # #assign angler method types
    # myBankAnglers<-anglers_assign_method(myBankAnglers, 
    #                                      anglerBankLureProb-50,
    #                                      mySeed)
  } else {
    myBankAnglers<-NA
  }
  
  #create dataset of boat anglers with starting position
  if(percentBoat>0){
    myBoatAnglers<-anglers_place_boat(lakeGeom=lakeGeom,
                                      numberAnglers=boatAnglers,
                                      meanPartySizeBoat=meanPartySizeBoat,
                                      maxPartySizeBoat=maxPartySizeBoat,
                                      anglerBoatDistribution=anglerBoatDistribution,
                                      anglerBoatPartyRadius = anglerBoatPartyRadius,
                                      anglerBoatRestrictions = anglerBoatRestrictions,
                                      anglerBoatProbs=anglerBoatProbs,
                                      boatShorelineBuffer=boatShorelineBuffer,
                                      mySeed=mySeed,
                                      numberSims=numberSims,
                                      parGroupSize,
                                      parNumberCores)
    # #assign angler method types
    # myBoatAnglers<-anglers_assign_method(myBoatAnglers, 
    #                                      anglerBoatLureProb-50, 
    #                                      mySeed)
  } else {
    myBoatAnglers<-NA
  }
  
  #alter starting points for anglers on the move
  
  #combine angler datasets
  if(any(!is.na(myBankAnglers)) & any(!is.na(myBoatAnglers))){
    myAnglers<-rbind(myBankAnglers %>% select(simId, partyId, partyAnglerId, anglerType), myBoatAnglers %>% select(simId, partyId, partyAnglerId, anglerType))
  } else if (any(is.na(myBankAnglers)) & any(!is.na(myBoatAnglers))) {
    myAnglers<-myBoatAnglers %>% select(simId, partyId, partyAnglerId, anglerType)
  } else if (any(!is.na(myBankAnglers)) & any(is.na(myBoatAnglers))) {
    myAnglers<-myBankAnglers %>% select(simId, partyId, partyAnglerId, anglerType)
  }
  
  #add angler Id number
  myAnglers<-myAnglers %>% 
    mutate(anglerId=row_number())
  
  #return
  return(myAnglers)
  
}

