sims_runSimulations<-function(myLakeObject,
                         myParamsObject,
                         mySimsObject){
  
  # cl<<-makeCluster(mySimsObject$parNumberCores, type="PSOCK", outfile="")
  # registerDoParallel(cl)
  
  tic("Simulation Time =")
  
  myResults<-list()

  myResults$myFish<-fish_place_random(lakeGeom=myLakeObject$lakeGeom,
                            numberFish=myParamsObject$numberFish,
                            fishShorelineBuffer = myParamsObject$fishShorelineBuffer,
                            mySeed=mySimsObject$seed)

  myResults$myAnglers<-anglers_place(myLakeObject=myLakeObject,
                           anglerBoatDistribution = myParamsObject$anglerBoatDistribution,
                           anglerBankDistribution = myParamsObject$anglerBankDistribution,
                           anglerBoatPartyRadius = myParamsObject$anglerBoatPartyRadius,
                           anglerBankPartyRadius = myParamsObject$anglerBankPartyRadius,
                           totalAnglers = myParamsObject$totalAnglers,
                           bankAnglers=myParamsObject$bankAnglers,
                           boatAnglers=myParamsObject$boatAnglers,
                           meanPartySizeBoat=myParamsObject$meanPartySizeBoat,
                           maxPartySizeBoat=myParamsObject$maxPartySizeBoat,
                           meanPartySizeBank=myParamsObject$meanPartySizeBank,
                           maxPartySizeBank=myParamsObject$maxPartySizeBank,
                           boatShorelineBuffer= myParamsObject$boatShorelineBuffer,
                           percentBank = myParamsObject$percentBank,
                           anglerBankRestrictions=myLakeObject$restrictionsShore,
                           anglerBoatRestrictions = myLakeObject$restrictionsBoat,
                           anglerBankProbs=myLakeObject$probsShore,
                           anglerBoatProbs=myLakeObject$probsBoat,
                           mySeed=mySimsObject$seed,
                           numberSims=mySimsObject$numberSimulations,
                           parGroupSize=mySimsObject$parGroupSize,
                           parNumberCores=mySimsObject$parNumberCores)

  myResults$myCasts<-casts_place(lakeGeom=myLakeObject$lakeGeom,
                       myAnglers=myResults$myAnglers, 
                       castDistanceMean=myParamsObject$castDistanceMean,
                       castDistanceSd=myParamsObject$castDistanceSd,
                       castsPerHourMean=myParamsObject$castsPerHourMean,
                       castsPerHourSd=myParamsObject$castsPerHourSd,
                       mySeed=mySimsObject$seed)
  
  #process spatial data for interactions
  tmpFish<-st_intersects(st_buffer(myResults$myFish, 1), myResults$myCasts)
  tmpFish %>% lengths
  
  #create dataframe of all interactions
  myResults$myInteractions<-myResults$myFish[rep(seq_len(dim(myResults$myFish)[1]), tmpFish %>% lengths), 2] %>% as.data.frame() %>% select(-geometry)
  myResults$myInteractions$anglerId<-myResults$myCasts$anglerId[unlist(tmpFish[tmpFish %>% lengths>0])]
  myResults$myInteractions$castId<-myResults$myCasts$castId[unlist(tmpFish[tmpFish %>% lengths>0])]
  myResults$myInteractions<-myResults$myInteractions %>%
    left_join(myResults$myAnglers %>% as.data.frame() %>% select(anglerId, partyId, simId), by=c("anglerId"))
  myResults$myInteractions<-myResults$myInteractions %>% mutate(interactionId=row_number())

  
  #add table of interaction counts
  myResults$interactionCounts<-myResults$myInteractions %>%
    as.data.frame %>%
    group_by(simId, fishId) %>%
    summarise(numInteractions=n()) %>%
    right_join(expand.grid(fishId=seq(1, myParamsObject$numberFish,1), simId=seq(1,mySimsObject$numberSimulations,1)), by=c("simId", "fishId")) %>%
    mutate(numInteractions=ifelse(is.na(numInteractions), 0, numInteractions)) 
  
  #add interaction count to myFish
    #myResults$interactionFrequences<-table(tmpFish %>% lengths) %>% data.frame() %>% rename("NumberInteractions"="Var1")
    #yResults$myFish$numberInteractions<-tmpFish %>% lengths
  
    #must add in 0's for fish and simulations with no interactions
    myResults$myFish<-myResults$myInteractions %>%
      as.data.frame %>%
      group_by(simId, fishId) %>%
      summarise(numInteractions=n()) %>%
      right_join(expand.grid(fishId=seq(1, myParamsObject$numberFish,1), simId=seq(1,mySimsObject$numberSimulations,1)), by=c("simId", "fishId")) %>%
      mutate(numInteractions=ifelse(is.na(numInteractions), 0, numInteractions)) %>%
      group_by(fishId) %>% 
      arrange(simId, fishId) %>%
      summarise(meanInteractions=mean(numInteractions, na.rm=TRUE),
                sdInteractions=sd(numInteractions, na.rm=TRUE),
                numInteractions=n()) %>%
      mutate(CI=1.96*(sdInteractions/sqrt(numInteractions)),
            upperCI=meanInteractions+CI,
            lowerCI=meanInteractions-CI) %>%
      select(fishId, 
             InteractionsMean=meanInteractions,
            InteractionsSd=sdInteractions,
            InteractionsN=numInteractions,
            InteractionsCi=CI) %>%
      right_join(myResults$myFish, by=c("fishId"))
    
  #add elapsed time to results
  timing<-toc()
  tic.clear()
  myResults$timings$ElapsedTime=round((timing$toc-timing$tic)/60,1)
  
  # stopCluster(cl)
  
  return(myResults)
  
}








 # myResults$pltInteractionTable<-ggplot(data=myResults$tblInteractions) +
  #   geom_bar(aes(x=NumberInteractions, y=Freq), stat="identity", size=2)
  # 
  
  #table(myFish$castInteractions)
  
  # myResults$meanInteractions<-myInteractions %>%
  #     group_by(fishId) %>%
  #     summarize(numInteractions=n()) %>%
  #     ungroup() %>%
  #     right_join(myFish[,c("fishId")], by=c("fishId")) %>%
  #     mutate(numInteractions=ifelse(is.na(numInteractions),0, numInteractions)) %>%
  #     summarize(meanInteractions=mean(numInteractions, na.rm=TRUE))
  #     summarize(meanInteractions=mean())
  #
  