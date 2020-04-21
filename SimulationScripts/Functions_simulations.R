sims_runSimulations<-function(myLakeObject,
                         myParamsObject,
                         mySimsObject){
  
  tic("Simulation Time =")
  
  myResults<-list()

  myResults$myFish<-fish_place_random(lakeGeom=myLakeObject$lakeGeom,
                            numberFish=myParamsObject$numberFish,
                            fishShorelineBuffer = myParamsObject$fishShorelineBuffer,
                            mySeed=mySimsObject$seed)

    myResults$myAnglers<-anglers_place(lakeGeom=myLakeObject$lakeGeom,
                           lakeName=myLakeObject$lakeName,
                           anglerBoatDistribution = myParamsObject$anglerBoatDistribution,
                           anglerBankDistribution = myParamsObject$anglerBankDistribution,
                           anglerBoatPartyRadius = myParamsObject$anglerBoatPartyRadius,
                           anglerBankPartyRadius = myParamsObject$anglerBankPartyRadius,
                           totalAnglers = myParamsObject$totalAnglers,
                           meanPartySizeBoat=myParamsObject$meanPartySizeBoat,
                           maxPartySizeBoat=myParamsObject$maxPartySizeBoat,
                           meanPartySizeBank=myParamsObject$meanPartySizeBank,
                           maxPartySizeBank=myParamsObject$maxPartySizeBank,
                           boatShorelineBuffer= myParamsObject$boatShorelineBuffer,
                           percentBank = myParamsObject$percentBank,
                           mySeed=mySimsObject$seed,
                           numberSims=mySimsObject$numberSimulations)

  myResults$myCasts<-casts_place(lakeGeom=myLakeObject$lakeGeom,
                       myAnglers=myResults$myAnglers, 
                       castDistanceMean=myParamsObject$castDistanceMean,
                       castDistanceSd=myParamsObject$castDistanceSd,
                       castsPerHourMean=myParamsObject$castsPerHourMean,
                       castsPerHourSd=myParamsObject$castsPerHourSd,
                       mySeed=mySimsObject$seed)
  
  #process spatial data for interactions
  tmpFish<-st_intersects(st_buffer(myResults$myFish, 1), myResults$myCasts)
  myResults$interactionFrequences<-table(tmpFish %>% lengths) %>% data.frame() %>% rename("NumberInteractions"="Var1")
  #add interaction count to myFish
  myResults$myFish$numberInteractions<-tmpFish %>% lengths
  #create dataframe of all interactions
  myResults$myInteractions<-myResults$myFish[rep(seq_len(dim(myResults$myFish)[1]), myResults$myFish$numberInteractions), 2]
  myResults$myInteractions$anglerId<-myResults$myCasts$castId[unlist(tmpFish[tmpFish %>% lengths>0])]
  myResults$myInteractions$castId<-myResults$myCasts$castId[unlist(tmpFish[tmpFish %>% lengths>0])]
  
  #calculate mean number ints per fish
  myResults$interactionsPerFishMean<-mean(myResults$myFish$numberInteractions, na.rm=TRUE)
  myResults$interactionsPerFishVar<-var(myResults$myFish$numberInteractions, na.rm=TRUE)
  myResults$interactionsPerFishSd<-sd(myResults$myFish$numberInteractions, na.rm=TRUE)
  
  #add elapsed time to results
  timing<-toc()
  tic.clear()
  myResults$timings$ElapsedTime=round((timing$toc-timing$tic)/60,1)
  
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
  