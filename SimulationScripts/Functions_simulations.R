runSimulations<-function(myLakeObject,
                         myParamObject,
                         numberSimulations=1){
                         #mySimObject){
  
  tic("Simulation Time =")
  
  myResults<-list()
  myResults$myLakeObject<-myLakeObject
  myResults$myParamObject<-myParamObject
  #myResults$mySimObject<-mySimObject
  
  
  myResults$myFish<-fish_place_random(lakeGeom=lake,
                            numberFish=100,
                            fishShorelineBuffer = 0.5,
                            mySeed=12345)
  
  myResults$myAnglers<-anglers_place(lakeGeom=lake,
                           lakeName=lake$name,
                           anglerBoatDistribution = "Clustered By Party",
                           anglerBankDistribution = "Clustered By Party",
                           anglerBoatPartyRadius = 2.5,
                           anglerBankPartyRadius = 3,
                           totalAnglers = 100,
                           meanPartySizeBoat=1.8,
                           maxPartySizeBoat=5,
                           meanPartySizeBank=2.4,
                           maxPartySizeBank=6,
                           boatShorelineBuffer= 5,
                           percentBank = 50,
                           mySeed=12345)

  myResults$myCasts<-casts_place(lakeGeom=lake,
                       myResults$myAnglers, 
                       numberCastsPerAngler=60,
                       meanCastDistance=10,
                       sdCastDistance=3,
                       meanCastsPerHour=40,
                       sdCastsPerHour=15,
                       mySeed=12345)
  
  
  
  
  #process spatial data for interactions
  tmpFish<-st_intersects(st_buffer(myResults$myFish, 1), myResults$myCasts)
  myResults$tblInteractionTable<-table(tmpFish %>% lengths) %>% data.frame() %>% rename("NumberInteractions"="Var1")
  
  
  # myResults$pltInteractionTable<-ggplot(data=myResults$tblInteractions) +
  #   geom_bar(aes(x=NumberInteractions, y=Freq), stat="identity", size=2)
  # 
  
  #add interaction count to myFish
  myResults$myFish$castInteractions<-tmpFish %>% lengths
  #table(myFish$castInteractions)
  
  #create dataframe of all interactions
  myResults$myInteractions<-myFish[rep(seq_len(dim(myFish)[1]), myFish$castInteractions), 2]
  myResults$myInteractions$anglerId<-myCasts$castId[unlist(tmpFish[tmpFish %>% lengths>0])]
  myResults$myInteractions$castId<-myCasts$castId[unlist(tmpFish[tmpFish %>% lengths>0])]
  
  #calculate mean number ints per fish
  myResults$numMeanInteractionsPerFish<-mean(myFish$castInteractions)
  
  
  
  
  # myResults$meanInteractions<-myInteractions %>%
  #     group_by(fishId) %>%
  #     summarize(numInteractions=n()) %>%
  #     ungroup() %>%
  #     right_join(myFish[,c("fishId")], by=c("fishId")) %>%
  #     mutate(numInteractions=ifelse(is.na(numInteractions),0, numInteractions)) %>%
  #     summarize(meanInteractions=mean(numInteractions, na.rm=TRUE))
  #     summarize(meanInteractions=mean())
  #
  tic("test")
  timing<-toc()
  myResults$ElapsedTime(round((timing$toc-timing$tic)/60,1))
  
  return(myResults)
  
}