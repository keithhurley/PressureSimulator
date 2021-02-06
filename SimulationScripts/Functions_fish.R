#fish distributions:
#  1) random - assumes single angler party
#  2) schooling - clusters by school size


#create function to set fish school radius
fish_getRadius<-function(fishSchoolingDistance, fishSchoolSize) {
  mySchoolRadius<-(fishSchoolSize/2)*fishSchoolingDistance
  return(mySchoolRadius)
}



fish_distributeFishIntoSchools<-function(numberFish=100,
                                               meanSchoolSize=2,
                                               mySeed,
                                               parNumberCores){
  
  #set seed
  set.seed(round(mySeed*55,0))
  
  #create schools of anglers
  numberOfSchools=round(numberFish/meanSchoolSize)
  
  #adjust if number of schools is 0
  if(numberOfSchools==0){
    numberFish=1
    
    numberOfSchools=1
    
    schools<-data.frame(schoolId=c(1))
    
    schools$numberInSchool<-numberFish
  } else { 
    schools<-data.frame(schoolId=sort(rep(1:numberOfSchools)))
      
    #do next step in one call to rnorm or rpois for efficiency...not in mutate statement
    schools$numberInSchool=round(rpois(nrow(schools), meanSchoolSize),0)
  }
  
  #eliminate anything under zero and over maxPartySize
  #convert 0 schools to 1
  #convert over schools to max party size
  schools <- schools %>% 
    mutate(numberInSchool=ifelse(numberInSchool==0, 1, numberInSchool))
  
  #trim total numbers to no more than requested number of fish
  schools<-schools %>% mutate(rowId=row_number())
  
  while (sum(schools$numberInSchool)>numberFish) {
      schools <- schools[!(schools$rowId==max(schools$rowId)),] 
  }

  schools<-schools %>% select(-rowId)
  
  #add schools to get to total number of fish
  
    suppressMessages({

    #sort so they can resort following dopar
    schools<-schools %>% arrange(schoolId)
    
    
      require(dplyr)
      
      tmpschools<-schools
      #calculate how many schools of mean size can be used and add them
      currentSum<-sum(tmpschools$numberInSchool)
      if (numberFish-currentSum>=round(meanSchoolSize,0)) {
        numberNeeded=floor((numberFish-currentSum)/round(meanSchoolSize,0))
        tmpschools<-rbind(tmpschools,
                          data.frame(schoolId=c(seq(from=max(tmpschools$schoolId)+1,
                                                   to=max(tmpschools$schoolId) + numberNeeded,
                                                   by=1)),
                                     numberInSchool=rep(round(meanSchoolSize,0),numberNeeded)))
      }
      #if short less than one mean party size...add a school with the rest
      if(sum(tmpschools$numberInSchool)<numberFish){
        tmpschools<-rbind(tmpschools,
                          c(schoolId=max(tmpschools$schoolId)+1,
                            numberInSchool=numberFish-sum(tmpschools$numberInSchool)))
      }
      
      schools<- tmpschools
  })
  
  
  return(schools)
}




fish_place<-function(myLakeObject, 
                     numberFish, 
                     fishDistribution="Random",
                     fishShorelineBuffer,
                     fishMeanNumberPerSchool,
                     fishSchoolingDistance,
                     mySeed,
                     parGroupSize,
                     parNumberCores){

  #set seed
  set.seed(round(mySeed*0.9135/0.134,0))
  
  
  #prepare lake segments
  #get lake segments for selections
  #if (is.na(anglerBoatPartyRadius)==FALSE & anglerBoatPartyRadius>boatShorelineBuffer) {boatShorelineBuffer=anglerBoatPartyRadius}
  myLakeSegments<-geo_createFishSegments(myLakeObject, fishShorelineBuffer)
  
  #create a lake polygon that removes shoreline buffer and restricted areas to use in placing fish schools around primary point
  myFishArea<-geo_createFishAreaPolygon(myLakeObject, fishShorelineBuffer)
  
  if (fishDistribution=="Random") {
    
    myFish<-geo_sampleLakePoints(myLakeSegments, totalPoints=data.frame(simId=1,totalPoints=numberFish) %>% as_tibble(), mySeed) %>%
      as.data.frame() %>%
      st_as_sf(crs=6343) %>%
      mutate(fishId=row_number()) %>%
      mutate(schoolId=fishId) %>%
      select(-simId) #use same fish for all simulations, this hack lets us use existing geo_sample functions
    
  } else if (fishDistribution=="Schooling"){
    #create parties of anglers
    schoolList<-fish_distributeFishIntoSchools(numberFish=numberFish,
                                                    meanSchoolSize=fishMeanNumberPerSchool,
                                                    mySeed=mySeed,
                                                    parNumberCores=parNumberCores)
    
    
    #get random points for each boat
    myFish<-geo_sampleLakePoints(myLakeSegments, totalPoints=schoolList %>% mutate(simId=1) %>% group_by(simId) %>% summarise(totalPoints=n()), mySeed) %>%
      arrange(simId) %>%
      st_bind_cols(schoolList %>% arrange(schoolId)) 
    
    #alter row location so subsequent anglers in a party have a different location
    #...within a buffered circle (i.e. a boat)
    suppressMessages({
      print(paste(getDoParWorkers(), " Cores Will Be Used For Placing Fish", sep=""))
      
      cl<-makeCluster(parNumberCores, type="PSOCK")
      registerDoParallel(cl)
      registerDoRNG(round(mySeed*412,0))
      
      #sort prior to dopar to ensure order
      myFish<-myFish %>% arrange(simId, schoolId)
      
      myFish2<-foreach(i=seq(1,floor(nrow(myFish)/parGroupSize)*parGroupSize,by=parGroupSize), .combine="rbind") %dopar% {
        
        require(dplyr)
        require(sf)
        require(foreach)
        
        fish_getRadius<-function(fishSchoolingDistance, fishSchoolSize) {
            mySchoolRadius<-(fishSchoolSize/2)*fishSchoolingDistance
            return(mySchoolRadius)
          }
        
        #do full groups using parallel processing
        k<-foreach(j=0:(parGroupSize-1), .combine="rbind") %do% {
          

          
          #don't do expensive buffer and intersection calculations when not necessary
          if(myFish$numberInSchool[i+j]==1){
            myTmp<-myFish[i+j,] %>%
              mutate(simId=myFish$simId[i+j],
                     numberInSchool=myFish$numberInSchool[i+j],
                     schoolId=myFish$schoolId[i+j])  %>%
              select(simId, schoolId, numberInSchool)
          } else {
            myTmp<-st_buffer(myFish[i+j,], fish_getRadius(fishSchoolingDistance, myFish$numberInSchool[i+j])) %>%
              st_intersection(myFishArea) %>%
              st_sample(size=myFish[i+j,]$numberInSchool) %>%
              data.frame()%>%
              st_as_sf(crs=6343) %>%
              mutate(simId=myFish$simId[i+j],
                     numberInSchool=myFish$numberInSchool[i+j],
                     schoolId=myFish$schoolId[i+j])  %>%
              select(simId, schoolId, numberInSchool)
          }
          return(myTmp)
        }
        
        return(k)
      }
      
      
      #do the last group which may or may not be partial
      #create proper number of for loop to do last items
      #get number of odd items of partial group
      o=nrow(myFish) - (floor(nrow(myFish)/parGroupSize)*parGroupSize)
      i=floor(nrow(myFish)/parGroupSize)*parGroupSize
      l<-foreach(j=1:o, .combine="rbind") %do% {
        
        fish_getRadius<-function(fishSchoolingDistance, fishSchoolSize) {
          mySchoolRadius<-(fishSchoolSize/2)*fishSchoolingDistance
          return(mySchoolRadius)
        }
        
        #don't do expensive buffer and intersection calculations when not necessary
        if(myFish$numberInSchool[i+j]==1){
          myTmp<-myFish[i+j,]  %>%
            mutate(numberInSchool=myFish$numberInSchool[i+j],
                   schoolId=myFish$schoolId[i+j]) %>%
            select(simId, schoolId, numberInSchool)
        } else {
          myTmp<-st_buffer(myFish[i+j,], fish_getRadius(fishSchoolingDistance, myFish$numberInSchool[i+j])) %>% 
            st_intersection(myFishArea) %>%
            st_sample(size=myFish[i+j,]$numberInSchool) %>%
            data.frame()%>%
            st_as_sf(crs=6343) %>%
            mutate(simId=myFish$simId[i+j],
                   numberInSchool=myFish$numberInSchool[i+j],
                   schoolId=myFish$schoolId[i+j])  %>%
            select(simId, schoolId, numberInSchool)
        }
        
        return(myTmp)
      }
      
      stopCluster(cl)
      
      myFish2<-rbind(myFish2,l)
      rm(l)  
      
      #sort to ensure order after dopar
      myFish2<-myFish2 %>% arrange(simId, schoolId)
      
      
      myFish2<-myFish2 %>% 
        group_by(simId, schoolId) %>%
        mutate(schoolFishId=row_number()) %>%
        ungroup() %>%
        mutate(fishId=row_number())
      
    })
    
    myFish<-myFish2 %>% select(simId, fishId)
    rm(myFish2)

  }
  
  return(myFish)
  
}


# ggplot() +
#   geom_sf(data=myLakeObject$lakeGeom, fill="lightblue") +
#   geom_sf(data=myResults$myFish) +
#   facet_wrap(~schoolId)
# 
# myParamsObject$numberFish=1000
# myParamsObject$fishMeanNumberPerSchool=35
