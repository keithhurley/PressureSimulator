##  this function creates a linestring for the shoreline, removes any restricted area, 
##  and creates Relative Probabilities for each segment
##  arguements:
##      myLakeObject  the lake object used for the current simulation

geo_createShorelineSegments<-function(myLakeObject){
  
  a<-myLakeObject$lakeGeom %>%
    st_cast("LINESTRING", warn=FALSE) 
  
  if(!is.na(myLakeObject$restrictionsShore)){
    
    d <- a %>%  
      st_difference(st_union(myLakeObject$restrictionsShore)) %>%
      mutate(RelativePr=1) %>%
      mutate(segmentID=row_number())
  }

  if(!is.na(myLakeObject$probsShore)){  
    
    b <- a %>%
      st_intersection(myLakeObject$probsShore) %>%
      select(RelativePr)
  
    c<-a %>%
      st_difference(st_union(b)) %>%
      mutate(RelativePr=1) %>%
      st_cast("LINESTRING") %>%
      select(RelativePr)
    
    d<-b %>%
      bind_rows(c) %>%
      mutate(segmentID=row_number())
  } 

  return(d)

  }



##  this function takes in a list of segments with probabilities and samples points on them
##  segments are linestrings
##  arguements:
##      mySegments     dataframe with segments of shoreline, RelativePr column, and segmentID column
##      totalPoints    number of points to sample across segments
##      mySeed         seed value to ensure repeatibility

geo_sampleShorelinePoints<-function(mySegments, totalPoints, mySeed){
  
  ## set seed
  set.seed(mySeed*123/321+5)
  
  ## collapse and separate overlapping prob areas
  ## use additive (not multiplive) combining of prob values?  or should segment be in twice?
  
  
  ## get different probability levels
  uniqueProbs<-mySegments %>% st_drop_geometry() %>% group_by(RelativePr) %>% summarise(num=n()) %>% arrange(RelativePr)
  
  ## get number of points to sample in each probability
  ## this is tickets in a lottery sampling....size of area DOES NOT play a role
  numberOfPointsPerProb<-data.frame(RelativePr=sample(uniqueProbs$RelativePr, 
                                                      size=totalPoints, 
                                                      replace=TRUE, 
                                                      prob=uniqueProbs$RelativePr %>% unique())) %>% 
    group_by(RelativePr) %>%
    tally(name="totalNumberOfPoints")
  
  ## within each probability, distribute the points among segments
  ##  length of segment DOES matter here....otherwise small areas will be oversampled and etc
  a<-foreach(intProb=numberOfPointsPerProb$RelativePr, .combine="rbind") %do% {
    #wrangle data for segments of each probability level
    op<-mySegments %>%
      filter(RelativePr==intProb) %>%
      mutate(segmentLength=st_length(geometry)) %>%
      st_drop_geometry() %>%
      left_join(numberOfPointsPerProb) %>%
      mutate(actualProb=RelativePr * as.numeric(segmentLength))
    
    if(length(op$segmentID)==1) {
      ## assign total number of points to a single segment
      op$numberOfPoints=op$totalNumberOfPoints 
      op<-op %>%
        select(segmentID, numberOfPoints)
    } else {
      ## create a dataframe with the number of points to sample in each segment of this probability level
      mySampleNumbers<- data.frame(segmentID=sample(op$segmentID, 
                                                    op$totalNumberOfPoints[1], 
                                                    replace=TRUE,
                                                    prob=op$actualProb),
                                   dummy=1) %>%
        group_by(segmentID) %>%
        summarise(numberOfPoints=sum(dummy, na.rm=TRUE)) %>%
        right_join(op %>% select(segmentID)) %>%
        mutate(numberOfPoints=replace_na(numberOfPoints, 0))
      
      ## join back to original data to fill in 0's for any segment that wasn't sampled
      op<-op %>% 
        left_join(mySampleNumbers, by=("segmentID")) %>%
        select(segmentID, numberOfPoints)
    }
    
    return(op)  
  }
  
  
  a<-mySegments %>% 
    left_join(a, by=c("segmentID")) %>%
    st_sample(size=.$numberOfPoints)  %>%
    st_cast("POINT")
  
  return(a)
}

probs<-geo_createShorelineSegments(myLakeObject)
pts<-geo_sampleShorelinePoints(probs, 100, 1234)

ggplot() +
  geom_sf(data=myLakeObject$lakeGeom) +
  geom_sf(data=myLakeObject$probsShore %>% mutate(RelativePr=factor(RelativePr)), aes(fill=RelativePr), color=NA, alpha=0.3) +
  geom_sf(data=pts, size=2, color="black") +
  scale_size_manual(values=c(10, 20, 30, 40, 50)) +
  scale_color_viridis_d() +
  theme_void()
