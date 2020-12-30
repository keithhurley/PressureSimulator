##  this function creates a linestring for the lake, removes any restricted area, 
##  and creates Relative Probabilities for each polygon
##  arguements:
##      myLakeObject  the lake object used for the current simulation

geo_createLakeSegments<-function(myLakeObject){
  
  a<-myLakeObject$lakeGeom %>%
    st_cast("POLYGON", warn=FALSE) 
  
  if(any(!is.na(myLakeObject$restrictionsBoat))){
    
    a <- a %>%  
      st_difference(st_union(myLakeObject$restrictionsBoat)) %>%
      st_cast("MULTIPOLYGON", warn=FALSE) %>%
      st_cast("POLYGON", warn=FALSE) %>%
      mutate(segmentID=row_number())
  }
  
  if(any(!is.na(myLakeObject$probsBoat))){  
    
    b <- a %>%
      st_intersection(myLakeObject$probsBoat) %>%
      st_cast("MULTIPOLYGON", warn=FALSE) %>%
      st_cast("POLYGON", warn=FALSE) %>%
      select(RelativePr)
    
    c<-a %>%
      st_difference(st_union(b)) %>%
      mutate(RelativePr=1) %>%
      st_cast("MULTIPOLYGON", warn=FALSE) %>%
      st_cast("POLYGON", warn=FALSE) %>%
      select(RelativePr)
    
    d<-b %>%
      bind_rows(c) %>%
      mutate(segmentID=row_number())
  } 
  
  return(d)
  
}



##  this function takes in a list of segments with probabilities and samples points on them
##  segments are polygons
##  arguements:
##      mySegments     dataframe with segments of lake, RelativePr column, and segmentID column
##      totalPoints    number of points to sample across segments
##      mySeed         seed value to ensure repeatibility

geo_sampleLakePoints<-function(mySegments, totalPoints, mySeed){
  
  ## set seed
  set.seed(mySeed*123/321+5)
  
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
      mutate(segmentLength=st_area(geometry)) %>%
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






##  this function creates a linestring for the shoreline, removes any restricted area, 
##  and creates Relative Probabilities for each segment
##  arguements:
##      myLakeObject  the lake object used for the current simulation

geo_createShorelineSegments<-function(myLakeObject){
  
  a<-myLakeObject$lakeGeom %>%
    st_cast("MULTILINESTRING", warn=FALSE) %>%
    st_cast("LINESTRING", warn=FALSE) 
  
  if(any(!is.na(myLakeObject$restrictionsShore))){
    
    a <- a %>%  
      st_difference(st_union(myLakeObject$restrictionsShore)) %>%
      st_cast("LINESTRING", warn=FALSE) %>%
      mutate(segmentID=row_number())
  }

  if(any(!is.na(myLakeObject$probsShore))){  
    
    b <- a %>%
      st_intersection(myLakeObject$probsShore) %>%
      select(RelativePr) %>%
      st_cast("LINESTRING", warn=FALSE)
    
    c<-a %>%
      st_difference(st_union(b)) %>%
      mutate(RelativePr=1) %>%
      st_cast("MULTILINESTRING", warn=FALSE) %>%
      st_cast("LINESTRING", warn=FALSE) %>%
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
   geom_sf(data=myLakeObject$lakeGeom, fill="lightblue") +
   geom_sf(data=myLakeObject$restrictionsShore, fill="white",color="gray70", alpha=.6) +
   geom_sf(data=myLakeObject$probsShore %>% mutate(RelativePr=factor(RelativePr)), aes(fill=RelativePr), color=NA, alpha=0.3) +
   geom_sf(data=pts, size=2, color="black") +
   scale_size_manual(values=c(10, 20, 30, 40, 50)) +
   scale_color_viridis_d() +
   labs(fill="Relative \nProbability", title="Shore Angler Placement", subtitle="White areas were considered restricted.") +
   theme_void() +
   theme(plot.title=element_text(hjust=0.2),
         plot.subtitle=element_text(hjust=0.3))
 


 probs<-geo_createLakeSegments(myLakeObject)
 pts<-geo_sampleLakePoints(probs, 100, 1234)
 
 ggplot() +
   geom_sf(data=myLakeObject$lakeGeom, fill="lightblue") +
   geom_sf(data=myLakeObject$restrictionsBoat, fill="white",color="gray70", alpha=.6) +
   geom_sf(data=myLakeObject$probsBoat %>% mutate(RelativePr=factor(RelativePr)), aes(fill=RelativePr), color=NA, alpha=0.8) +
   geom_sf(data=pts, size=2, color="black") +
   scale_size_manual(values=c(10, 20, 30, 40, 50)) +
   scale_color_viridis_d() +
   labs(fill="Relative \nProbability", title="Boat Angler Placement", subtitle="White areas were considered restricted.") +
   theme_void() +
   theme(plot.title=element_text(hjust=0.2),
         plot.subtitle=element_text(hjust=0.3))

         