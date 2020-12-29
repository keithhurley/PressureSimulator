plot<-function(dataset) { 
  ggplot() +
    geom_sf(data=myLakeObject$lakeGeom, size=0.75, color="black", fill="steelblue") +
    geom_sf(data=myLakeObject$restrictionsShore, fill="black", color="black", alpha=0.3) +
    geom_sf(data=myLakeObject$probsShore, fill="yellow", color="yellow", alpha=0.3) +
    geom_sf(data=dataset, aes(color=RelativePr), size=1.5)
}


a<-myLakeObject$lakeGeom %>%
  st_cast("LINESTRING", warn=FALSE) %>%
  st_difference(st_union(myLakeObject$restrictionsShore))
  

plot(a)

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

plot(d %>% mutate(RelativePr=as.character(RelativePr)))


ggplot() + 
  geom_sf(data=d, aes(color=as.character(RelativePr)), size=2) +
  scale_color_viridis_d()




createShoreline_restrictions<-funci

a<-myLakeObject$lakeGeom %>%
  st_cast("LINESTRING", warn=FALSE) %>%
  st_difference(st_union(myLakeObject$restrictionsShore))

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






mySegments<-d                   
totalPoints=100
mySeed=1234578

## mySegments     dataframe with segments of shoreline, RelativePr column, and segmentID column
sampleShorelinePoints<-function(mySegments, totalPoints, mySeed){
  
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

myPoints<-sampleShorelinePoints(d, 100, 1223)


d1<- d %>%
  mutate(RelativePr=factor(RelativePr))

ggplot() +
  geom_sf(data=myLakeObject$lakeGeom) +
  geom_sf(data=d1, aes(size=RelativePr, color=RelativePr)) +
  geom_sf(data=myPoints, size=2, color="black") +
  scale_size_manual(values=c(10, 20, 30, 40, 50)) +
  scale_color_viridis_d() 
  


#possiblePointsInEachSegment<-st_sample()

# 
# 
# 
# pts<-st_sample(d, size=1000000, by_polygon=false, exact=TRUE)
# 
# ggplot() + 
#   geom_sf(data=d, aes(color=as.character(RelativePr)), size=2) +
#   geom_sf(data=pts, size=2, color="red") +
#   scale_color_viridis_d()
# 
# pts %>%
#   tally(RelativePr)
