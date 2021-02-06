##  this function creates a linestring for the lake, removes any restricted area, 
##  and creates Relative Probabilities for each polygon
##  arguements:
##      myLakeObject  the lake object used for the current simulation

geo_createLakeSegments<-function(myLakeObject, myBoatShorelineBuffer=1){
  
  a<-myLakeObject$lakeGeom %>%
    st_cast("POLYGON", warn=FALSE) %>% 
    st_buffer((-1*myBoatShorelineBuffer))
  
  #this removes warnings about spatial attribute variables assumed constant
  st_agr(a)="constant"
  
  if(any(!is.na(myLakeObject$restrictionsBoat))){
    
    a <- a %>%  
      st_difference(st_union(myLakeObject$restrictionsBoat)) %>%
      st_cast("MULTIPOLYGON", warn=FALSE) %>%
      st_cast("POLYGON", warn=FALSE) %>%
      mutate(segmentID=row_number())
    
    st_agr(a) = "constant"
  }
  
  if(any(!is.na(myLakeObject$probsBoat))){  
    
    st_agr(myLakeObject$probsBoat)="constant"
    
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
    
    a<-b %>%
      bind_rows(c) %>%
      mutate(segmentID=row_number())
  } 
  
  #if base geom was used with no restrictions or priorities...then need to add a RealitivePr column
  if (!"RelativePr" %in% names(a)) {
    a<-a %>%
      mutate(RelativePr=1) %>%
      mutate(segmentID=row_number()) %>%
      select(RelativePr, segmentID) 
  }
  
  return(a)
  
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
  
  ## get number of points to sample in each probability and simId
  ## this is tickets in a lottery sampling....size of area DOES NOT play a role
  numberOfPointsPerProb<-data.frame(RelativePr=sample(uniqueProbs$RelativePr,
                                                      size=sum(totalPoints$totalPoints), 
                                                      replace=TRUE, 
                                                      prob=uniqueProbs$RelativePr %>% 
                                                        unique()
                                                      )
                                    ) %>%
    bind_cols(totalPoints[rep(seq_len(dim(totalPoints)[1]), totalPoints$totalPoints), 1]) %>%
    group_by(RelativePr, simId) %>%
    tally(name="totalNumberOfPoints")
  
  
  ## within each probability, distribute the points among segments
  ##  length of segment DOES matter here....otherwise small areas will be oversampled and etc
  
  ## create a dataframe with the number of points to sample in each segment of this probability level
      mySampleNumbers<-mySegments %>%
        mutate(segmentLength=st_area(geometry)) %>%
        st_drop_geometry() %>%
        left_join(numberOfPointsPerProb, by=c("RelativePr")) %>%
        mutate(actualProb=RelativePr * as.numeric(segmentLength)) %>%
        select(simId, segmentID, totalNumberOfPoints, RelativePr, actualProb) %>%
        #nest the groups of simID and segments and Probs to form selection possibility list columns
        group_by(simId, RelativePr, totalNumberOfPoints) %>%
        nest() %>%
        ungroup() %>%
        mutate(sample=map2(data, totalNumberOfPoints, weight=actualProb, replace=TRUE, sample_n)) %>%
        select(-data) %>%
        unnest(sample) %>%
          group_by( simId, segmentID) %>%
          summarise(numberOfPoints=n())
      

    #create list to sample from
    a1<-mySegments %>% 
      right_join(mySampleNumbers, by=c("segmentID"))
    
    #sample points
    a<-a1 %>% st_sample(size=.$numberOfPoints)
    
    #join back to list to reattach simId
    a<-a %>%
      cbind(a1[rep(seq_len(dim(a1)[1]), a1$numberOfPoints), c(3)])
  
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
  
  st_agr(a)="constant"
  
  if(any(!is.na(myLakeObject$restrictionsShore))){
    
    a <- a %>%  
      st_difference(st_union(myLakeObject$restrictionsShore)) %>%
      st_cast("LINESTRING", warn=FALSE) %>%
      mutate(segmentID=row_number()) 
    
    st_agr(a)="constant"
  }

  if(any(!is.na(myLakeObject$probsShore))){  
  
    st_agr(myLakeObject$probsShore)="constant"
    
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
    
    a<-b %>%
      bind_rows(c) %>%
      mutate(segmentID=row_number()) 
  } 

  #if base geom was used with no restrictions or priorities...then need to add a RelativePr column
  if (!"RelativePr" %in% names(a)) {
    a<-a %>%
      mutate(RelativePr=1) %>%
      mutate(segmentID=row_number()) %>%
      select(RelativePr, segmentID) 
  }
  
  return(a)

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
                                                      size=sum(totalPoints$totalPoints), 
                                                      replace=TRUE, 
                                                      prob=uniqueProbs$RelativePr %>% 
                                                        unique()
                                                      )
                                    ) %>% 
    bind_cols(totalPoints[rep(seq_len(dim(totalPoints)[1]), totalPoints$totalPoints), 1]) %>%
    group_by(RelativePr, simId) %>%
    tally(name="totalNumberOfPoints")
  
  ## within each probability, distribute the points among segments
  ##  length of segment DOES matter here....otherwise small areas will be oversampled and etc
    mySampleNumbers<-mySegments %>%
      mutate(segmentLength=st_length(geometry)) %>%
      st_drop_geometry() %>%
      left_join(numberOfPointsPerProb, by=c("RelativePr")) %>%
      mutate(actualProb=RelativePr * as.numeric(segmentLength)) %>%
      select(simId, segmentID, totalNumberOfPoints, RelativePr, actualProb) %>%
      #nest the groups of simID and segments and Probs to form selection possibility list columns
      group_by(simId, RelativePr, totalNumberOfPoints) %>%
      nest() %>%
      ungroup() %>%
      mutate(sample=map2(data, totalNumberOfPoints, weight=actualProb, replace=TRUE, sample_n)) %>%
      select(-data) %>%
      unnest(sample) %>%
      group_by( simId, segmentID) %>%
      summarise(numberOfPoints=n())
    
    #create list to sample from
    a1<-mySegments %>% 
      right_join(mySampleNumbers, by=c("segmentID")) 
    
        #sample points
    a<-a1 %>% st_sample(size=.$numberOfPoints) %>% st_cast("POINT")
    
    #join back to list to reattach simId
    a<-a %>%
      cbind(a1[rep(seq_len(dim(a1)[1]), a1$numberOfPoints), c(3)] %>%
              st_drop_geometry()) %>%
      st_as_sf(crs=6343)
    
    return(a)
    
}



##  this function creates a linestring for the lake, removes any restricted area, 
##  and creates Relative Probabilities for each polygon
##  arguements:
##      myLakeObject  the lake object used for the current simulation

geo_createFishSegments<-function(myLakeObject, myFishShorelineBuffer=1){
  
  a<-myLakeObject$lakeGeom %>%
    st_cast("POLYGON", warn=FALSE) %>% 
    st_buffer((-1*myFishShorelineBuffer))
  
  #this removes warnings about spatial attribute variables assumed constant
  st_agr(a)="constant"
  
  if(any(!is.na(myLakeObject$restrictionsFish))){
    
    a <- a %>%  
      st_difference(st_union(myLakeObject$restrictionsFish)) %>%
      st_cast("MULTIPOLYGON", warn=FALSE) %>%
      st_cast("POLYGON", warn=FALSE) %>%
      mutate(segmentID=row_number())
    
    st_agr(a) = "constant"
  }
  
  if(any(!is.na(myLakeObject$probsFish))){  
    
    st_agr(myLakeObject$probsFish)="constant"
    
    b <- a %>%
      st_intersection(myLakeObject$probsFish) %>%
      st_cast("MULTIPOLYGON", warn=FALSE) %>%
      st_cast("POLYGON", warn=FALSE) %>%
      select(RelativePr)
    
    c<-a %>%
      st_difference(st_union(b)) %>%
      mutate(RelativePr=1) %>%
      st_cast("MULTIPOLYGON", warn=FALSE) %>%
      st_cast("POLYGON", warn=FALSE) %>%
      select(RelativePr)
    
    a<-b %>%
      bind_rows(c) %>%
      mutate(segmentID=row_number())
  } 
  
  #if base geom was used with no restrictions or priorities...then need to add a RealitivePr column
  if (!"RelativePr" %in% names(a)) {
    a<-a %>%
      mutate(RelativePr=1) %>%
      mutate(segmentID=row_number()) %>%
      select(RelativePr, segmentID) 
  }
  
  return(a)
  
}


#create polygon of lake with fishShorelineBuffer and restricted areas
geo_createFishAreaPolygon<-function(myLakeObject, fishShorelineBuffer) {
  myPoly<-myLakeObject$lakeGeom %>%
    st_cast("POLYGON", warn=FALSE) %>% 
    st_buffer(-1*fishShorelineBuffer)
  
  #this removes warnings about spatial attribute variables assumed constant
  st_agr(a)="constant"
  
  if(any(!is.na(myLakeObject$restrictionsFish))){
    
    a <- a %>%  
      st_difference(st_union(myLakeObject$restrictionsFish)) %>%
      st_cast("MULTIPOLYGON", warn=FALSE) %>%
      st_cast("POLYGON", warn=FALSE) %>%
      mutate(segmentID=row_number())
    
    st_agr(a) = "constant"
  }
  return(myPoly)
}


# 
# 
# probs<-geo_createShorelineSegments(myLakeObject)
#  pts<-geo_sampleShorelinePoints(probs, 100, 1234)
#  
#  ggplot() +
#    geom_sf(data=myLakeObject$lakeGeom, fill="lightblue") +
#    geom_sf(data=myLakeObject$restrictionsShore, fill="white",color="gray70", alpha=.6) +
#    geom_sf(data=myLakeObject$probsShore %>% mutate(RelativePr=factor(RelativePr)), aes(fill=RelativePr), color=NA, alpha=0.3) +
#    geom_sf(data=pts, size=2, color="black") +
#    scale_size_manual(values=c(10, 20, 30, 40, 50)) +
#    scale_color_viridis_d() +
#    labs(fill="Relative \nProbability", title="Shore Angler Placement", subtitle="White areas were considered restricted.") +
#    theme_void() +
#    theme(plot.title=element_text(hjust=0.2),
#          plot.subtitle=element_text(hjust=0.3))
#  
# 
# 
#  probs<-geo_createLakeSegments(myLakeObject)
#  pts<-geo_sampleLakePoints(probs, 100, 1234)
#  
#  ggplot() +
#    geom_sf(data=myLakeObject$lakeGeom, fill="lightblue") +
#    geom_sf(data=myLakeObject$restrictionsBoat, fill="white",color="gray70", alpha=.6) +
#    geom_sf(data=myLakeObject$probsBoat %>% mutate(RelativePr=factor(RelativePr)), aes(fill=RelativePr), color=NA, alpha=0.8) +
#    geom_sf(data=pts, size=2, color="black") +
#    scale_size_manual(values=c(10, 20, 30, 40, 50)) +
#    scale_color_viridis_d() +
#    labs(fill="Relative \nProbability", title="Boat Angler Placement", subtitle="White areas were considered restricted.") +
#    theme_void() +
#    theme(plot.title=element_text(hjust=0.2),
#          plot.subtitle=element_text(hjust=0.3))
# 
#          