require(sf)

fish_place_random<-function(lakeGeom, numberFish, fishDistribution="random", fishShorelineBuffer, lakeRestrictions=NA, lakeFishProbs=NA, mySeed){

  #set seed
  set.seed(round(mySeed*0.9135/0.134,0))

  if (fishDistribution=="random") {
    d<-st_sample(st_buffer(lakeGeom, (-1 * fishShorelineBuffer)), size=numberFish) %>%
      as.data.frame() %>%
      st_as_sf(crs = 6343) %>% 
      mutate(fishId=row_number()) 
  }
  
  return(d)
}
