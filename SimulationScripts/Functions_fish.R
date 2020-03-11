require(sf)

fish_place_random<-function(lakeGeom, numberFish, fishDistribution="random", lakeRestrictions=NA, lakeFishProbs=NA, mySeed){

  #set seed
  set.seed(round(mySeed*0.9135/0.134,0))
  
    if (fishDistribution=="random") {
    d<-st_sample(st_buffer(lakeGeom, -0.5), size=numberFish) %>%
      as.data.frame() %>%
      st_as_sf(crs = 5514) %>% 
      mutate(fishId=row_number()) 
  }
  
  return(d)
}
