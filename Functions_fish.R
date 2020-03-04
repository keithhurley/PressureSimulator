require(sf)

fish_place_random<-function(lakeGeom, numberFish=100, fishDistribution="random", lakeRestrictions=NA, lakeFishProbs=NA){
  if (fishDistribution=="random") {
    d<-st_sample(st_buffer(lakeGeom, -0.5), size=100) %>%
      as.data.frame() %>%
      st_as_sf(crs = 5514) 
  }
  
  return(d)
}
