require(sf)

fish_place_random<-function(lakeGeom, numberFish=100, fishDistribution="random", lakeRestrictions=NA, lakeFishProbs=NA){
  if (fishDistribution=="random") {
    d<-st_sample(st_buffer(lakeGeom, -100), size=100)
  }
  
  return(d)
}
