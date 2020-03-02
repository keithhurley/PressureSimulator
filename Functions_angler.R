require(sf)
require(tidyverse)


anglers_place_random<-function(lakeGeom, numberAnglers=100, anglerDistribution="random", lakeRestrictions=NA, lakeAnglerProbs=NA){
  if (anglerDistribution=="random") {
    myAnglers<-lakes_round_base %>% st_cast("LINESTRING") %>% st_line_sample(n=100, type="random")
    myAnglers<-myAnglers %>% st_cast("POINT")
    myAnglers<-st_set_crs(myAnglers, 5514)
    myAnglers<-st_cast(myAnglers) %>%
      as.data.frame() %>%
      st_as_sf(crs = 5514) 
  }
  
  return(myAnglers)
}


casts_create<-function(myAnglers){
  require(geosphere)
  myCasts<-myAnglers %>% 
    st_transform(4326) %>% 
    st_coordinates() %>% 
    as.data.frame() %>% 
    select(X,Y) %>% 
    destPoint(b=90, d=7) %>%
    as.data.frame() %>%
    st_as_sf(coords = c("lon", "lat"), 
             crs = 4326) %>%
    st_transform(crs=5514) %>%
    rownames_to_column("castID")
    
  myAnglers %>%
    rownames_to_column("castID") %>%
    rbind(myCasts) -> myCasts
  
  myCasts2<-myCasts %>%
    group_by(castID) %>% 
    summarise(m=mean(castID)) %>%
    st_cast("LINESTRING") %>%
    ggplot() +
    geom_sf()
  
  
  
  bind_cols(myAnglers) %>%
    mutate(x1=st_coordinates(geometry)[,1],
           y1=st_coordinates(geometry)[,2],
           x2=st_coordinates(geometry1)[,1],
           y2=st_coordinates(geometry1)[,2]) 
  
  lapply(myCasts,c(myCasts$x1, myCasts$y1), c(myCasts$x2, myCasts$y2))
  
  s1 <- rbind(c(0,3),c(0,4),c(1,5),c(2,5))
  (ls <- st_linestring(s1))
  
  st_geometry(myCasts)<-NULL
  myCasts<-myCasts %>% select(-geometry1)
  myCasts<-st_linestring(myCasts, "LINESTRING")
    
    st_cast(c(st_coordinates(.$geometry), st_coordinates(.$geometry1)), "LINESTRING")
  
  myAnglers2<-myAnglers %>%
    st_union(myCasts, by_feature = FALSE)
  
  
  
  
  
  
  st_cast(c(myAnglers, myCasts[[1]]),"LINESTRING") %>% ggplot() + geom_sf()
  
  % st_coordinates() %>% st_linestring()
  
  
  
  # Number of total linestrings to be created
  n <- length(myAnglers) 
  
  # Build linestrings
  linestrings <- lapply(X = 1:n, FUN = function(x) {
    
    pair <- st_combine(c(myAnglers[[x]], myCasts[[1]][[x]]))
    line <- st_cast(pair, "LINESTRING")
    return(line)
  })
    
  
  #st_linestring(myAnglers %>% st_coordinates(), myCasts %>% st_coordinates())
  return(linestrings[[1]])
  
}
