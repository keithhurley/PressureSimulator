
#creates base dataframe to hold all casts
casts_create_df<-function(myAnglers, numberCastsPerAngler=100){
  myAnglers <- myAnglers %>%
    left_join(expand.grid(anglerId=myAnglers$anglerId, castId=1:numberCastsPerAngler))
  return(myAnglers)
}

cast_create_cast_coords<-function(myCasts){
  myAnglers_coords <- myCasts %>%
    st_coordinates() %>%
    data.frame() %>%
    bind_cols(myCasts) %>% 
    select(anglerId, castId, anglerType, anglerMethod, cast_X=X, cast_Y=Y)
  return(myAnglers_coords)
}



cast_create_splashdown_coords<-function(myCasts){
  myCasts<-myCasts %>%
    mutate(splash_X=10*cos(90)+cast_X,
           splash_Y=10*sin(90)+cast_Y)
  return(myCasts)
}


create_casts_poly<-function(myCasts_coords){
  tmpMatrix<-st_linestring(as.matrix(myCasts_coords[,c(5:8)]))
  ggplot()+geom_sf(data=tmpMatrix)
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
 
} 
  
  
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
  
  
  
  
