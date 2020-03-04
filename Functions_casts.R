
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

casts_create_random_cast_params_direction<-function(){
  myDirection<-floor(runif(1, min=0, max=360))
  return(myDirection)
}

casts_create_random_cast_params_distance<-function(){
  myDistance<-rnorm(1, mean=10, sd=1) + 0.5
  return(myDistance)
}




cast_create_splashdown_coords<-function(myCasts_coords){

  #set seed
  set.seed(round(myRandomSeed*0.2175/0.57734,0))

  myCasts_coords$dir<-apply(myCasts_coords,1,function(x) casts_create_random_cast_params_direction())
  myCasts_coords$dist<-apply(myCasts_coords,1,function(x) casts_create_random_cast_params_distance())
  
  myCasts_coords<-myCasts_coords %>%
     mutate(splash_X= (dist * cos(dir))+cast_X,
            splash_Y= (dist * sin(dir))+cast_Y)
  
  #test for validity...i.e. cast splashdown is in lake
  myCasts_temp<-myCasts_coords %>%
    st_as_sf(coords = c("splash_X", "splash_Y"), 
             crs = 5514) 
  myCasts_coords$valid<-as.vector(as.matrix(st_intersects(myCasts_temp, lakes_round_base)))

  while (any(myCasts_coords$valid==FALSE)) {
    
    print("Running Another Round Of Casts...")
    
    myCasts_coords$dir[myCasts_coords$valid==FALSE]<-apply(myCasts_coords[myCasts_coords$valid==FALSE,],1,function(x) casts_create_random_cast_params_direction())
    myCasts_coords$dist[myCasts_coords$valid==FALSE]<-apply(myCasts_coords[myCasts_coords$valid==FALSE,],1,function(x) casts_create_random_cast_params_distance())

    myCasts_coords<-myCasts_coords %>%
      mutate(splash_X= (dist * cos(dir))+cast_X,
             splash_Y= (dist * sin(dir))+cast_Y)
    
    #test for validity...i.e. cast splashdown is in lake
    myCasts_temp<-myCasts_coords %>%
      st_as_sf(coords = c("splash_X", "splash_Y"), 
               crs = 5514) 
    myCasts_coords$valid<-as.vector(as.matrix(st_intersects(myCasts_temp, lakes_round_base)))
    print(sum(myCasts_coords$valid==FALSE))
    }

  return(myCasts_coords)

}


create_casts_poly<-function(myCasts_coords){

  tmp<-myCasts_coords %>%
    select(-valid) %>%
    unite(start, cast_X, cast_Y) %>%
    unite(end, splash_X, splash_Y) %>%
    gather(start_end, coords, start ,end) %>%
    separate(coords, c("LONG", "LAT"),sep="_") %>%
    mutate_at(vars(LONG, LAT), as.numeric) %>%
    st_as_sf(coords=c("LONG", "LAT"))  %>%
      group_by(anglerId, castId, anglerType, anglerMethod) %>%
      summarise() %>%
      st_cast("LINESTRING") %>%
      st_set_crs(5514)
  
    #myCasts_coords[111,]
    #ggplot()+geom_sf(data=tmp[1:50,])

  return(tmp)
  
}


#st_intersects(myCasts_lines, lakes_round_base)






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
  # 
  # 
  # bind_cols(myAnglers) %>%
  #   mutate(x1=st_coordinates(geometry)[,1],
  #          y1=st_coordinates(geometry)[,2],
  #          x2=st_coordinates(geometry1)[,1],
  #          y2=st_coordinates(geometry1)[,2]) 
  # 
  # lapply(myCasts,c(myCasts$x1, myCasts$y1), c(myCasts$x2, myCasts$y2))
  # 
  # s1 <- rbind(c(0,3),c(0,4),c(1,5),c(2,5))
  # (ls <- st_linestring(s1))
  # 
  # st_geometry(myCasts)<-NULL
  # myCasts<-myCasts %>% select(-geometry1)
  # myCasts<-st_linestring(myCasts, "LINESTRING")
  # 
  # st_cast(c(st_coordinates(.$geometry), st_coordinates(.$geometry1)), "LINESTRING")
  # 
  # myAnglers2<-myAnglers %>%
  #   st_union(myCasts, by_feature = FALSE)
  # 
  # 
  # 
  # 
