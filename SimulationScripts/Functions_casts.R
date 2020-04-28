#creates base dataframe to hold all casts
casts_create_df<-function(myAnglers,castsPerHourMean, castsPerHourSd=1){
  tmp_myAnglers<-data.frame(anglerId=myAnglers$anglerId)
  #absolute function is used to correct for any negative number of casts
  #+1 is used for 0 cast corrections
  tmp_myAnglers$numberOfCastsPerHour<-abs(as.numeric(lapply(myAnglers$anglerId, function(x) floor(rnorm(1,mean=castsPerHourMean, sd=castsPerHourSd)))))+1
  tmp_myAnglers <- uncount(tmp_myAnglers,weights=numberOfCastsPerHour) %>%
    group_by(anglerId) %>%
    mutate(castId=row_number()) %>%
    ungroup()
    
  
  myAnglers <- myAnglers %>%
    right_join(tmp_myAnglers, by="anglerId")
  
  return(myAnglers)
}

casts_create_cast_coords<-function(myCasts){
  myAnglers_coords <- myCasts %>%
    st_coordinates() %>%
    data.frame() %>%
    bind_cols(myCasts) %>% 
    select(anglerId, castId, anglerType, cast_X=X, cast_Y=Y)
  return(myAnglers_coords)
}

casts_create_random_cast_params_direction<-function(){
  myDirection<-floor(runif(1, min=0, max=360))
  return(myDirection)
}

casts_create_random_cast_params_distance<-function(castDistanceMean, castDistanceSd){
  myDistance<-rnorm(1, mean=castDistanceMean, sd=castDistanceSd) 
  return(myDistance)
}

casts_create_splashdown_coords<-function(lakeGeom, myCasts_coords, castDistanceMean,
                                        castDistanceSd, mySeed){
  #set seed
  set.seed(round(mySeed*0.2175/0.57734,0))

  myCasts_coords$dir<-apply(myCasts_coords,1,function(x) casts_create_random_cast_params_direction())
  myCasts_coords$dist<-apply(myCasts_coords,1,function(x) casts_create_random_cast_params_distance(castDistanceMean=castDistanceMean,
                                                                                                   castDistanceSd=castDistanceSd))
  
  myCasts_coords<-myCasts_coords %>%
     mutate(splash_X= (dist * cos(dir))+cast_X,
            splash_Y= (dist * sin(dir))+cast_Y)
  
  #test for validity...i.e. cast splashdown is in lake
  myCasts_temp<-myCasts_coords %>%
    st_as_sf(coords = c("splash_X", "splash_Y"), 
             crs = 6343) 
  myCasts_coords$valid<-as.vector(as.matrix(st_intersects(myCasts_temp, lakeGeom)))

  
  
  while (any(myCasts_coords$valid==FALSE)) {
    
    print("Running Another Round Of Casts...")
    
    myCasts_coords$dir[myCasts_coords$valid==FALSE]<-apply(myCasts_coords[myCasts_coords$valid==FALSE,],1,function(x) casts_create_random_cast_params_direction())
    myCasts_coords$dist[myCasts_coords$valid==FALSE]<-apply(myCasts_coords[myCasts_coords$valid==FALSE,],1,function(x) casts_create_random_cast_params_distance(castDistanceMean=castDistanceMean,
                                                                                                                                                                castDistanceSd=castDistanceSd))

    myCasts_coords[myCasts_coords$valid==FALSE,]<-myCasts_coords[myCasts_coords$valid==FALSE,] %>%
      mutate(splash_X= (dist * cos(dir))+cast_X,
             splash_Y= (dist * sin(dir))+cast_Y)
    
    #test for validity...i.e. cast splashdown is in lake
    myCasts_temp<-myCasts_coords[myCasts_coords$valid==FALSE,] %>%
      st_as_sf(coords = c("splash_X", "splash_Y"), 
               crs = 6343) 
    myCasts_coords$valid[myCasts_coords$valid==FALSE]<-as.vector(as.matrix(st_intersects(myCasts_temp, lakeGeom)))
    print(sum(myCasts_coords$valid==FALSE))
    }

  return(myCasts_coords)

}


casts_create_casts_poly<-function(myCasts_coords){
  t<-myCasts_coords %>%
    mutate(tmp=purrr::pmap(list(cast_X, cast_Y, splash_X, splash_Y), 
                           function(.cx, .cy, .sx, .sy) 
                             rbind(c(.cx, .cy), c(.sx, .sy)))) %>%
    pull(tmp) %>%
    lapply(t, FUN=st_linestring) 
  myCasts_coords<-cbind(myCasts_coords,st_as_sfc(t))
  myCasts_coords<-st_as_sf(myCasts_coords, 
                           crs = 6343) 
  return(myCasts_coords)
  
}

casts_place<-function(lakeGeom,
                      myAnglers, 
                      castDistanceMean,
                      castDistanceSd,
                      castsPerHourMean,
                      castsPerHourSd,
                      mySeed){


  myCasts<-casts_create_df(myAnglers=myAnglers, 
                           castsPerHourMean=castsPerHourMean,
                           castsPerHourSd=castsPerHourSd)

  #myCasts<-casts_create_angler_movement()

  myCasts_coords<-casts_create_cast_coords(myCasts)

  myCasts_coords<-casts_create_splashdown_coords(lakeGeom,
                                                myCasts_coords,
                                                castDistanceMean=castDistanceMean,
                                                castDistanceSd=castDistanceSd,
                                                mySeed)

  myCasts_lines<-casts_create_casts_poly(myCasts_coords)

  return(myCasts_lines)
}


