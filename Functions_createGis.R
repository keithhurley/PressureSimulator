library(sf)
library(tidyverse)
source("Functions_fish.R")
source("Functions_angler.R")

#myPoint<-c(st_point(c(41.239611, -96.649833)))
#st_as_sf(myPoint)

mapsToMake=data.frame(acres=c(1,5,10,50,100,1000), 
             radius=c(35.89088388448,80.254456138,113.4969403113, 
                      253.7868737742,358.9088388438,1134.9890349188))

load(file="./Misc/myPoint")
d<-myPoint_fixed
#d<- st_sfc(st_geometrycollection(list(st_point(c(41.239611, -96.649833)))))
#d<-st_set_crs(d, 3857)#4326)
d<-st_transform(d,6343)
# d1<-st_buffer(d, 1134.9890349188) #1000 acres
# d1<-st_buffer(d, 358.9088388438) #100 acres
# d1<-st_buffer(d, 253.7868737742) #50 acres
# d1<-st_buffer(d, 113.4969403113) #10 acres
# d1<-st_buffer(d, 80.254456138) #5 acres
# d1<-st_buffer(d, 35.8908838844) #1 acres

library(foreach)
foreach(i=1:nrow(mapsToMake)) %do% {
  lake<-st_as_sf(data.frame(st_buffer(d, mapsToMake$radius[i])))
  #lake<-st_set_crs(lake,6343)
  lake$name<-paste("round_", mapsToMake$acres[i], sep="")
  #save(lake, file=paste("./data/lakes/round_",mapsToMake$acres[i], "/lake.rData",sep=""))
}

st_write(lake, dsn = '.', layer = 'round_1', driver = 'ESRI Shapefile')

# d1<-st_set_crs(d1,6343)
# lakes_round_5=d1
# save(lakes_round_5, file="./data/lakes/round_5")
# load(file="./data/lakes/round_5")

myFish<-fish_place_random(lakeGeom=lakes_round_base)

ggplot(lakes_round_base) +
  geom_sf() +
  geom_sf(data=myFish)

load(file="./data/lakes/round_1000")
t1000<-st_as_sf(data.frame(lakes_round_1000))


ggplot() +
  geom_sf(data=lakes_round_1000) +
  geom_sf(data=lakes_round_100) +
  geom_sf(data=lakes_round_50) +
  geom_sf(data=lakes_round_10) +
  geom_sf(data=lakes_round_5) +
  geom_sf(data=lakes_round_1)






myAnglers<-anglers_place_random(lakeGeom=lakes_round_base)
ggplot(lakes_round_base) +
  #geom_sf(data=myAnglers) +
  geom_sf(data=myCasts)

str(linestrings)
data.frame(linestrings)
st_combine(linestrings)
tt<-bind_rows(data.frame(linestrings))
sf::st_as_sf(data.table::rbindlist(linestrings))




# make restriction zones --------------------------------------------------
createShorelineRestrictions<-function(myLakes=c("round_1", "round_5", "round_10", "round_50", "round_100", "round_1000"),
                                      levels=rev(c(10,20,30,40,50,60,70,80,90))){
  #loop through each lake
  foreach(l=1:length(myLakes)) %do% {
    load(file=paste("./data/lakes/", myLakes[l], "/lake.rData", sep=""))
    myShoreline<-st_cast(lake, "LINESTRING")
    myBreakPoints<-st_line_sample(st_cast(lake, "LINESTRING"), n=10, type="regular")
    buf <- st_buffer(myBreakPoints,0.00000001)
    parts = st_collection_extract(lwgeom::st_split(myShoreline, buf),"LINESTRING")
    
    #do first segment...
    myParts=c(3,4)
    myLakeName<-myLakes[l]
    lake_restrictions_shore<-st_combine(parts[myParts,])
    lake_restrictions_shore<-st_cast(st_combine(st_cast(lake_restrictions_shore, "MULTIPOINT")), "LINESTRING")
    lake_restrictions_shore<-st_as_sf(data.frame(lake_restrictions_shore))
    lake_restrictions_shore<-st_set_crs(lake_restrictions_shore,6343)
    ggplot(lake_restrictions_shore) + geom_sf() + geom_sf(data=myBreakPoints)
    save(lake_restrictions_shore, file=paste("./data/lakes/", myLakeName,"/restrictions/shore/", "90", "percent.rData", sep=""))
    
    #do rest of segments
    myLevel=0
    foreach(i=seq(5,20,2)) %do% {
      myParts=c(myParts, i, i+1)
      lake_restrictions_shore<-st_combine(parts[myParts,])
      lake_restrictions_shore<-st_cast(st_combine(st_cast(lake_restrictions_shore, "MULTIPOINT")), "LINESTRING")
      lake_restrictions_shore<-st_as_sf(data.frame(lake_restrictions_shore))
      lake<-st_set_crs(lake,6343)
      ggplot(lake_restrictions_shore) + geom_sf() + geom_sf(data=myBreakPoints)
      myLevel=myLevel+1
      save(lake_restrictions_shore, file=paste("./data/lakes/", myLakeName, "/restrictions/shore/", levels[myLevel+1], "percent.rData", sep=""))
      }
    }
  }

createShorelineRestrictions()


myLakes<-foreach(i=seq(10,90,10), .combine="rbind") %do% {
  load(paste("./data/lakes/round_1/restrictions/shore/", i, "percent.rData", sep=""))
  lake_restrictions_shore$name=i
  data.frame(lake_restrictions_shore)
  return(lake_restrictions_shore)
}

ggplot(myLakes[3,]) +
  geom_sf() +
  geom_sf(data=myBreakPoints)+
  facet_wrap(~name)


createShorelineProbs<-function(myLakes=c("round_1"),#, "round_5", "round_10", "round_50", "round_100", "round_1000"),
                                      levels=rev(c(10,20,30,40,50,60,70,80,90)),
                               probs=c(2,3,4,5)){
  #loop through each lake
  foreach(l=1:length(myLakes)) %do% {
    load(file=paste("./data/lakes/", myLakes[l], "/lake.rData", sep=""))
    myShoreline<-st_cast(lake, "LINESTRING")
    myBreakPoints<-st_line_sample(st_cast(lake, "LINESTRING"), n=10, type="regular")
    buf <- st_buffer(myBreakPoints,0.00000001)
    parts = st_collection_extract(lwgeom::st_split(myShoreline, buf),"LINESTRING")
    
    foreach(p=2:length(probs)) %do%{
    #do first segment...
    myParts=c(3,4)
    myLakeName<-myLakes[l]
    lake_restrictions_shore<-st_combine(parts[myParts,])
    lake_restrictions_shore<-st_cast(st_combine(st_cast(lake_restrictions_shore, "MULTIPOINT")), "LINESTRING")
    lake_restrictions_shore<-st_as_sf(data.frame(lake_restrictions_shore))
    lake_restrictions_shore<-st_set_crs(lake_restrictions_shore,6343)
    lake_restrictions_shore$prob=2
    ggplot(lake_restrictions_shore) + geom_sf() + geom_sf(data=myBreakPoints)
    save(lake_restrictions_shore, file=paste("./data/lakes/", myLakeName,"/restrictions/shore/", "90", "percent.rData", sep=""))
    
    #do rest of segments
    myLevel=0
    foreach(i=seq(5,20,2)) %do% {
      myParts=c(myParts, i, i+1)
      lake_restrictions_shore<-st_combine(parts[myParts,])
      lake_restrictions_shore<-st_cast(st_combine(st_cast(lake_restrictions_shore, "MULTIPOINT")), "LINESTRING")
      lake_restrictions_shore<-st_as_sf(data.frame(lake_restrictions_shore))
      lake<-st_set_crs(lake,6343)
      ggplot(lake_restrictions_shore) + geom_sf() + geom_sf(data=myBreakPoints)
      myLevel=myLevel+1
      save(lake_restrictions_shore, file=paste("./data/lakes/", myLakeName, "/restrictions/shore/", levels[myLevel+1], "percent.rData", sep=""))
    }
    }
  }
}
