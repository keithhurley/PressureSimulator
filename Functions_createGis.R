library(sf)
library(tidyverse)
source("Functions_fish.R")
source("Functions_angler.R")

#myPoint<-c(st_point(c(41.239611, -96.649833)))
#st_as_sf(myPoint)

mapsToMake=data.frame(acres=c(1,5,10,50,100,1000), 
             radius=c(35.8908838844,80.254456138,113.4969403113, 
                      253.7868737742,358.9088388438,1134.9890349188))

d<- st_sfc(st_geometrycollection(list(st_point(c(41.239611, -96.649833)))))
st_set_crs(d, 5514)
# d1<-st_buffer(d, 1134.9890349188) #1000 acres
# d1<-st_buffer(d, 358.9088388438) #100 acres
# d1<-st_buffer(d, 253.7868737742) #50 acres
# d1<-st_buffer(d, 113.4969403113) #10 acres
# d1<-st_buffer(d, 80.254456138) #5 acres
# d1<-st_buffer(d, 35.8908838844) #1 acres

library(foreach)
foreach(i=1:nrow(mapsToMake)) %do% {
  lake<-st_as_sf(data.frame(st_buffer(d, mapsToMake$radius[i])))
  lake<-st_set_crs(lake,5514)
  lake$name<-paste("round_", mapsToMake$acres[i])
  save(lake, file=paste("./data/lakes/round_",mapsToMake$acres[i], ".rData",sep=""))
}

# d1<-st_set_crs(d1,5514)
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
                                      levels=c(10,20,30,40,50,60,70,80,90)){
  foreach(l=1:length(myLakes)) %do% {
  load(file=paste("./data/lakes/", myLakes[l], "/lake.rData", sep=""))
  myShoreline<-st_cast(lake, "LINESTRING")
  myBreakPoints<-st_line_sample(st_cast(lake, "LINESTRING"), n=10, type="regular")
  buf <- st_buffer(myBreakPoints,0.00000001)
  parts = st_collection_extract(lwgeom::st_split(myShoreline, buf),"LINESTRING")
  myParts=c(1,2,20,21)
  myLakeName<-myLakes[l]
  lake_restrictions_shore<-st_combine(parts[myParts,])
  save(lake_restrictions_shore, file=paste("./data/lakes/", myLakeName,"/restrictions/shore/", "10", "percent.rData", sep=""))
  myLevel=0
  foreach(i=seq(3,19,2)) %do% {
    myParts=c(myParts, i, i+1)
    lake_restrictions_shore<-st_combine(parts[myParts,])
    myLevel=myLevel+1
    save(lake_restrictions_shore, file=paste("./data/lakes/", myLakeName, "/restrictions/shore/", levels[myLevel], "percent.rData", sep=""))
    }
  }
}

createShorelineRestrictions()

ggplot() + geom_sf(data=lake) + geom_sf(data=st_sample(st_combine(parts[2:3,]), 3), size=3, color="red") + geom_sf(data=st_combine(parts[2:3,]), color="yellow")

