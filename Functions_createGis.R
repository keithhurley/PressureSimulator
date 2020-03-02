library(sf)
library(tidyverse)
source("Functions_fish.R")
source("Functions_angler.R")

#myPoint<-c(st_point(c(41.239611, -96.649833)))
#st_as_sf(myPoint)


#d<- st_sfc(st_geometrycollection(list(st_point(c(41.239611, -96.649833)))))
#st_set_crs(d, 5514)
#d1<-st_buffer(d, 1177.820506909612)

#ggplot(d1) +
#  geom_sf()

#d1<-st_set_crs(d1,5514)
#lakes_round_base=d1
#save(lakes_round_base, file="./data/lakes/round")
load(file="./data/lakes/round")

myFish<-fish_place_random(lakeGeom=lakes_round_base)

ggplot(lakes_round_base) +
  geom_sf() +
  geom_sf(data=myFish)


myAnglers<-anglers_place_random(lakeGeom=lakes_round_base)
ggplot(lakes_round_base) +
  #geom_sf(data=myAnglers) +
  geom_sf(data=myCasts)

str(linestrings)
data.frame(linestrings)
st_combine(linestrings)
tt<-bind_rows(data.frame(linestrings))
sf::st_as_sf(data.table::rbindlist(linestrings))
