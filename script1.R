library(sf)
library(tidyverse)
source("Functions_fish.R")
source("Functions_angler.R")
source("Functions_casts.R")

myRandomSeed=12345

myTheme<-theme_minimal() +
  theme(axis.text=element_blank(),
        panel.grid=element_line(color="transparent"),
        plot.title = element_text(size=48, hjust=0.5))

old<-theme_set(myTheme)

load(file="./data/lakes/round")

myFish<-fish_place_random(lakeGeom=lakes_round_base)

ggplot() +
  geom_sf(data=lakes_round_base, fill="lightskyblue") +
  geom_sf(data=myFish) +
  labs(title="Fish") 

myAnglers<-anglers_place(lakeGeom=lakes_round_base,
                         totalAnglers = 500,
                         percentBank = 50)
ggplot() +
  geom_sf(data=lakes_round_base, fill="lightskyblue") +
  geom_sf(data=myAnglers, color="red", size=2) +
  #geom_sf(data=myFish, color="black", size=1.5) +
  labs(title="Anglers") 

myCasts<-casts_create_df(myAnglers, numberCastsPerAngler = 60)

#myCasts<-casts_create_angler_movement()

myCasts_coords<-cast_create_cast_coords(myCasts)

myCasts_coords<-cast_create_splashdown_coords(myCasts_coords)

myCasts_lines<-create_casts_poly(myCasts_coords)

ggplot() +
  geom_sf(data=myCasts_lines[1:60,]) #+
  #geom_sf(data=myFish, color="black", size=1.5) 

tmpFish<-st_intersects(st_buffer(myFish, 1), myCasts_lines)
myFish$castInteractions<-tmpFish %>% lengths

tmpCasts<-st_intersects(myCasts_lines, st_buffer(myFish, 1))
myCasts_lines$fishInteractions<-tmpCasts %>% lengths

unlist(tmpFish[tmpFish %>% lengths>0])


tmp %>% lengths
tmp[tmp %>% lengths>0]

myCasts_lines[3195,]

ggplot() +
  #geom_sf(data=lakes_round_base, fill="lightskyblue") +
  geom_sf(data=myCasts_lines %>% filter(anglerId == myCasts_lines[3195,]$anglerId)) +
  geom_sf(data=st_buffer(myFish[tmp %>% lengths>0,],1), fill="red", color="transparent", alpha=0.4) +
  geom_sf(data=myFish[tmp %>% lengths>0,], size=1, color="black") 



theme_set(old)
