library(sf)
library(tidyverse)
source("Functions_fish.R")
source("Functions_angler.R")

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
                         totalAnglers = 100,
                         percentBank = 50)
ggplot() +
  geom_sf(data=lakes_round_base, fill="lightskyblue") +
  geom_sf(data=myAnglers, color="red", size=2) +
  #geom_sf(data=myFish, color="black", size=1.5) +
  labs(title="Anglers") 

myCasts<-casts_create_df(myAnglers, numberCastsPerAngler = 10)

myCasts<-casts_create_angler_movement()

myCasts_coords<-cast_create_cast_coords(myCasts)

myCasts_coords<-cast_create_splashdown_coords(myCasts_coords)

myCasts<-create_casts_poly()





theme_set(old)
