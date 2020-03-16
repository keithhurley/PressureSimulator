library(sf)
library(tidyverse)
source("./SimulationScripts/Functions_fish.R")
source("./SimulationScripts/Functions_angler.R")
source("./SimulationScripts/Functions_casts.R")
library(profvis)
library(tictoc)


myRandomSeed=12345

myTheme<-theme_minimal() +
  theme(axis.text=element_blank(),
        panel.grid=element_line(color="transparent"),
        plot.title = element_text(size=48, hjust=0.5))

old<-theme_set(myTheme)

load(file="./data/lakes/round")

myFish<-fish_place_random(lakeGeom=lakes_round_base, numberFish=100)

# ggplot() +
#   geom_sf(data=lakes_round_base, fill="lightskyblue") +
#   geom_sf(data=myFish) +
#   labs(title="Fish") 

myAnglers<-anglers_place(lakeGeom=lakes_round_base,
                         anglerBankDistribution = "Random",
                         anglerBoatDistribution = "Random",
                         totalAnglers = 100000,
                         percentBank = 50,
                         meanPartySizeBank=1,
                         meanPartySizeBoat=1,
                         maxPartySizeBoat=4,
                         mySeed=myRandomSeed)
tic()
ggplot() +
  geom_sf(data=lakes_round_base, fill="lightskyblue") +
  geom_sf(data=st_buffer(myAnglers, 35.89), color="red") +
  geom_point(data=myAnglers %>% st_coordinates() %>% data.frame(), aes(x=X, y=Y),  color="black", size=0.5) +
  #geom_sf(data=myFish, color="black", size=1.5) +
  labs(title="Anglers")
toc()

myCasts<-casts_place(myAnglers, numberCastsPerAngler=20)

 # ggplot() +
 #  geom_sf(data=myCasts_lines[3001:3060,]) #+
 #  #geom_sf(data=myFish, color="black", size=1.5)

#process spatial data for interactions
tmpFish<-st_intersects(st_buffer(myFish, 1), myCasts)

toc()

#add interaction count to myFish
myFish$castInteractions<-tmpFish %>% lengths
table(myFish$castInteractions)

#create dataframe of all interactions
myInteractions<-myFish[rep(seq_len(dim(myFish)[1]), myFish$castInteractions), 2]
myInteractions$anglerId<-myCasts$castId[unlist(tmpFish[tmpFish %>% lengths>0])]
myInteractions$castId<-myCasts$castId[unlist(tmpFish[tmpFish %>% lengths>0])]


ggplot() +
  geom_sf(data=myInteractions[c(1,3),]) +
  geom_sf(data=myCasts[myCasts %>% filter(anglerId %in% unique(myInteractions$anglerId) & castId %in% unique(myInteractions$castId)),])



ggplot() +
  #geom_sf(data=lakes_round_base, fill="lightskyblue") +
  geom_sf(data=myCasts %>% filter(anglerId == myCasts[3195,]$anglerId)) +
  geom_sf(data=st_buffer(myFish[tmp %>% lengths>0,],1), fill="red", color="transparent", alpha=0.4) +
  geom_sf(data=myFish[tmp %>% lengths>0,], size=1, color="black") 



theme_set(old)
