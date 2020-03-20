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

load(file="./data/lakes/round_1.rData")

myFish<-fish_place_random(lakeGeom=lake,
                          numberFish=100,
                          fishShorelineBuffer = 0.5,
                          mySeed=12345)

# ggplot() +
#   geom_sf(data=lakes_round_base, fill="lightskyblue") +
#   geom_sf(data=myFish) +
#   labs(title="Fish") 

myAnglers<-anglers_place(lakeGeom=lake,
                         anglerBoatDistribution = "Clustered By Party",
                         anglerBankDistribution = "Clustered By Party",
                         anglerBoatPartyRadius = 2.5,
                         anglerBankPartyRadius = 3,
                         totalAnglers = 1000,
                         meanPartySizeBoat=1.8,
                         maxPartySizeBoat=5,
                         meanPartySizeBank=2.4,
                         maxPartySizeBank=6,
                         boatShorelineBuffer= 5,
                         percentBank = 50,
                         mySeed=12345)
tic()
ggplot() +
  geom_sf(data=lake, fill="lightskyblue") +
  #geom_sf(data=st_buffer(myAnglers, 20), fill="red", alpha=0.25) +
  geom_point(data=myAnglers %>% st_coordinates() %>% data.frame(), aes(x=X, y=Y),  color="black", size=0.5) +
  #geom_sf(data=myFish, color="black", size=1.5) +
  labs(title="Anglers")
toc()


myCasts<-casts_place(lakeGeom=lake,
                     myAnglers, 
                     numberCastsPerAngler=60,
                     meanCastDistance=10,
                     sdCastDistance=3,
                     mySeed=12345)



 ggplot() +
   geom_sf(data=lake, fill="navy") +
  geom_sf(data=myCasts) +
  geom_sf(data=myFish, color="red", size=1.5)

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
