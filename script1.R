source("./SimulationScripts/Functions_prep.R")

myRandomSeed=12345

#load lake object
load(file="./data/lakes/round_1/lake.rData")
myLake<-list()
myLake$acres=100

#create parameter object
myParams<-create_parameters_object(
  randomSeed=12345,
  acres=myLake$acres,
  hoursPerAcre=100,
  tripLengthMean=3.4,
  tripLengthSd=1,
  castsPerHourMean=60,
  castsPerHourSd=20,
  numberFish=100,
  fishShorelineBuffer=0.5,
  anglerBoatDistribution="clustered",
  anglerBankDistribution="clustered",
  anglerBoatPartyRadius=1.5,
  anglerBankPartyRadius=3,
  meanPartySizeBoat=2,
  maxPartySizeBoat=4,
  meanPartySizeBank=2.2,
  maxPartySizeBank=5,
  boatShorelineBuffer=5,
  percentBank=50,
  meanCastDistance=7,
  sdCastDistance=3,
  meanCastsPerHour=60,
  sdCastsPerHour=20
)


#run simulations
myOutput<-runSimulations(myLakeObject,
               myParamObject,
               numberSimulations=1)
  
#save output
  #create name for run
  #create timestamp for run/filenames













# runSimulations<-function(myLakeObject,
#                          myParamObject,
#                          numberSimulations){
#   
#   myFish<-fish_place_random(lakeGeom=lake,
#                             numberFish=100,
#                             fishShorelineBuffer = 0.5,
#                             mySeed=12345)
# 
#   # ggplot() +
#   #   geom_sf(data=lakes_round_base, fill="lightskyblue") +
#   #   geom_sf(data=myFish) +
#   #   labs(title="Fish") 
# 
#   myAnglers<-anglers_place(lakeGeom=lake,
#                           lakeName=lake$name,
#                           anglerBoatDistribution = "Clustered By Party",
#                           anglerBankDistribution = "Clustered By Party",
#                           anglerBoatPartyRadius = 2.5,
#                           anglerBankPartyRadius = 3,
#                           totalAnglers = 100,
#                           meanPartySizeBoat=1.8,
#                           maxPartySizeBoat=5,
#                           meanPartySizeBank=2.4,
#                           maxPartySizeBank=6,
#                           boatShorelineBuffer= 5,
#                           percentBank = 50,
#                           mySeed=12345)
#   # tic()
#   # ggplot() +
#   #   geom_sf(data=lake, fill="lightskyblue") +
#   #   #geom_sf(data=st_buffer(myAnglers, 20), fill="red", alpha=0.25) +
#   #   geom_point(data=myAnglers %>% st_coordinates() %>% data.frame(), aes(x=X, y=Y),  color="black", size=0.5) +
#   #   #geom_sf(data=myFish, color="black", size=1.5) +
#   #   labs(title="Anglers")
#   # toc()
#   
# 
#   myCasts<-casts_place(lakeGeom=lake,
#                       myAnglers, 
#                       numberCastsPerAngler=60,
#                       meanCastDistance=10,
#                       sdCastDistance=3,
#                       meanCastsPerHour=40,
#                       sdCastsPerHour=15,
#                       mySeed=12345)
#   
#   # ggplot() +
#   #   geom_sf(data=lake, fill="navy") +
#   #   geom_sf(data=myCasts) +
#   #   geom_sf(data=myFish, color="red", size=1.5)
#   
#   #process spatial data for interactions
#   tmpFish<-st_intersects(st_buffer(myFish, 1), myCasts)
#   
#   toc()
#   
#   #add interaction count to myFish
#   myFish$castInteractions<-tmpFish %>% lengths
#   table(myFish$castInteractions)
#   
#   #create dataframe of all interactions
#   myInteractions<-myFish[rep(seq_len(dim(myFish)[1]), myFish$castInteractions), 2]
#   myInteractions$anglerId<-myCasts$castId[unlist(tmpFish[tmpFish %>% lengths>0])]
#   myInteractions$castId<-myCasts$castId[unlist(tmpFish[tmpFish %>% lengths>0])]
#   
#   toc()
#   
#   # ggplot() +
#   #   geom_sf(data=myInteractions[c(1,3),]) +
#   #   geom_sf(data=myCasts[myCasts %>% filter(anglerId %in% unique(myInteractions$anglerId) & castId %in% unique(myInteractions$castId)),])
#   
#   # ggplot() +
#   #   #geom_sf(data=lakes_round_base, fill="lightskyblue") +
#   #   geom_sf(data=myCasts %>% filter(anglerId == myCasts[3195,]$anglerId)) +
#   #   geom_sf(data=st_buffer(myFish[tmp %>% lengths>0,],1), fill="red", color="transparent", alpha=0.4) +
#   #   geom_sf(data=myFish[tmp %>% lengths>0,], size=1, color="black") 
#   
# }