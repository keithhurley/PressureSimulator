myLakeObject=myLakeObject
myParamsObject=myParamsObject
mySimsObject=mySimsObject


#angler place
myLakeObject=myLakeObject
anglerBoatDistribution = myParamsObject$anglerBoatDistribution
anglerBankDistribution = myParamsObject$anglerBankDistribution
anglerBoatPartyRadius = myParamsObject$anglerBoatPartyRadius
anglerBankPartyRadius = myParamsObject$anglerBankPartyRadius
totalAnglers = myParamsObject$totalAnglers
bankAnglers=myParamsObject$bankAnglers
boatAnglers=myParamsObject$boatAnglers
meanPartySizeBoat=myParamsObject$meanPartySizeBoat
maxPartySizeBoat=myParamsObject$maxPartySizeBoat
meanPartySizeBank=myParamsObject$meanPartySizeBank
maxPartySizeBank=myParamsObject$maxPartySizeBank
boatShorelineBuffer= myParamsObject$boatShorelineBuffer
percentBank = myParamsObject$percentBank
anglerBankRestrictions=myLakeObject$restrictionsShore
anglerBoatRestrictions = myLakeObject$restrictionsBoat
anglerBankProbs=myLakeObject$probsShore
anglerBoatProbs=myLakeObject$probsBoat
mySeed=mySimsObject$seed
numberSims=mySimsObject$numberSimulations
parGroupSize=mySimsObject$parGroupSize
parNumberCores=mySimsObject$parNumberCores


#boat angler place
myLakeObject=myLakeObject
numberAnglers=boatAnglers
meanPartySizeBoat=meanPartySizeBoat
maxPartySizeBoat=maxPartySizeBoat
anglerBoatDistribution=anglerBoatDistribution
anglerBoatPartyRadius = anglerBoatPartyRadius
anglerBoatRestrictions = anglerBoatRestrictions
anglerBoatProbs=anglerBoatProbs
boatShorelineBuffer=boatShorelineBuffer
mySeed=mySeed
numberSims=numberSims
parGroupSize
parNumberCores=12


#bank angler place
myLakeObject=myLakeObject
numberAnglers=bankAnglers
meanPartySizeBank=meanPartySizeBank
maxPartySizeBank=maxPartySizeBank
anglerBankDistribution = anglerBankDistribution
anglerBankPartyRadius=anglerBankPartyRadius
anglerBankRestrictions = anglerBankRestrictions
anglerBankProbs= anglerBankProbs
mySeed=mySeed
numberSims=numberSims
parGroupSize
parNumberCores

#casts place
lakeGeom=myLakeObject$lakeGeom
myAnglers=myResults$myAnglers 
castDistanceMean=myParamsObject$castDistanceMean
castDistanceSd=myParamsObject$castDistanceSd
castsPerHourMean=myParamsObject$castsPerHourMean
castsPerHourSd=myParamsObject$castsPerHourSd
mySeed=mySimsObject$seed

