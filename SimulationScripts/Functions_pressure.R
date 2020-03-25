createPressureObject<-function(myAcres=1, myHoursPerAcre=100, myTripLength_Mean=3, 
                               myTripLength_Sd=1, myCastsPerHour_Mean=30, myCastsPerHour_Sd=10) {
  myPressureList<-list(myAcres=myAcres,
                       myHoursPerAcre=myHoursPerAcre,
                       myTripLength_Mean=myTripLength_Mean,
                       myTripLength_Sd=myTripLength_Sd,
                       myCastsPerHour_Mean=myCastsPerHour_Mean,
                       myCastsPerHour_Sd=myCastsPerHour_Sd)
  myPressureList$myNumberAnglers<-round((myHoursPerAcre*myAcres)/myTripLength_Mean, 0)
  myPressureList$projectedNumberOfCasts<-(myHoursPerAcre*myAcres)*myCastsPerHour_Mean
  return(myPressureList)
}

