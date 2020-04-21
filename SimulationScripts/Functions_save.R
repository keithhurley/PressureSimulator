save_simulation_run<-function(fileNameAndPath, ...){

  save(...,file=paste(fileNameAndPath, ".rData", sep=""))
  return(TRUE)
  
}

save_create_timestamp<-function() {
  return(paste(
                "_",
                str_pad(year(Sys.Date()), width=2, pad="0"),
                str_pad(month(Sys.Date()), width=2, pad="0"),
                str_pad(day(Sys.Date()), width=2, pad="0"),
                "_",
                str_pad(hour(Sys.time()), width=2, pad="0"),
                str_pad(minute(Sys.time()), width=2, pad="0"),
                str_pad(round(second(Sys.time())*100,0), width=4, pad="0"),
                sep=""))
}

save_create_timestamp()

