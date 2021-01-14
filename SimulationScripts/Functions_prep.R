  options(stringsAsFactors=FALSE)
  options(dplyr.summarise.inform = FALSE)
  
  library(tictoc)
  library(sf)
  library(tidyverse)
  library(stringr)
  library(lubridate)
  #library(profvis)
  library(foreach)
  library(doParallel)
  library(doRNG)
  
  
  source("./SimulationScripts/Functions_angler.R")
  source("./SimulationScripts/Functions_casts.R")
  source("./SimulationScripts/Functions_fish.R")
  source("./SimulationScripts/Functions_objects.R")
  source("./SimulationScripts/Functions_geometry.R")
  source("./SimulationScripts/Functions_simulations.R")
  source("./SimulationScripts/Functions_save.R")
  source("./Scripts/Functions_themes.R")
  
  
  old<-theme_set(myTheme)
  
  #theme_set(old)
  
  