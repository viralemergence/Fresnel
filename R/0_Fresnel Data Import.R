
# Fresnel Data Import ####

library(tidyverse)

if(file.exists("Data/AlberyPredicted.rds")){
  
  AlberyPredicted <- readRDS("Data/AlberyPredicted.rds")
  
}