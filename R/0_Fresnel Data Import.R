
# Fresnel Data Import ####

library(tidyverse); library(data.table); library(fs)

if(file.exists("Data/AlberyPredicted.rds")){
  
  AlberyPredicted <- readRDS("Data/AlberyPredicted.rds")
  
}else{
  
  AlberyPredicted <- readRDS("https://github.com/gfalbery/PredictedNetwork.rds")
  
}

if(file.exists("Data/CarlsonPredicted.rds")){
  
  CarlsonPredicted <- fread("Data/CarlsonPredicted.csv")
  
}else{
  
  ("Data/AlberyPredicted.rds")
  
}