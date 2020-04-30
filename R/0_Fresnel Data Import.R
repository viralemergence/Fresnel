
# Fresnel Data Import ####

library(tidyverse); library(data.table); library(fs)

if(file.exists("Intermediate/AlberyPredicted.rds")){
  
  AlberyPredicted <- readRDS("Intermediate/AlberyPredicted.rds")
  AlberyPredictedBats <- readRDS("Intermediate/AlberyPredictedBats.rds")
  
}else{
  
  source("R/01_Albery Predictions.R")
  
}

if(file.exists("Data/CarlsonPredicted.rds")){
  
  CarlsonPredicted <- fread("Data/CarlsonPredicted.csv")
  
}else{
  
  source("R/02_Carlson Predicted.R")  
  
}