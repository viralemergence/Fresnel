
# Fresnel Data Import ####

library(tidyverse); library(data.table); library(fs)

if(file.exists("Intermediate/AlberyPredicted.rds")){
  
  AlberyPredicted <- readRDS("Intermediate/AlberyPredicted.rds")
  AlberyPredictedBats <- readRDS("Intermediate/AlberyPredictedBats.rds")
  
}else{
  
  source("R/01_Albery Predictions.R")
  
}


CarlsonPredicted <- fread("Data/CarlsonPredicted.csv")


BeckerPredicted <- fread("Data/PhylofactorPredicted.csv")

BeckerPredicted %>% arrange(desc(preds_allbetacov)) %>%
  mutate(BeckerRank = rank(preds_allbetacov)) %>% 
  mutate_at("BeckerRank", ~max(.x) - .x + 1) ->
  
  BeckerPredicted
