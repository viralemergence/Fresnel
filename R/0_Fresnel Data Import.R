
# Fresnel Data Import ####

library(tidyverse); library(data.table); library(fs); library(ggregplot)

rm(list = ls())

Panth1 <- read.delim("Data/PanTHERIA_1-0_WR05_Aug2008.txt") %>%
  dplyr::rename_all(~str_replace(.x, "MSW05_", "h")) %>%
  rename(Sp = hBinomial)

Panth1$Sp <- Panth1$Sp %>% str_replace(" ", "_")

if(file.exists("Intermediate/AlberyPredicted.rds")){
  
  AlberyPredicted <- readRDS("Intermediate/AlberyPredicted.rds")
  AlberyPredictedBats <- readRDS("Intermediate/AlberyPredictedBats.rds")
  
}else{
  
  source("R/01_Albery Predictions.R")
  
}

CarlsonPredicted <- fread("Data/CarlsonPredicted.csv")

CarlsonPredicted %>% rename_all(CamelConvert) %>% 
  mutate_at("Host_species", ~.x %>% str_trim %>% str_replace_all(" ", "_")) ->
  
  CarlsonPredicted


# 03_Damas Predictions
# Adding the Damas dataset for ####
# https://www.biorxiv.org/content/10.1101/2020.04.16.045302v1.supplementary-material

Damas <- read.csv("Data/Damas_Ace2.csv", header = T)

Damas %>% 
  rename(DamasRank = Sort.by.predicted.susceptibility) %>%
  arrange(DamasRank) -> DamasRank

DamasRank %>% 
  mutate_at("Species", ~.x %>% str_trim %>% str_replace_all(" ", "_")) ->
  
  Damas

BeckerPredicted <- fread("Data/PhylofactorPredicted.csv")

BeckerPredicted %>% arrange(desc(preds_allbetacov)) %>%
  mutate(BeckerRank = rank(preds_allbetacov)) %>% 
  mutate_at("BeckerRank", ~max(.x) - .x + 1) %>% rename(Sp = V1) ->
  
  BeckerPredicted

PoisotPredicted <- fread("Data/PoisotPredicted.csv")

PoisotPredicted %>%
  rename_all(CamelConvert) %>%
  #filter(Virus %>% str_detect("Beta")) %>%
  mutate_at("Host", ~.x %>% str_trim %>% str_replace_all(" ", "_")) %>%
  rename(PoisotRank = Match) -> 
  
  PoisotPredicted

FarrellPredicted <- 
  #read_csv("https://github.com/ViromeNet/farrell-betacov/blob/master/results/batcov_elmasri_phylo_pred_betacovsOnly.csv")
  read_csv("Data/FarrellPredicted.csv")

#FarrellPredicted %>% write.csv("Data/FarrellPredicted.csv")

FarrellPredicted %>% rename(FarrellRank = rank) ->
  FarrellPredicted
