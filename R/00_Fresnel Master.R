
# 00_Fresnel Master Code ####

rm(list = ls())

library(tidyverse); library(fs)

here::here() %>% setwd()

getwd()

Repos <- c(
  "albery-betacov",
  # "becker-betacov",
  "carlson-betacov",
  "dallas-betacov",
  "farrell-betacov",
  "guth-betacov",
  "poisot-betacov")

SourceScripts <- list(
  
  "R/01_Albery Predictions.R",
  #
  "BART-1.R",
  "models/Dallas2017.rmd",
  "scripts",
  "",
  NA
  
)

names(SourceScripts) <- Repos

# 0_Sourcing Models ####

r <- 2

for(r in seq_along(Repos)){
  
  print(Repos[r])
  
  setwd(paste0(here::here(),"/Github/Repos/", Repos[r]))
  
  source(SourceScripts[[r]])
  
  
}


# 1_Uniting Predictions ####




# 2_Making Bat Figures ####



# 3_Making Mammal Figures ####



# 4_Making Maps ####

