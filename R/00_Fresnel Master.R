
# 00_Fresnel Master Code ####

library(tidyverse); library(fs)

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

r <- 1

for(r in seq_along(Repos)){
  
  setwd(paste0("Github/Repos/", Repos[r]))
  
  source(SourceScripts[[r]])
  
  
}


# 1_Uniting Predictions ####




# 2_Making Bat Figures ####



# 3_Making Mammal Figures ####



# 4_Making Maps ####

