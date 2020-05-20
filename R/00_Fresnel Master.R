
# 00_Fresnel Master Code ####

rm(list = ls())

library(tidyverse); library(fs); library(glue); library(zip)
library(conflicted)

conflict_prefer("unzip", "zip")

here::here() %>% setwd()

getwd()

# function from https://stackoverflow.com/questions/10966109/how-to-source-r-markdown-file-like-sourcemyfile-r

ksource <- function(x, ...) {
  
  library(knitr)
  
  source(purl(x, output = tempfile()), ...)
  
}

Repos <- c(
  
  "albery-betacov",
  # "becker-betacov",
  "carlson-betacov",
  "dallas-betacov",
  "farrell-betacov",
  "guth-betacov",
  "poisot-betacov"
  
)

SourceScripts <- list(
  
  "R/01_Albery Predictions.R",
  #
  "BART-1.R",
  "models",
  "scripts",
  "",
  NA
  
)

names(SourceScripts) <- Repos

# Master Files ####

MasterFiles <- T

if(MasterFiles){
  
  download.file(url = paste0("https://github.com/viralemergence/", "virionette", "/archive/master.zip"),
                destfile = paste0(here::here(), "/Github/Repos/", "virionette", ".zip"))
  
  unzip(paste0(here::here(), "/Github/Repos/", "virionette", ".zip"),
        exdir = paste0(here::here(), "/Github/Repos/"))
  
  Virionette <- read.csv("Github/Repos/virionette-master/03_interaction_data/virionette.csv")
  Citations <- read.csv("Github/Repos/virionette-master/04_predictors/Citations.csv")
  HanTraits <- read.csv("Github/Repos/virionette-master/04_predictors/Han-BatTraits.csv")
  
  BatTree <- readRDS("Github/Repos/virionette-master/04_predictors/bat-supertree_clean.rds")
  SuperTree <- readRDS("Github/Repos/virionette-master/04_predictors/Full Supertree.rds")
  
  file.rename(paste0(here::here(), "/Github/Repos/", "virionette", "-master"),
              paste0(here::here(), "/Github/Repos/", "virionette"))
  
  download.file(url = paste0("https://github.com/viralemergence/", "becker-betacov", "/archive/master.zip"),
                destfile = paste0(here::here(), "/Github/Repos/", "becker-betacov", ".zip"))
  
  unzip(paste0(here::here(), "/Github/Repos/", "becker-betacov", ".zip"),
        exdir = paste0(here::here(), "/Github/Repos/"))
  
  file.rename(paste0(here::here(), "/Github/Repos/", "becker-betacov", "-master"),
              paste0(here::here(), "/Github/Repos/", "becker-betacov"))
  
  
}

# 0_Downloading repos ####

Download <- F

if(Download){
  
  r <- 1
  
  for(r in r:length(Repos)){
    
    FocalRepo <- Repos[[r]] 
    
    if(dir.exists(paste0(here::here(), "/Github/Repos/", FocalRepo))){
      
      dir_delete(paste0(here::here(), "/Github/Repos/", FocalRepo))
      
    }
    
    download.file(url = paste0("https://github.com/viralemergence/", FocalRepo, "/archive/master.zip"),
                  destfile = paste0(here::here(), "/Github/Repos/", FocalRepo, ".zip"))
    
    unzip(paste0(here::here(), "/Github/Repos/", FocalRepo, ".zip"),
          exdir = paste0(here::here(), "/Github/Repos"))
    
    file.rename(paste0(here::here(), "/Github/Repos/", FocalRepo, "-master"),
                paste0(here::here(), "/Github/Repos/", FocalRepo))
    
  }
  
  for(r in r:length(Repos)){
    
    FocalRepo <- Repos[[r]] 
    
    if(dir.exists(paste0(here::here(), "/Github/Repos/", FocalRepo, ".zip"))){
      
      
      file_delete(paste0(here::here(), "/Github/Repos/", FocalRepo, ".zip"))
      
    }
  }
}

# 0_Sourcing Models ####

ModelRun <- T

if(ModelRun){
  
  r <- 1
  
  for(r in r:length(Repos)){
    
    print(Repos[r])
    
    if(!Repos[r] == "poisot-betacov"){
      
      setwd(paste0(here::here(),"/Github/Repos/", Repos[r]))
      
      if(SourceScripts[[r]] %>% str_detect("[.]R$|[.]rmd$")){
        
        source(SourceScripts[[r]])
        
      }else{
        
        if(!(SourceScripts[[r]] %>% nchar)){
          
          list.files(full.names = T, 
                     pattern = "[.]R$|[.]rmd$|[.]Rmd$") ->
            
            SubSources
          
        }else{
          
          SourceScripts[[r]] %>% list.files(full.names = T, 
                                            pattern = "[.]R$|[.]rmd$|[.]Rmd$") ->
            
            SubSources
          
        }
        
        rr <- 2
        
        for(rr in seq_along(SubSources)){
          
          #invisible(lapply(paste0('package:', 
          #                        names(sessionInfo()$otherPkgs)), 
          #                 detach, 
          #                 character.only = TRUE, 
          #                 unload = TRUE))
          
          #detach(package:dplyr)
          
          print(SubSources[[rr]])
          
          if(stringr::str_detect(SubSources[[rr]], "[.]R$")){
            
            source(SubSources[[rr]])
            
          }else{
            
            ksource(SubSources[[rr]])
            
          }
          
        }
      }
    }
  }
}

# Saving some sourcing script ####

#purl(SubSources[[rr]], 
#     output = paste0(SourceScripts[[r]], "/", 
#                     (SubSources[[rr]]) %>% 
#                       str_split("/") %>% 
#                       map_chr(2)) %>%
#       str_replace_all(c(".Rmd" = ".R",
#                         ".rmd" = ".R")))


# 0b_Copying across csvs ####

OutputCSVs <- list(
  
  glue("Output Files/{c('AlberyBats', 'AlberyNonBats')}.csv"),
  
  # ,
  
  c(glue("Carlson{c('Dart', 'Dart', 'Bart', 'Bart')}{c('Citations', 'Uncorrected', 'Citations', 'Uncorrected')}.csv")),
  
  c(glue("Dallas{c('Mammals', 'Mammals', 'Bats', 'Bats')}{c('Citations', 'Uncorrected', 'Citations', 'Uncorrected')}.csv")),
  
  c(glue("results/Farrell{c('Mammals', 'Mammals', 'Bat', 'Bat')}{c('Phylogeny', 'Full', 'Phylogeny', 'Full')}.csv")),
  
  c(glue("Guth{c('Citations', 'Uncorrected')}.csv")),
  
  c(glue("predictions/Poisot{c(rep('Knn', 4), rep('Lf', 2))}{c(1,1,2,2,'','')}{rep(c('Bat','Mammal'), 3)}.csv"))
  
)

names(OutputCSVs) <- Repos

i <- 1
j <- 1

conflict_prefer("last", "dplyr")

here::here() %>% setwd()

for(i in i:length(Repos)){
  
  j <- 1
  
  for(j in j:length(OutputCSVs[[Repos[[i]]]])){
    
    print(j)
    
    file_copy(
      
      path = paste0("Github/Repos/", Repos[i], "/", 
                    OutputCSVs[[Repos[i]]][[j]]),
      
      new_path = 
        paste0("Github/", "CSVs", "/", 
               OutputCSVs[[Repos[i]]][[j]] %>%
                 str_split("/") %>% map_chr(last)
               
        ),
      
      overwrite = T
    )
    
    
  }
}

# 1_Uniting Predictions ####

source("R/01_Fresnel Import.R")

# 2_Phylofactor ####

source("R/02_Phylofactor.R")

# 3a_Making Bat Figures ####

source("R/03a_Bat Figures.R")

# 3b_Making Mammal Figures ####

source("R/03b_Mammal Figures.R")

# 4_Making Maps ####

