
# 000_Fresnel Automating ####

# 00_Fresnel Master Code ####

rm(list = ls())

library(tidyverse); library(fs); library(glue); library(zip)
library(conflicted)

conflict_prefer("unzip", "zip")

here::here() %>% setwd()

getwd()

Repos <- c(
  
  "albery-betacov",
  # "becker-betacov",
  "carlson-betacov",
  "dallas-betacov",
  "farrell-betacov",
  "guth-betacov",
  "poisot-betacov",
  "stock-betacov"
  
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

# 0a_Downloading Data Repos ####

source("R/00a_Downloading Data Repos.R")

# 0b_Downloading Model Repos ####

source("R/00b_Downloading Model Repos.R")

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

# Making the binary, thresholded file ####

source("R/X_Thresholder.R")
