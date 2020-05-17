
# 00_Fresnel Master Code ####

rm(list = ls())

library(tidyverse); library(fs); library(glue)

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

# 0_Downloading repos ####

if(Download){
  
  r <- 1
  
  for(r in r:length(Repos)){
    
   FocalRepo <- Repos[[r]] 
   
   download.file(url = paste0("https://github.com/gfalbery/", FocalRepo, "/archive/master.zip"),
                 destfile = paste0(here::here(), "/Github/Repos/", FocalRepo, ".zip"))
   
   unzip(paste0(here::here(), "/Github/Repos/", FocalRepo, ".zip"),
         exdir = paste0(here::here(), "/Github/Repos"))
   
   file.rename(paste0(here::here(), "/Github/Repos/", FocalRepo, "-master"),
               paste0(here::here(), "/Github/Repos/", FocalRepo))
   
   file_delete(paste0(here::here(), "/Github/Repos/", FocalRepo, ".zip"))
   
  }
}

# 0_Sourcing Models ####

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
        
        detach(package:dplyr)
        
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

