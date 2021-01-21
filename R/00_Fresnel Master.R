
# 00_Fresnel Master Code ####

rm(list = ls())

library(tidyverse); library(fs); library(glue); library(zip)
library(conflicted); library(magrittr)

dir_create("Github/Repos")
dir_create("Github/CSVs")

conflict_prefer("unzip", "zip")
conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("summarise", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("intersect", "base")
conflict_prefer("as.matrix", "base")
conflict_prefer("map", "purrr")
conflict_prefer("select", "dplyr")

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
  NA,
  "scripts"
  
)

names(SourceScripts) <- Repos

ksource <- function(x, ...) {
  
  library(knitr)
  
  source(purl(x, output = tempfile()), ...)
  
}

# 0a_Downloading Data Repos ####

dir_delete("Github/Repos/virionette")

source("R/00a_Downloading Data Repos.R")

# Dictating whether to add the most up-to-date betacov predictions ####

AddNewData <- T

if(AddNewData){
  
  if(file.exists(paste0(here::here(), 
                        '/Github/Repos/virionette/03_interaction_data/OldVirionette.csv'))){
    
    Virionette <- read_csv(paste0(here::here(), 
                                  '/Github/Repos/virionette/03_interaction_data/OldVirionette.csv'))
    
  }else{
    
    Virionette <- read_csv(paste0(here::here(), 
                                  '/Github/Repos/virionette/03_interaction_data/virionette.csv'))
    
    Virionette %>% 
      write.csv(paste0(here::here(), 
                       '/Github/Repos/virionette/03_interaction_data/OldVirionette.csv'))
    
  }
  
  BinaryWebsite <- read_csv("BinaryWebsite.csv")
  
  NewData <- 
    BinaryWebsite %>% filter(`New data` == "New data") %>% 
    mutate(virus_genus = "Betacoronavirus", host_order = "Chiroptera") %>% 
    dplyr::select(host_species = Sp, host_order, virus_genus)
  
  Virionette %>% 
    # anti_join(NewData, by = c("host_species", "host_order", "virus_genus")) %>% #nrow
    bind_rows(NewData) %>% 
    # nrow
    mutate_at("host_species", ~str_replace_all(.x, "Myonycteris angolensis", "Lissonycteris angolensis")) %>% 
    write.csv(paste0(here::here(), 
                     '/Github/Repos/virionette/03_interaction_data/virionette.csv'))
  
}

# 0b_Downloading Model Repos ####

Repos %>% paste0("Github/Repos/", .) %>% map(dir_delete)
Repos %>% paste0("Github/Repos/", ., ".zip") %>% map(file_delete)

source("R/00b_Downloading Model Repos.R")

# Deleting Output CSVs

OutputCSVs <- list(
  
  glue("Output Files/{c('AlberyBats', 'AlberyNonBats')}.csv"),
  
  # ,
  
  c(glue("Carlson{c('Dart', 'Dart', 'Bart', 'Bart')}{c('Citations', 'Uncorrected', 'Citations', 'Uncorrected')}.csv")),
  
  c(glue("Dallas{c('Mammals', 'Mammals', 'Bats', 'Bats')}{c('Citations', 'Uncorrected', 'Citations', 'Uncorrected')}.csv")),
  
  c(glue("results/Farrell{c('Mammals', 'Mammals', 'Bat', 'Bat')}{c('Phylogeny', 'Full', 'Phylogeny', 'Full')}.csv")),
  
  c(glue("Guth{c('Citations', 'Uncorrected')}.csv")),
  
  c(glue("predictions/Poisot{c(rep('Knn', 4), rep('Lf', 2))}{c(1,1,2,2,'','')}{rep(c('Bat','Mammal'), 3)}.csv")),
  
  c(glue("05_results/Stock_both{c('', '_nocites')}.csv"))
  
)

names(OutputCSVs) <- Repos

for(r in seq_along(Repos)){
  
  print(paste("Removing", Repos[r], "CSVs"))
  
  paste0(here::here(),"/Github/Repos/", Repos[r]) %>% 
    dir_ls(recurse = T) -> Files
  
  paste0(here::here(),"/Github/Repos/", Repos[r], "/", OutputCSVs[[Repos[r]]]) %>% 
    paste(collapse = "|") -> ToRemove
  
  Files[str_detect(Files, ToRemove)] %>% 
    map(file.remove)
  
  
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
                                            pattern = "[.]r$|[.]R$|[.]rmd$|[.]Rmd$") ->
            
            SubSources
          
        }
        
        # rr <- 2
        
        for(rr in seq_along(SubSources)){
          
          #invisible(lapply(paste0('package:', 
          #                        names(sessionInfo()$otherPkgs)), 
          #                 detach, 
          #                 character.only = TRUE, 
          #                 unload = TRUE))
          
          #detach(package:dplyr)
          
          print(SubSources[[rr]])
          
          if(stringr::str_detect(SubSources[[rr]], "[.]R$|[.]r$")){
            
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
  
  c(glue("predictions/Poisot{c(rep('Knn', 4), rep('Lf', 2))}{c(1,1,2,2,'','')}{rep(c('Bat','Mammal'), 3)}.csv")),
  
  c(glue("05_results/Stock_both{c('', '_nocites')}.csv"))
  
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

