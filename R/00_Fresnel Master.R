
# 00_Fresnel Master Code ####

rm(list = ls())

library(tidyverse); library(fs)

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
  "poisot-betacov")

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

# 0_Sourcing Models ####

r <- 4

for(r in seq_along(Repos)){
  
  print(Repos[r])
  
  setwd(paste0(here::here(),"/Github/Repos/", Repos[r]))
  
  if(SourceScripts %>% str_detect("[.]R$|[.]rmd$")){
    
    source(SourceScripts[[r]])
    
  }else{
    
    SourceScripts[[r]] %>% list.files(full.names = T, 
                                      pattern = "[.]R$|[.]rmd$|[.]Rmd$") ->
      
      SubSources
    
    file_copy(
      paste0(here::here(), "/Github/Repos/virionette/04_predictors/bat-supertree_clean.rds"),
      paste0(SourceScripts[[r]], "/bat-supertree_clean.rds"),
      overwrite = T
    )
    
    file_copy(
      paste0(here::here(), "/Github/Repos/virionette/03_interaction_data/virionette.csv"),
      paste0(SourceScripts[[r]], "/virionette.csv"),
      overwrite = T
    )
    
    rr <- 1
    
    for(rr in seq_along(SubSources)){
      
      print(SubSources[[rr]])
      
      purl(SubSources[[rr]], 
           output = paste0(SourceScripts[[r]]))#, "/", SubSources[[rr]]))
      
      purl(SubSources[[rr]], 
           output = paste0(SourceScripts[[r]], "/", (SubSources[[rr]]) %>% 
             str_split("/") %>% map_chr(2)) %>%
             str_replace_all(c(".Rmd" = ".R",
                               ".rmd" = ".R")))
      
      SubSources[[rr]] %>% 
        str_replace_all(c(".Rmd" = ".R",
                          ".rmd" = ".R")) %>%
        source
      
      ksource(SubSources[[rr]])
      
    }
  }
}


# 1_Uniting Predictions ####




# 2_Making Bat Figures ####



# 3_Making Mammal Figures ####



# 4_Making Maps ####

