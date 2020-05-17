
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
      
      rr <- 1
      
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

purl(SubSources[[rr]], 
     output = paste0(SourceScripts[[r]], "/", 
                     (SubSources[[rr]]) %>% 
                       str_split("/") %>% 
                       map_chr(2)) %>%
       str_replace_all(c(".Rmd" = ".R",
                         ".rmd" = ".R")))

#SubSources[[rr]] %>% 
#  str_replace_all(c(".Rmd" = ".R",
#                    ".rmd" = ".R")) %>%
#  source#

#}
##}
#}

# 0b_Copying across csvs ####


# 1_Uniting Predictions ####




# 2_Making Bat Figures ####



# 3_Making Mammal Figures ####



# 4_Making Maps ####

