
# 00b_Downloading Model Repos ####

library(tidyverse); library(fs); library(glue); library(zip)
library(conflicted)

conflict_prefer("unzip", "zip")

# function from https://stackoverflow.com/questions/10966109/how-to-source-r-markdown-file-like-sourcemyfile-r

ksource <- function(x, ...) {
  
  library(knitr)
  
  source(purl(x, output = tempfile()), ...)
  
}

Repos <- c(
  
  "albery-betacov",
  "carlson-betacov",
  "dallas-betacov",
  "farrell-betacov",
  "guth-betacov",
  "poisot-betacov"
  
)

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