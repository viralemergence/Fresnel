
# 00a_Downloading Data Repos ####

library(tidyverse); library(fs); library(glue); library(zip)
library(conflicted)

conflict_prefer("unzip", "zip")

here::here() %>% setwd()

getwd()


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

file_delete(paste0(here::here(), "/Github/Repos/", "virionette", ".zip"))

download.file(url = paste0("https://github.com/viralemergence/", "becker-betacov", "/archive/master.zip"),
              destfile = paste0(here::here(), "/Github/Repos/", "becker-betacov", ".zip"))

unzip(paste0(here::here(), "/Github/Repos/", "becker-betacov", ".zip"),
      exdir = paste0(here::here(), "/Github/Repos/"))

file.rename(paste0(here::here(), "/Github/Repos/", "becker-betacov", "-master"),
            paste0(here::here(), "/Github/Repos/", "becker-betacov"))

file_delete(paste0(here::here(), "/Github/Repos/", "becker-betacov", ".zip"))


