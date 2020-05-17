
# Importing predictions and creating ranked predictions ####

library(tidyverse); library(fs); library(magrittr); library(ggregplot)
library(conflicted); library(dplyr)

conflict_prefer("map", "purrr")

c("select", "filter", "intersect", "summarise", "mutate", "rename", "arrange") %>%
  map(~conflict_prefer(.x, "dplyr"))

rm(list = ls())

here::here() %>% setwd()

Virionette <- 
  read_csv(paste0(here::here(),
                  "/GitHub/Repos/virionette/03_interaction_data/virionette.csv"))

BatsVsOther <- read.csv("Data/BatsVsOther.csv") %>%
  rename_all(CamelConvert) %>% 
  mutate_at("Bats", ~.x %>% as.character %>% CamelConvert)

Repos <- c(
  "albery-betacov",
  "becker-betacov",
  "carlson-batcov",
  "dallas-betacov",
  "farrell-betacov",
  "guth-betacov",
  "poisot-betacov")

# ~~~~~ Bats ####

GithubDir <- "Github/CSVs/"

albery <- read_csv(paste0(GithubDir, "AlberyBats.csv"))
carlson1 <- read_csv(paste0(GithubDir, "CarlsonBartCitations.csv"))
carlson2 <- read_csv(paste0(GithubDir, "CarlsonBartUncorrected.csv"))
carlson3 <- read_csv(paste0(GithubDir, "CarlsonDartCitations.csv"))
carlson4 <- read_csv(paste0(GithubDir, "CarlsonDartUncorrected.csv"))
dallas1 <- read_csv(paste0(GithubDir,"DallasMammalsCitations.csv"))
farrell1 <- read_csv(paste0(GithubDir, "FarrellBatFull.csv"))
farrell2 <- read_csv(paste0(GithubDir, "FarrellBatPhylogeny.csv"))
guth1 <- read_csv(paste0(GithubDir, "GuthCitations.csv"))
guth2 <- read_csv(paste0(GithubDir, "GuthUncorrected.csv"))
poisot1 <- read_csv(paste0(GithubDir, "PoisotKnn1Bat.csv"), col_names = FALSE)
poisot2 <- read_csv(paste0(GithubDir, "PoisotKnn2Bat.csv"), col_names = FALSE)
poisot3 <- read_csv(paste0(GithubDir, "PoisotLfBat.csv"), col_names = FALSE)

# Triaging column names ####

albery %>% rename(P.Alb = Count) -> albery
carlson1 %>% select(host_species, pred) %>% rename(Sp = host_species, P.Car1 = pred) -> carlson1
carlson2 %>% select(host_species, pred) %>% rename(Sp = host_species, P.Car2 = pred) -> carlson2
carlson3 %>% select(host_species, pred) %>% rename(Sp = host_species, P.Car3 = pred) -> carlson3
carlson4 %>% select(host_species, pred) %>% rename(Sp = host_species, P.Car4 = pred) -> carlson4
dallas1 %>% select(host, suitability) %>% rename(Sp = host, P.Dal1 = suitability) -> dallas1
farrell1 %>% select(Host, p.interaction) %>% rename(Sp = Host, P.Far1 = p.interaction) -> farrell1
farrell2 %>% select(Host, p.interaction) %>% rename(Sp = Host, P.Far2 = p.interaction) -> farrell2
guth1 %>% select(host_species, pred_med) %>% rename(Sp = host_species, P.Gut1 = pred_med) -> guth1
guth2 %>% select(host_species, pred_med) %>% rename(Sp = host_species, P.Gut2 = pred_med) -> guth2
poisot1 %>% rename(Sp = X1, P.Po1 = X2) -> poisot1
poisot2 %>% rename(Sp = X1, P.Po2 = X2) -> poisot2
poisot3 %>% rename(Sp = X1, P.Po3 = X2) -> poisot3

# Specifically subset Tad ####

assoc <- Virionette %>% 
  filter(host_order == 'Chiroptera')

dallas1 %>% as_tibble %>% mutate(InAssocBats = (Sp %in% assoc$host_species)) %>%
  filter(InAssocBats==1) -> dallas1

# Drops or not ####

ModelList <- list(albery, carlson3, dallas1, farrell1, guth1, poisot2, poisot3) 

# Mutating ####

ModelList %>%
  map(~.x %>% 
        mutate_at("Sp", function(a) a %>% 
                    str_trim %>% 
                    str_replace("_", " ")) %>%
        mutate_at(vars(starts_with("P.")), function(a) rank(a, na.last = "keep")) %>%
        mutate_at(vars(starts_with("P.")), function(a) max(na.omit(a)) - a + 1) %>%
        rename_all(function(a) str_replace_all(a, "^P.", "R.")) %>%
        bind_cols(.x %>% dplyr::select(starts_with("P.")))) %>%
  reduce(full_join) %>% 
  mutate_at("Sp", ~.x %>% 
              str_trim %>% 
              str_replace("_", " ")) -> 
  
  Models

# Add in betacov true/false

Virionette %>% 
  
  group_by(host_species) %>% 
  summarise(Betacov = as.numeric(any(virus_genus == "Betacoronavirus"))) %>%
  
  select(Sp = host_species, Betacov) %>%
  mutate_at("Sp", ~.x %>% str_trim %>% str_replace_all("_"," ")) -> 
  truth

Models %>% 
  left_join(truth) %>% 
  mutate(Betacov = replace_na(Betacov, 0)) ->
  Models

# Generate proportional rankings

Models %<>% 
  mutate(Rank = rowMeans(select(Models, starts_with("R.")), na.rm = TRUE))

Models %>% select(starts_with("R.")) %>% names -> 
  RankNames

Models %<>% 
  mutate_at(RankNames, ~.x/max(.x, na.rm = T)) %>%  
  mutate(PropRank = rowMeans(select(Models, starts_with("R.")) %>% 
                               mutate_at(RankNames, ~.x/max(.x, na.rm = T)), 
                             na.rm = TRUE)) %>% 
  arrange(Rank)

Models %>% 
  select(Sp, Betacov, starts_with("R."), starts_with("P."), 
         Rank, PropRank) %>%
  as.data.frame -> 
  BatModels

# Splitting in-sample 

BatModels %<>% mutate(InSample = as.numeric(Sp %in% Virionette$host_species))

BatModels_IS <- BatModels %>% filter(!(!InSample))
BatModels_OS <- BatModels #%>% filter(!(InSample))

NACols <- BatModels_OS %>% is.na %>% colSums

NACols[NACols >= (nrow(BatModels_OS) - nrow(BatModels_IS))] %>% 
  names %>% setdiff(names(BatModels_OS), .) %>%
  select(BatModels_OS, .) ->
  
  BatModels_OS

BatModels_OS %<>% arrange(PropRank)
BatModels_IS %<>% arrange(PropRank)

# ~~~~~ NonBats ####

GithubDir <- "Github/CSVs/"

albery <- read_csv(paste0(GithubDir, "AlberyNonBats.csv"))
dallas1 <- read_csv(paste0(GithubDir, "DallasMammalsCitations.csv"))
dallas2 <- read_csv(paste0(GithubDir, "DallasMammalsUncorrected.csv"))
farrell1 <- read_csv(paste0(GithubDir, "FarrellMammalsFull.csv"))
farrell2 <- read_csv(paste0(GithubDir, "FarrellMammalsPhylogeny.csv"))
poisot1 <- read_csv(paste0(GithubDir, "PoisotKnn1Mammal.csv"), col_names = FALSE)
poisot2 <- read_csv(paste0(GithubDir, "PoisotKnn2Mammal.csv"), col_names = FALSE)
poisot3 <- read_csv(paste0(GithubDir, "PoisotLfMammal.csv"), col_names = FALSE)

# Triaging column names ####

albery %>% rename(P.Alb = Count) -> albery
dallas1 %>% select(host, suitability) %>% rename(Sp = host, P.Dal1 = suitability) -> dallas1
dallas2 %>% select(host, suitability) %>% rename(Sp = host, P.Dal2 = suitability) -> dallas2
farrell1 %>% select(Host, p.interaction) %>% rename(Sp = Host, P.Far1 = p.interaction) -> farrell1
farrell2 %>% select(Host, p.interaction) %>% rename(Sp = Host, P.Far2 = p.interaction) -> farrell2
poisot1 %>% rename(Sp = X1, P.Po1 = X2) -> poisot1
poisot2 %>% rename(Sp = X1, P.Po2 = X2) -> poisot2
poisot3 %>% rename(Sp = X1, P.Po3 = X2) -> poisot3

# Drops or not ####

ModelList <- list(albery, dallas1, farrell1, poisot2, poisot3) 

# Mutating ####

ModelList %>%
  map(~.x %>% 
        mutate_at("Sp", function(a) a %>% 
                    str_trim %>% 
                    str_replace("_", " ")) %>%
        #filter(!(Sp = 'Homo sapiens'))  %>% 
        mutate_at(vars(starts_with("P.")), function(a) rank(a, na.last = "keep")) %>%
        mutate_at(vars(starts_with("P.")), function(a) max(na.omit(a)) - a + 1) %>%
        rename_all(function(a) str_replace_all(a, "^P.", "R.")) %>%
        bind_cols(.x %>% select(starts_with("P.")))) %>%
  reduce(full_join) %>% 
  mutate_at("Sp", ~.x %>% 
              str_trim %>% 
              str_replace("_", " ")) %>% 
  select(Sp, starts_with("R.")) -> 
  
  Models

# Add in betacov true/false

Virionette %>% 
  
  group_by(host_species) %>% 
  summarise(Betacov = as.numeric(any(virus_genus == "Betacoronavirus"))) %>%
  
  select(Sp = host_species, Betacov) %>%
  mutate_at("Sp", ~.x %>% str_trim %>% str_replace_all("_"," ")) -> 
  truth

Models %>% 
  left_join(truth) %>% 
  mutate(Betacov = replace_na(Betacov, 0)) ->
  Models

Models %<>% left_join(BatsVsOther %>%
                        mutate_at("Name", ~.x %>% 
                                    str_trim %>% 
                                    str_replace_all("_", " ")) %>%
                        dplyr::select(Name, Bats), 
                      by = c("Sp" = "Name")) %>%
  filter(Bats == "Other") %>%
  as.data.frame()

# Generate proportional rankings

Models %<>% 
  mutate(Rank = rowMeans(select(Models, starts_with("R.")), na.rm = TRUE))

Models %>% select(starts_with("R.")) %>% names -> 
  RankNames

Models %<>% 
  mutate_at(RankNames, ~.x/max(.x, na.rm = T)) %>%  
  mutate(PropRank = rowMeans(select(Models, starts_with("R.")) %>% 
                               mutate_at(RankNames, ~.x/max(.x, na.rm = T)), 
                             na.rm = TRUE)) %>% 
  arrange(PropRank)

Models %>% 
  select(Sp, Betacov, starts_with("R."), starts_with("P."), 
         Rank, PropRank) %>%
  as.data.frame -> 
  NonBatModels

# Splitting in-sample 

NonBatModels %<>% mutate(InSample = as.numeric(Sp %in% Virionette$host_species))

NonBatModels_IS <- NonBatModels %>% filter(!(!InSample))
NonBatModels_OS <- NonBatModels #%>% filter(!(InSample))

NACols <- NonBatModels_OS %>% is.na %>% colSums

NonBatModels_OS %<>% select(Sp, Betacov, R.Alb, Rank, PropRank, InSample)

NonBatModels_OS %<>% arrange(PropRank)
NonBatModels_IS %<>% arrange(PropRank)

if(0){
  
  # dir_create("Cleaned Files")
  
  #BatModels_OS %>% write.csv("Cleaned Files/BatModels_OS.csv", row.names = F)
  #BatModels_IS %>% write.csv("Cleaned Files/BatModels_IS.csv", row.names = F)
  #NonBatModels_OS %>% write.csv("Cleaned Files/NonBatModels_OS.csv", row.names = F)
  #NonBatModels_IS %>% write.csv("Cleaned Files/NonBatModels_IS.csv", row.names = F)
  
  
  
  batin %>% gather("Key", "Value", -c(Sp, Betacov, Rank, PropRank, InSample)) %>%
    left_join(
      
      batin2 %>% gather("Key", "Value", -c(Sp, Betacov, Rank, PropRank, InSample)),
      
      by = c("Sp", "Key")
    ) %>% 
    
    ggplot(aes(Value.x, Value.y)) + geom_point() + 
    facet_wrap(~Key)
  
  batout %>% gather("Key", "Value", -c(Sp, Betacov, Rank, PropRank, InSample)) %>%
    left_join(
      
      batout2 %>% gather("Key", "Value", -c(Sp, Betacov, Rank, PropRank, InSample)),
      
      by = c("Sp", "Key")
    ) %>% 
    
    ggplot(aes(Value.x, Value.y)) + geom_point() + 
    facet_wrap(~Key)
  
}