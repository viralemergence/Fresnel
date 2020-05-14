
# Importing predictions and creating ranked predictions ####

library(tidyverse); library(fs)

rm(list = ls())

Repos <- c(
  "albery-betacov",
  "becker-betacov",
  "carlson-batcov",
  "dallas-betacov",
  "farrell-betacov",
  "guth-betacov",
  "poisot-betacov")

# ~~~~~ Bats ####

GithubDir <- "Github/BatCSVs/"

albery <- read_csv(paste0(GithubDir, "AlberyBats.csv"))
becker <- read_csv(paste0(GithubDir, "PhylofactorPredictions.csv"))
carlson <- read_csv(paste0(GithubDir, "batcov-bart.csv"))
dallas <- read_csv(paste0(GithubDir, "DallasPredictions.csv"))
farrell <- read_csv(paste0(GithubDir, "FarrellPredicted.csv"))
guth <- read_csv(paste0(GithubDir, "GuthPredictions.csv"))
poisot1 <- read_csv(paste0(GithubDir, "PoisotTanimotoChiropteraToChiropteraPredictions.csv"), col_names = FALSE)
poisot2 <- read_csv(paste0(GithubDir, "PoisotLinearFilterChiropteraToChiropteraPredictions.csv"), col_names = FALSE)

# Triaging column names ####

albery %>% rename(P.Alb = Count) -> albery
becker %>% select(X1, Prediction) %>% rename(Sp = X1, P.Bec = Prediction) -> becker
carlson %>% select(host_species, pred) %>% rename(Sp = host_species, P.Car = pred) -> carlson
dallas %>% select(host, suitability) %>% rename(Sp = host, P.Dal = suitability) -> dallas
farrell %>% select(Host, p.interaction) %>% rename(Sp = Host, P.Far = p.interaction) -> farrell
guth %>% select(host_species, pred_med) %>% rename(Sp = host_species, P.Gut = pred_med) -> guth
poisot1 %>% rename(Sp = X1, P.Po1 = X2) -> poisot1
poisot2 %>% rename(Sp = X1, P.Po2 = X2) -> poisot2

# Mutating ####

albery %>% mutate(R.Alb = rank(P.Alb)) %>% 
  mutate(R.Alb = (max(R.Alb) - R.Alb + 1)) -> albery

becker %>% mutate(R.Bec = rank(P.Bec)) %>% 
  mutate(R.Bec = (max(R.Bec) - R.Bec + 1)) -> becker

carlson %>% mutate(R.Car = rank(P.Car)) %>% 
  mutate(R.Car = (max(R.Car) - R.Car + 1)) -> carlson

dallas %>% mutate(R.Dal = rank(P.Dal)) %>% 
  mutate(R.Dal = (max(R.Dal) - R.Dal + 1)) -> dallas

farrell %>% filter(!is.na(P.Far)) %>% 
  mutate(R.Far = rank(P.Far)) %>% 
  mutate(R.Far = (max(R.Far) - R.Far + 1)) -> farrell

guth %>% mutate(R.Gut = rank(P.Gut)) %>% 
  mutate(R.Gut = (max(R.Gut) - R.Gut + 1)) -> guth

poisot1 %>% mutate(R.Po1 = rank(P.Po1)) %>% 
  mutate(R.Po1 = (max(R.Po1) - R.Po1 + 1)) -> poisot1

poisot2 %>% mutate(R.Po2 = rank(P.Po2)) %>% 
  mutate(R.Po2 = (max(R.Po2) - R.Po2 + 1)) -> poisot2

list(albery, becker, carlson, dallas, farrell, guth, poisot1, poisot2) %>%
  reduce(full_join) %>% 
  mutate_at("Sp", ~.x %>% 
              str_trim %>% 
              str_replace("_", " ")) %>% 
  select(Sp, starts_with("R.")) ->
  
  Models

read_csv(paste0(GithubDir, "PhylofactorPredictions.csv")) %>% select(X1, betacov) %>%
  
  rename(Sp = X1, Betacov = betacov) %>% mutate(Sp = gsub("_"," ",Sp)) -> 
  
  truth

truth %>% 
  left_join(Models) -> Models

Models %<>% 
  mutate(Rank = rowMeans(select(Models, starts_with("R.")), na.rm = TRUE))

Models %>% select(starts_with("R.")) %>% names -> RankNames

Models %<>% 
  mutate_at(RankNames, ~.x/max(.x, na.rm = T)) %>%  
  mutate(PropRank = rowMeans(select(Models, starts_with("R.")) %>% 
                               mutate_at(RankNames, ~.x/max(.x, na.rm = T)), 
                             na.rm = TRUE)) %>% 
  arrange(PropRank)

Models %>% 
  select(Sp, Betacov, starts_with("R."), Rank, PropRank) %>%
  as.data.frame -> 
  
  BatModels

# ~~~~~ NonBats ####

GithubDir <- "Github/MammalCSVs/"

albery <- read_csv(paste0(GithubDir, "AlberyNonBats.csv"))[,-1]
dallas1 <- (read_csv(paste0(GithubDir, "DallasMammalsCitations.csv"))[,-1] %>% 
              mutate(host = gsub(" ","_",host)))
dallas2 <- (read_csv(paste0(GithubDir, "DallasMammalsUncorrected.csv"))[,-1] %>% 
              mutate(host = gsub(" ","_",host)))
farrell1 <- read_csv(paste0(GithubDir, "FarrellMammalsFull.csv"))
farrell2 <- read_csv(paste0(GithubDir, "FarrellMammalsPhylogeny.csv"))
poisot1 <- (read_csv(paste0(GithubDir, "PoisotKnn1Mammal.csv"), col_names = FALSE) %>% 
              mutate(X1 = gsub(" ","_",X1)))
poisot2 <- (read_csv(paste0(GithubDir, "PoisotKnn2Mammal.csv"), col_names = FALSE) %>% 
              mutate(X1 = gsub(" ","_",X1)))
poisot3 <- (read_csv(paste0(GithubDir, "PoisotLfMammal.csv"), col_names = FALSE) %>% 
              mutate(X1 = gsub(" ","_",X1)))

# Triaging column names ####

albery %>% rename(P.Alb = Count) -> albery
dallas1 %>% select(host, suitability) %>% rename(Sp = host, P.Dal1 = suitability) -> dallas1
dallas2 %>% select(host, suitability) %>% rename(Sp = host, P.Dal2 = suitability) -> dallas2
farrell1 %>% select(Host, p.interaction) %>% rename(Sp = Host, P.Far1 = p.interaction) -> farrell1
farrell2 %>% select(Host, p.interaction) %>% rename(Sp = Host, P.Far2 = p.interaction) -> farrell2
poisot1 %>% rename(Sp = X1, P.Po1 = X2) -> poisot1
poisot2 %>% rename(Sp = X1, P.Po2 = X2) -> poisot2
poisot3 %>% rename(Sp = X1, P.Po3 = X2) -> poisot3

# Mutating ####

albery %>% mutate(R.Alb = rank(P.Alb)) %>% 
  mutate(R.Alb = (max(R.Alb) - R.Alb + 1)) -> albery

dallas1 %>% mutate(R.Dal1 = rank(P.Dal1)) %>% 
  mutate(R.Dal1 = (max(R.Dal1) - R.Dal1 + 1)) -> dallas1

dallas2 %>% mutate(R.Dal2 = rank(P.Dal2)) %>% 
  mutate(R.Dal2 = (max(R.Dal2) - R.Dal2 + 1)) -> dallas2

farrell1 %>% filter(!is.na(P.Far1)) %>% 
  mutate(R.Far1 = rank(P.Far1)) %>% 
  mutate(R.Far1 = (max(R.Far1) - R.Far1 + 1)) -> farrell1

farrell2 %>% filter(!is.na(P.Far2)) %>% 
  mutate(R.Far2 = rank(P.Far2)) %>% 
  mutate(R.Far2 = (max(R.Far2) - R.Far2 + 1)) -> farrell2

poisot1 %>% mutate(R.Po1 = rank(P.Po1)) %>% 
  mutate(R.Po1 = (max(R.Po1) - R.Po1 + 1)) -> poisot1

poisot2 %>% mutate(R.Po2 = rank(P.Po2)) %>% 
  mutate(R.Po2 = (max(R.Po2) - R.Po2 + 1)) -> poisot2

poisot3 %>% mutate(R.Po3 = rank(P.Po3)) %>% 
  mutate(R.Po3 = (max(R.Po3) - R.Po3 + 1)) -> poisot3

list(albery, dallas1, dallas2, farrell1, farrell2, poisot1, poisot2, poisot3) %>%
  reduce(full_join) %>% 
  mutate_at("Sp", ~.x %>% 
              str_trim %>% 
              str_replace("_", " ")) %>% 
  select(Sp, starts_with("R.")) ->
  
  Models

read_csv(paste0(GithubDir, "DallasMammalsUncorrected.csv")) %>% select(host, presence) %>%
  
  rename(Sp = host, Betacov = presence) %>% mutate(Sp = gsub("_"," ",Sp)) -> 
  
  truth

Models %>% 
  left_join(truth) %>% 
  mutate(Betacov = replace_na(Betacov, 0)) -> Models

Models %<>% 
  mutate(Rank = rowMeans(select(Models, starts_with("R.")), na.rm = TRUE))

Models %>% select(starts_with("R.")) %>% names -> RankNames

Models %<>% 
  mutate_at(RankNames, ~.x/max(.x, na.rm = T)) %>%  
  mutate(PropRank = rowMeans(select(Models, starts_with("R.")) %>% 
                               mutate_at(RankNames, ~.x/max(.x, na.rm = T)), 
                             na.rm = TRUE)) %>% 
  arrange(PropRank)

Models %>% 
  select(Sp, Betacov, starts_with("R."), Rank, PropRank) %>%
  as.data.frame -> 
  
  NonBatModels

# ~~~~~ NonBats ####

GithubDir <- "Github/NonBatCSVs/"

albery <- read_csv(paste0(GithubDir, "AlberyNonBats.csv"))
becker <- read_csv(paste0(GithubDir, "PhylofactorPredictions.csv"))
carlson <- read_csv(paste0(GithubDir, "batcov-bart.csv"))
dallas <- read_csv(paste0(GithubDir, "DallasPredictions.csv"))
farrell <- read_csv(paste0(GithubDir, "FarrellPredicted.csv"))
guth <- read_csv(paste0(GithubDir, "GuthPredictions.csv"))
poisot1 <- read_csv(paste0(GithubDir, "PoisotTanimotoChiropteraToChiropteraPredictions.csv"), col_names = FALSE)
poisot2 <- read_csv(paste0(GithubDir, "PoisotLinearFilterChiropteraToChiropteraPredictions.csv"), col_names = FALSE)

# Triaging column names ####

albery %>% rename(P.Alb = Count) -> albery
becker %>% select(X1, Prediction) %>% rename(Sp = X1, P.Bec = Prediction) -> becker
carlson %>% select(host_species, pred) %>% rename(Sp = host_species, P.Car = pred) -> carlson
dallas %>% select(host, suitability) %>% rename(Sp = host, P.Dal = suitability) -> dallas
farrell %>% select(Host, p.interaction) %>% rename(Sp = Host, P.Far = p.interaction) -> farrell
guth %>% select(host_species, pred_med) %>% rename(Sp = host_species, P.Gut = pred_med) -> guth
poisot1 %>% rename(Sp = X1, P.Po1 = X2) -> poisot1
poisot2 %>% rename(Sp = X1, P.Po2 = X2) -> poisot2

# Mutating ####

albery %>% mutate(R.Alb = rank(P.Alb)) %>% 
  mutate(R.Alb = (max(R.Alb) - R.Alb + 1)) -> albery

becker %>% mutate(R.Bec = rank(P.Bec)) %>% 
  mutate(R.Bec = (max(R.Bec) - R.Bec + 1)) -> becker

carlson %>% mutate(R.Car = rank(P.Car)) %>% 
  mutate(R.Car = (max(R.Car) - R.Car + 1)) -> carlson

dallas %>% mutate(R.Dal = rank(P.Dal)) %>% 
  mutate(R.Dal = (max(R.Dal) - R.Dal + 1)) -> dallas

farrell %>% filter(!is.na(P.Far)) %>% 
  mutate(R.Far = rank(P.Far)) %>% 
  mutate(R.Far = (max(R.Far) - R.Far + 1)) -> farrell

guth %>% mutate(R.Gut = rank(P.Gut)) %>% 
  mutate(R.Gut = (max(R.Gut) - R.Gut + 1)) -> guth

poisot1 %>% mutate(R.Po1 = rank(P.Po1)) %>% 
  mutate(R.Po1 = (max(R.Po1) - R.Po1 + 1)) -> poisot1

poisot2 %>% mutate(R.Po2 = rank(P.Po2)) %>% 
  mutate(R.Po2 = (max(R.Po2) - R.Po2 + 1)) -> poisot2

list(albery, becker, carlson, dallas, farrell, guth, poisot1, poisot2) %>%
  reduce(full_join) %>% 
  mutate_at("Sp", ~.x %>% 
              str_trim %>% 
              str_replace("_", " ")) %>% 
  select(Sp, starts_with("R.")) ->
  
  Models

read_csv(paste0(GithubDir, "DallasMammalsUncorrected.csv")) %>% select(host, presence) %>%
  
  rename(Sp = host, Betacov = presence) %>% mutate(Sp = gsub("_"," ",Sp)) -> 
  
  truth

truth %>% 
  left_join(Models) -> Models


Models %<>% 
  mutate(Rank = rowMeans(select(Models, starts_with("R.")), na.rm = TRUE))

Models %>% select(starts_with("R.")) %>% names -> RankNames

Models %<>% 
  mutate_at(RankNames, ~.x/max(.x, na.rm = T)) %>%  
  mutate(PropRank = rowMeans(select(Models, starts_with("R.")) %>% 
                               mutate_at(RankNames, ~.x/max(.x, na.rm = T)), 
                             na.rm = TRUE)) %>% 
  arrange(PropRank)

Models %>% 
  select(Sp, Betacov, starts_with("R."), Rank, PropRank) %>%
  as.data.frame -> 
  
  NonBatModels