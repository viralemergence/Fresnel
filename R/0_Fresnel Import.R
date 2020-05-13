
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

GithubDir <- "Github/CSVs/"

albery <- read_csv(paste0(GithubDir, "AlberyPredicted.csv"))
becker <- read_csv(paste0(GithubDir, "PhylofactorPredictions.csv"))
carlson <- read_csv(paste0(GithubDir, "batcov-bart.csv"))
dallas <- read_csv(paste0(GithubDir, "DallasPredictions.csv"))
farrell <- read_csv(paste0(GithubDir, "FarrellPredicted.csv"))
guth <- read_csv(paste0(GithubDir, "GuthPredictions.csv"))
poisot1 <- read_csv(paste0(GithubDir, "PoisotTanimotoChiropteraToChiropteraPredictions.csv"), col_names = FALSE)
poisot2 <- read_csv(paste0(GithubDir, "PoisotLinearFilterChiropteraToChiropteraPredictions.csv"), col_names = FALSE)

albery %>% rename(P.Alb = Count) -> albery
becker %>% select(X1, Prediction) %>% rename(Sp = X1, P.Bec = Prediction) -> becker
carlson %>% select(host_species, pred) %>% rename(Sp = host_species, P.Car = pred) -> carlson
dallas %>% select(host, suitability) %>% rename(Sp = host, P.Dal = suitability) -> dallas
farrell %>% select(Host, p.interaction) %>% rename(Sp = Host, P.Far = p.interaction) -> farrell
guth %>% select(host_species, pred_med) %>% rename(Sp = host_species, P.Gut = pred_med) -> guth
poisot1 %>% rename(Sp = X1, P.Po1 = X2) -> poisot1
poisot2 %>% rename(Sp = X1, P.Po2 = X2) -> poisot2

albery %>% mutate(R.Alb = rank(P.Alb)) %>% mutate(R.Alb = (max(R.Alb) - R.Alb + 1)) -> albery
becker %>% mutate(R.Bec = rank(P.Bec)) %>% mutate(R.Bec = (max(R.Bec) - R.Bec + 1)) -> becker
carlson %>% mutate(R.Car = rank(P.Car)) %>% mutate(R.Car = (max(R.Car) - R.Car + 1)) -> carlson

dallas %>% 
  mutate(R.Dal = rank(P.Dal)) %>% 
  mutate(R.Dal = (max(R.Dal) - R.Dal + 1)) -> 
  
  dallas

farrell %>% 
  filter(!is.na(P.Far)) %>%
  mutate(R.Far = rank(P.Far)) %>% 
  mutate(R.Far = (max(R.Far) - R.Far + 1)) -> farrell

guth %>% mutate(R.Gut = rank(P.Gut)) %>% mutate(R.Gut = (max(R.Gut) - R.Gut + 1)) -> guth
poisot1 %>% mutate(R.Po1 = rank(P.Po1)) %>% mutate(R.Po1 = (max(R.Po1) - R.Po1 + 1)) -> poisot1
poisot2 %>% mutate(R.Po2 = rank(P.Po2)) %>% mutate(R.Po2 = (max(R.Po2) - R.Po2 + 1)) -> poisot2

list(albery, becker, carlson, dallas, farrell, guth, poisot1, poisot2) %>%
  reduce(full_join) %>% 
  mutate_at("Sp", ~.x %>% 
              str_trim %>% 
              str_replace("_", " ")) -> Models

Models %>% select(Sp, R.Alb, R.Bec, R.Car, R.Dal, R.Far, R.Gut, R.Po1, R.Po2)  -> Models

read_csv(paste0(GithubDir, "PhylofactorPredictions.csv")) %>% select(X1, betacov) %>%
  
  rename(Sp = X1, Betacov = betacov) %>% mutate(Sp = gsub("_"," ",Sp)) -> 
  
  truth

left_join(truth, Models) -> Models

# Models %>% dplyr::select(-R.Bec) -> Models

Models %>% mutate(Rank = rowMeans(select(Models, starts_with("R.")), na.rm = TRUE)) -> Models

Models %>% select(starts_with("R.")) %>% names -> RankNames

Models %>% 
  mutate_at(RankNames, ~.x/max(.x, na.rm = T)) %>%  
  mutate(PropRank = rowMeans(select(Models, starts_with("R.")) %>% 
                               mutate_at(RankNames, ~.x/max(.x, na.rm = T)), 
                             na.rm = TRUE)) %>% 
  arrange(PropRank) -> 
  
  Models

Models %>% filter(!Betacov)
Models %>% filter(!(!Betacov))

Models %>% 
  select(Sp, Betacov, starts_with("R."), Rank, PropRank) %>%
  as.data.frame -> 
  
  Models
