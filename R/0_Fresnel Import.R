
# Importing predictions and creating ranked predictions ####

library(tidyverse); library(fs)

user <- 'Colin'

Repos <- c(
  "albery-betacov",
  "becker-betacov",
  "carlson-batcov",
  "dallas-betacov",
  "farrell-betacov",
  "guth-betacov",
  "poisot-betacov")

if(user=='Colin') {
  GithubDir <- "~/Github/Fresnel/Github/CSVs/"
} else if (user=='Greg') {
  GithubDir <- "Github/CSVs/"
}

albery <- read_csv(paste0(GithubDir, "AlberyPredicted.csv"))
becker <- read_csv(paste0(GithubDir, "PhylofactorPredictions.csv"))
carlson <- read_csv(paste0(GithubDir, "batcov-bart.csv"))
dallas <- read_csv(paste0(GithubDir, "DallasPredictions.csv"))
farrell <- read_csv(paste0(GithubDir, "batcov_elmasri_full_pred_betacovsOnly.csv"))
guth <- read_csv(paste0(GithubDir, "GuthPredictions.csv"))
poisot1 <- read_csv(paste0(GithubDir, "PoisotTanimotoChiropteraToChiropteraPredictions.csv"), col_names = FALSE)
poisot2 <- read_csv(paste0(GithubDir, "PoisotLinearFilterChiropteraToChiropteraPredictions.csv"), col_names = FALSE)

list(albery, becker, carlson, dallas, farrell, guth, poisot1, poisot2) -> 
  
  ModelList

albery %>% select(-X1) %>% rename(P.Alb = Count) -> albery
becker %>% select(X1, Prediction) %>% rename(Sp = X1, P.Bec = Prediction) -> becker
carlson %>% select(host_species, pred) %>% rename(Sp = host_species, P.Car = pred) -> carlson
dallas %>% select(host, suitability) %>% rename(Sp = host, P.Dal = suitability) -> dallas
farrell %>% select(Host, rank) %>% rename(Sp = Host, R.Far = rank) -> farrell
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

# No Farrell as he returned ranks
guth %>% mutate(R.Gut = rank(P.Gut)) %>% mutate(R.Gut = (max(R.Gut) - R.Gut + 1)) -> guth
poisot1 %>% mutate(R.Po1 = rank(P.Po1)) %>% mutate(R.Po1 = (max(R.Po1) - R.Po1 + 1)) -> poisot1
poisot2 %>% mutate(R.Po2 = rank(P.Po2)) %>% mutate(R.Po2 = (max(R.Po2) - R.Po2 + 1)) -> poisot2

albery %>% mutate(Sp = gsub("_"," ",Sp)) -> albery
becker %>% mutate(Sp = gsub("_"," ",Sp)) -> becker
carlson %>% mutate(Sp = gsub("_"," ",Sp)) -> carlson
dallas %>% mutate(Sp = gsub("_"," ",Sp)) -> dallas
farrell %>% mutate(Sp = gsub("_"," ",Sp)) -> farrell
guth %>% mutate(Sp = gsub("_"," ",Sp)) -> guth
poisot1 %>% mutate(Sp = gsub("_"," ",Sp)) -> poisot1
poisot2 %>% mutate(Sp = gsub("_"," ",Sp)) -> poisot2

full_join(albery, becker) %>% full_join(carlson) %>% full_join(dallas) %>% 
  full_join(farrell) %>% full_join(guth) %>% full_join(poisot1) %>% full_join(poisot2) -> Models

Models %>% select(Sp, R.Alb, R.Bec, R.Car, R.Dal, R.Far, R.Gut, R.Po1, R.Po2)  -> Models


if(user=='Colin') {
  read_csv("~/Github/becker-betacov/PhylofactorPredictions.csv") %>% select(X1, betacov) %>%
    rename(Sp = X1, Betacov = betacov) %>% mutate(Sp = gsub("_"," ",Sp)) -> truth
} else if (user=='Greg') {
  read_csv(paste0(GithubDir, "/becker-betacov/PhylofactorPredictions.csv")) %>% select(X1, betacov) %>%
    rename(Sp = X1, Betacov = betacov) %>% mutate(Sp = gsub("_"," ",Sp)) -> truth
}


left_join(truth, Models) -> Models

Models %>%  mutate(Rank = rowMeans(select(Models, starts_with("R.")), na.rm = TRUE)) -> Models

Models %>% arrange(Rank)

Models %>% select(starts_with("R.")) %>% names -> RankNames

Models %>% 
  #mutate_at(RankNames, ~.x/max(.x, na.rm = T)) %>%  
  mutate_at(RankNames, ~.x/max(.x, na.rm = T)) %>%  
  mutate(PropRank = rowMeans(select(Models, starts_with("R.")) %>% 
                               mutate_at(RankNames, ~.x/max(.x, na.rm = T)), 
                             na.rm = TRUE)) %>% 
  arrange(PropRank) -> 
  
  Models

