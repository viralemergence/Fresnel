
# Thresholding Model Predictions ####

library(PresenceAbsence); library(tidyverse)

RNames <- c('R.Alb','R.Car3','R.Dal1','R.Far1','R.Gut1','R.Po2','R.Po3','R.Stock1')

RNames %>% str_replace_all("^R.", "P.") -> PNames

thresh <- c('T.Alb','T.Car','T.Dal','T.Far','T.Gut','T.Po1','T.Po2','T.Stock1')

negatory <- function(x) {1-x}

BatModels %>% mutate(n = 1:nrow(BatModels)) %>%
  mutate_at(RNames, negatory) %>%
  mutate_at('PropRank', negatory) -> BatModels2

####### GET AUC'S

for (i in 1:8) {
  print(c(PNames)[i])
  print(auc(data.frame(BatModels2[,c('n','Betacov',PNames[i])]), na.rm = TRUE))
}
auc(data.frame(BatModels2[,c('n','Betacov',"PropRank")]), na.rm = TRUE)

####### 

tvalues <- optimal.thresholds(data.frame(BatModels2[,c('n','Betacov',RNames,PNames,'PropRank')]),
                              threshold = 10001,
                              opt.methods = 10,
                              req.sens = 0.9,
                              na.rm = TRUE)

for (name in RNames) {BatModels2[,name] <- as.vector(BatModels2[,name] > tvalues[1,name])}
for (name in PNames) {BatModels2[,name] <- as.vector(BatModels2[,name] > tvalues[1,name])}

colSums(BatModels2[BatModels2$Betacov==0,RNames], na.rm = TRUE)
colSums(BatModels2[BatModels2$Betacov==0,PNames], na.rm = TRUE)

# TOTAL RANK

BatModels2[,'PropRank'] <- as.vector(BatModels2[,'PropRank'] > tvalues[1,'PropRank'])

table(BatModels2[BatModels2$Betacov==0,'PropRank'])

# Rhinolophus

BatModels2[grep('Rhinolophus',BatModels2$Sp),] %>% View()

BatModels2[grep('Rhinolophus',BatModels2$Sp),] %>%
  filter(Betacov==0) %>% select(PropRank) %>% table()

# Clean it up to write out

BatModels2 %>% select(Sp, Betacov, P.Alb, P.Car3, P.Dal1, P.Far1, P.Gut1, P.Po2, P.Po3, P.Stock1, PropRank) %>%
  rename(Trait.1 = P.Gut1,
         Trait.2 = P.Car3,
         Trait.3 = P.Alb,
         Network.1 = P.Po2,
         Network.2 = P.Po3,
         Network.3 = P.Dal1,
         Network.4 = P.Far1,
         Hybrid.1 = P.Stock1,
         Ensemble = PropRank) -> BatModels2 

BatModels2 %>% write_csv("BinaryPredictions.csv")

# WHO WON! WHO'S NEXT!

verify <- c('Scotophilus kuhlii', 'Scotophilus heathii', 'Hipposideros larvatus', 'Hipposideros pomona', 'Pteropus lylei', 'Myotis pequinius', 'Myotis horsfieldii')

BatModels2 %>% filter(Sp %in% verify)

#################################

key1 <- c(`1` = "Reported", `0` = "Unreported")

key2 <- c(`0` = "Unlikely", `1` = "Suspected",
          `2` = "False -", `3` = "True +")

BatModels2 %>% mutate_at(vars(contains(".")), ~(as.numeric(.))) %>%
  mutate(Ensemble = as.numeric(Ensemble)) %>%
  mutate(Betacov2 = Betacov*2) %>%
  mutate_at(vars(contains(".")), ~(. + Betacov2)) %>% 
  mutate(Ensemble = Ensemble + Betacov2) %>% 
  mutate_at(vars(contains(".")), ~(recode(.,!!!key2))) %>% 
  mutate(Ensemble =  recode(Ensemble,!!!key2)) %>% 
  mutate(Betacov = recode(Betacov, !!!key1)) %>% 
  mutate(`New data` = Betacov) %>% 
  rename(`Training data` = Betacov) -> BatWeb

BatWeb$`New data`[which(BatWeb$Sp %in% verify)] <- 'New data'

BatWeb %>% select(c(`Training data`, 
                    `New data`,
                    Ensemble,
                    Trait.1,
                    Trait.2,
                    Trait.3,
                    Hybrid.1,
                    Network.1,
                    Network.2,
                    Network.3,
                    Network.4)) %>% rename(Hybrid = Hybrid.1) -> BatWeb

BatWeb %>% write_csv("BinaryWebsite.csv")
