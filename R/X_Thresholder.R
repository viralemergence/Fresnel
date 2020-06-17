
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

for (name in RNames) {BatModels2[,name] <- (BatModels2[,name] > tvalues[1,name])}
for (name in PNames) {BatModels2[,name] <- (BatModels2[,name] > tvalues[1,name])}

colSums(BatModels2[BatModels2$Betacov==0,RNames], na.rm = TRUE)
colSums(BatModels2[BatModels2$Betacov==0,PNames], na.rm = TRUE)

# TOTAL RANK

BatModels2[,'PropRank'] <- (BatModels2[,'PropRank'] > tvalues[1,'PropRank'])

table(BatModels2[BatModels2$Betacov==0,'PropRank'])

# Rhinolophus

BatModels2[grep('Rhinolophus',BatModels2$Sp),] %>% View()

BatModels2[grep('Rhinolophus',BatModels2$Sp),] %>%
  filter(Betacov==0) %>% select(PropRank) %>% table()

# Clean it up to write out

BatModels2 %>% select(Sp, Betacov, P.Alb, P.Car3, P.Dal1, P.Far1, P.Gut1, P.Po2, P.Po3, PropRank) %>%
  rename(Trait.3 = P.Alb,
         Trait.2 = P.Car3,
         Network.3 = P.Dal1,
         Network.4 = P.Far1,
         Trait.1 = P.Gut1,
         Network.1 = P.Po2,
         Network.2 = P.Po3,
         Ensemble = PropRank) %>% write_csv("BinaryPredictions.csv")

# WHO WON! WHO'S NEXT!

verify <- c('Scotophilus kuhlii', 'Scotophilus heathii', 'Hipposideros larvatus', 'Hipposideros pomona', 'Pteropus lylei', 'Myotis pequinius', 'Myotis horsfieldii')

BatModels2 %>% select(Sp, Betacov, P.Alb, P.Car3, P.Dal1, P.Far1, P.Gut1, P.Po2, P.Po3, PropRank) %>%
  filter(Sp %in% verify)
