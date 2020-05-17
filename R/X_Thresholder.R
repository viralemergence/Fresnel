
library(PresenceAbsence)

names <- c('R.Alb','R.Car3','R.Dal1','R.Far1','R.Gut1','R.Po2','R.Po3')
thresh <- c('T.Alb','T.Car','T.Dal','T.Far','T.Gut','T.Po1','T.Po2')

for (i in 1:length(names)) {
  
BatModels %>% select(Betacov, names[i]) %>% mutate(n = 1:nrow(BatModels)) %>%
    mutate(!!names[i] := (1-BatModels[,names[i]])) %>%
    select(n, Betacov, names[i]) -> training
  
t <- optimal.thresholds(data.frame(training),
                             threshold = 10001,
                             opt.methods = 10,
                             req.sens = 0.9,
                             na.rm = TRUE)[1,2]

BatModels %>% mutate(!!thresh[i] := (BatModels[,names[i]] > t)) -> BatModels

}

for(i in 1:length(names)) {
  print(thresh[i])
  print(table(BatModels[BatModels$Betacov==0, thresh[i]]))
}
