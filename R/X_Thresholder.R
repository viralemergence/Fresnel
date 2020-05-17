
library(PresenceAbsence)

RNames <- c('R.Alb','R.Car3','R.Dal1','R.Far1','R.Gut1','R.Po2','R.Po3')

RNames %>% str_replace_all("^R.", "P.") -> PNames

thresh <- c('T.Alb','T.Car','T.Dal','T.Far','T.Gut','T.Po1','T.Po2')

i <- 1

for (i in 1:length(RNames)) {
  
  print(RNames[i])
  
  BatModels %>% select(Betacov, RNames[i]) %>% mutate(n = 1:nrow(BatModels)) %>%
    mutate(!!RNames[i] := (1-BatModels[,RNames[i]])) %>%
    select(n, Betacov, RNames[i]) -> 
    
    training
  
  t <- optimal.thresholds(data.frame(training),
                          threshold = 10001,
                          opt.methods = 10,
                          req.sens = 0.9,
                          na.rm = TRUE)[1,2]
  
  BatModels %>% mutate(!!thresh[i] := (BatModels[,RNames[i]] > t)) -> BatModels
  
  BatModels %>% select(Betacov, PNames[i]) %>% mutate(n = 1:nrow(BatModels)) %>%
    #mutate(!!PNames[i] := (1-BatModels[,PNames[i]])) %>%
    select(n, Betacov, PNames[i]) -> 
    
    training
  
  t <- optimal.thresholds(data.frame(training),
                          threshold = 10001,
                          opt.methods = 10,
                          req.sens = 0.9,
                          na.rm = TRUE)[1,2]
  
  BatModels %>% mutate(!!(paste0(thresh[i], ".P")) := (BatModels[,PNames[i]] > t)) -> 
    
    BatModels
  
}

BatModels %>% 
  summarise_at(vars(starts_with("T.")), 
               ~paste0(table(.x), collapse = "; "))


for(i in 1:length(names)) {
  b <- BatModels[BatModels$Betacov==0,]
  print(thresh[i])
  print(table(b[,thresh[i]]))
}
