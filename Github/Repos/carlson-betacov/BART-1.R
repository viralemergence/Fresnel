
# Colin Code ####

#setwd('~/Github/carlson-betacov')

set.seed(05082020)

library(BART)
library(fastDummies)
library(tidyverse)
library(tidyverse)
library(dplyr)
detach(package:plyr)
library(PresenceAbsence)

varimp.pbart <- function(model, plot=TRUE) {
  
  names <- colnames(model$varcount)
  varimps <- colMeans(model$varcount/rowSums(model$varcount))
  
  var.df <- data.frame(names, varimps)
  
  var.df$names <- factor(var.df$names)
  var.df <- transform(var.df, names = reorder(names,
                                              -varimps))
  
  if(plot==TRUE){
    #g1 <- ggplot2::ggplot(var.df, aes(y=varimps, x=names)) +
    #  geom_bar(stat="identity", color="black") +
    #  theme(axis.text.x = element_text(angle = 45)) + 
    #  ylab("Relative importance") + theme_bluewhite()
    #print(g1)
    
    rel <- model$varcount/rowSums(model$varcount)
    colnames(rel) <- names
    
    rel %>% data.frame() %>% gather() %>%
      group_by(key) %>%
      summarise(mean = mean(value),
                sd = sd(value, na.rm = TRUE)) %>% 
      transform(Var = reorder(key, mean)) %>%
      ggplot(aes(x = Var, y = mean)) +
      geom_pointrange(aes(y = mean, x = Var, ymin = mean-sd, ymax = mean+sd),
                      color="#00AFDD") + 
      xlab(NULL) + ylab("Variable importance") + coord_flip() + 
      theme_bw() + theme(legend.position = "none",
                         axis.title.x = element_text(size=rel(1.3), vjust = -0.8),
                         axis.text.y = element_text(size=rel(1.4)),
                         plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
                         panel.grid.minor = element_blank(),
                         panel.grid.major.x = element_line(color='grey',
                                                           linetype='dashed')) -> p
    
    print(p)
    
  }
  
  return(var.df)
  
}

read_csv(paste0(here::here(), 
                '/GitHub/Repos/virionette/03_interaction_data/virionette.csv'))-> 
  
  batcov

batcov %>% 
  dplyr::filter(host_order == 'Chiroptera')

read_csv(paste0(here::here(), '/GitHub/Repos/virionette/04_predictors/Han-BatTraits.csv')) -> 
  
  traits

# Add outcome variables 

batcov %>% mutate(betacov = as.numeric(virus_genus == 'Betacoronavirus')) -> 
  
  batcov

batcov %>% dplyr::select(host_species, betacov) %>% unique -> batcov
batcov %>% group_by(host_species) %>% summarize(betacov = max(betacov)) -> batcov

# Create binomial names in the trait data

traits %>% mutate(host_species = paste(MSW05_Genus, MSW05_Species)) %>% 
  mutate() -> traits

# Add traits and associations

right_join(batcov, traits) %>% 
  mutate(betacov = replace_na(betacov, 0)) -> batdf 

# Turn categorical variable into columns

batdf %>% 
  dummy_cols('ForStrat.Value') %>% 
  dplyr::select(-ForStrat.Value) -> batdf

# Remove Sarah's drops 

batdf %>% dplyr::select(-BodyMass.Value) %>%
  dplyr::select(-X30.2_PET_Mean_mm) %>%
  dplyr::select(-X27.3_HuPopDen_5p_n.km2) %>%
  dplyr::select(-X27.1_HuPopDen_Min_n.km2) -> batdf

# Full model fails because there are too many NA's somewhere
# Drop any variable with > 50% NA's

varnums <- c(7:69)

varkeep <- 6 + unname(which(c(colSums(is.na(batdf[,7:69]))/nrow(batdf)) < 0.5))

batdf %>% data.frame -> batdf

# Add citations

read_csv(paste0(here::here(), '/GitHub/Repos/virionette/04_predictors/Citations.csv'))[,-1] %>%
  rename(host_species = name) -> cites

batdf <- left_join(batdf, cites)

# Fill in missing citations
#counter <- function(name) {as.numeric(as.character(easyPubMed::get_pubmed_ids(name)$Count))}
#for(i in 1:nrow(batdf)){
#  if(is.na(batdf$cites[i])){
#    batdf$cites[i] <- counter(batdf$host_species[i])
#  }
#  print(i)
#}

varwcite <- c(varkeep, which(colnames(batdf)=='cites'))

batdf.master <- batdf

brtvar <- c('X5.1_AdultBodyMass_g',
            'X8.1_AdultForearmLen_mm',
            'X26.1_GR_Area_km2',
            'X26.2_GR_MaxLat_dd',
            'X26.3_GR_MinLat_dd',
            'X26.4_GR_MidRangeLat_dd',
            'X26.5_GR_MaxLong_dd',
            'X26.6_GR_MinLong_dd',
            'X26.7_GR_MidRangeLong_dd',
            'X27.2_HuPopDen_Mean_n.km2',
            'X27.4_HuPopDen_Change',
            'X28.1_Precip_Mean_mm',
            'X28.2_Temp_Mean_01degC',
            'X30.1_AET_Mean_mm',
            'Diet.Inv',
            'Diet.Fruit',
            'Diet.Nect',
            'Activity.Crepuscular',
            'ForStrat.Value_A',
            'ForStrat.Value_Ar',
            'ForStrat.Value_G',
            'ForStrat.Value_S')

brtcite <- c(brtvar,'cites')

##########################################################################

# FOUR MODELS
# 1A - Baseline BART, no citations
# 1B - Baseline BART, with citations
# 2A - DART, no citations
# 2B - DART, with citations

##########################################################################

batdf <- batdf.master

model1a <- pbart(x.train = batdf[,brtvar], #varkeep
                 y.train = batdf[,'betacov'],
                 sparse = FALSE,
                 ntree = 200L,
                 ndpost = 10000L)

varimp.pbart(model1a)

batdf$pred1a <- colMeans(pnorm(model1a$yhat.train))

# A density plot in the style of Becker

batdf %>% 
  ggplot(aes(pred1a, 
             fill = factor(betacov), 
             colour = factor(betacov))) + 
  geom_density(alpha = 0.1) + 
  theme_classic()

# Top rankings 

batdf %>% 
  as_tibble() %>%
  filter(!(betacov == 1)) %>%
  dplyr::select(host_species, pred1a) %>%
  arrange(-pred1a)# %>% View()

# Get a 90% omission threshold

batdf %>% 
  as_tibble() %>%
  dplyr::select(host_species, betacov, pred1a) %>%
  data.frame() -> training

thresh <- optimal.thresholds(data.frame(training),
                             threshold = 10001,
                             opt.methods = 10,
                             req.sens = 0.9,
                             na.rm = TRUE)[1,2]

# How many new bats are above the threshold?

batdf %>% 
  as_tibble() %>%
  filter(!(betacov == 1)) %>%
  dplyr::select(host_species, pred1a) %>%
  arrange(-pred1a) %>% 
  filter(pred1a > thresh) -> not.df

nrow(not.df)

# How's the AUC look

auc.roc.plot(data.frame(training))

# File export

batdf %>% dplyr::select(host_species,
                 betacov,
                 pred1a) %>% 
  rename(pred = pred1a) %>% as_tibble() %>% 
  write.csv('CarlsonBartUncorrected.csv')

##########################################################################

batdf <- batdf.master

model1b <- pbart(x.train = batdf[,brtcite], #varwcite
                 y.train = batdf[,'betacov'],
                 sparse = FALSE,
                 ntree = 200L,
                 ndpost = 10000L)

varimp.pbart(model1b)

batdf$cites <- mean(na.omit(batdf$cites))

batdf$pred1b <- pnorm(colMeans(predict(model1b, batdf[,colnames(model1b$varcount)])$yhat.test))

# A density plot in the style of Becker

batdf %>% 
  ggplot(aes(pred1b, 
             fill = factor(betacov), 
             colour = factor(betacov))) + 
  geom_density(alpha = 0.1) + 
  theme_classic()

# Top rankings 

batdf %>% 
  as_tibble() %>%
  filter(!(betacov == 1)) %>%
  dplyr::select(host_species, pred1b) %>%
  arrange(-pred1b) %>% View()

# Get a 90% omission threshold

batdf %>% 
  as_tibble() %>%
  dplyr::select(host_species, betacov, pred1b) %>%
  data.frame() -> training

library(PresenceAbsence)

thresh <- optimal.thresholds(data.frame(training),
                             threshold = 10001,
                             opt.methods = 10,
                             req.sens = 0.9,
                             na.rm = TRUE)[1,2]

# How many new bats are above the threshold?

batdf %>% 
  as_tibble() %>%
  filter(!betacov) %>%
  dplyr::select(host_species, pred1b) %>%
  arrange(-pred1b) %>% 
  filter(pred1b > thresh) -> not.df

nrow(not.df)

# How's the AUC look

auc.roc.plot(data.frame(training))

# File export

batdf %>% dplyr::select(host_species,
                 betacov,
                 pred1b) %>% 
  rename(pred = pred1b) %>% as_tibble() %>% 
  write.csv('CarlsonBartCitations.csv')

##########################################################################

batdf <- batdf.master

model2a <- pbart(x.train = batdf[,brtvar], #varkeep
                 y.train = batdf[,'betacov'],
                 sparse = TRUE,
                 a = 0.75,
                 ntree = 200L,
                 ndpost = 10000L)

varimp.pbart(model2a)

batdf$pred2a <- colMeans(pnorm(model2a$yhat.train))

# A density plot in the style of Becker

batdf %>% 
  ggplot(aes(pred2a, 
             fill = factor(betacov), 
             colour = factor(betacov))) + 
  geom_density(alpha = 0.1) + 
  theme_classic()

# Top rankings 

batdf %>% 
  as_tibble() %>%
  filter(!(betacov == 1)) %>%
  dplyr::select(host_species, pred2a) %>%
  arrange(-pred2a) %>% View()

# Get a 90% omission threshold

batdf %>% 
  as_tibble() %>%
  dplyr::select(host_species, betacov, pred2a) %>%
  data.frame() -> training

thresh <- optimal.thresholds(data.frame(training),
                             threshold = 10001,
                             opt.methods = 10,
                             req.sens = 0.9,
                             na.rm = TRUE)[1,2]

# How many new bats are above the threshold?

batdf %>% 
  as_tibble() %>%
  filter(!(betacov == 1)) %>%
  dplyr::select(host_species, pred2a) %>%
  arrange(-pred2a) %>% 
  filter(pred2a > thresh) -> not.df

nrow(not.df)

# How's the AUC look

auc.roc.plot(data.frame(training))

# File export

batdf %>% dplyr::select(host_species,
                 betacov,
                 pred2a) %>% 
  rename(pred = pred2a) %>% as_tibble() %>% 
  write.csv('CarlsonDartUncorrected.csv')

##########################################################################

batdf <- batdf.master

model2b <- pbart(x.train = batdf[,brtcite], #varwcite
                 y.train = batdf[,'betacov'],
                 sparse = TRUE,
                 ntree = 200L,
                 ndpost = 10000L)

varimp.pbart(model2b)

batdf$cites <- mean(na.omit(batdf$cites))

batdf$pred2b <- pnorm(colMeans(predict(model2b, batdf[,colnames(model2b$varcount)])$yhat.test))

# A density plot in the style of Becker

batdf %>% 
  ggplot(aes(pred2b, 
             fill = factor(betacov), 
             colour = factor(betacov))) + 
  geom_density(alpha = 0.1) + 
  theme_classic()

# Top rankings 

batdf %>% 
  as_tibble() %>%
  filter(!(betacov == 1)) %>%
  dplyr::select(host_species, pred2b) %>%
  arrange(-pred2b) %>% View()

# Get a 90% omission threshold

batdf %>% 
  as_tibble() %>%
  dplyr::select(host_species, betacov, pred2b) %>%
  data.frame() -> training

thresh <- optimal.thresholds(data.frame(training),
                             threshold = 10001,
                             opt.methods = 10,
                             req.sens = 0.9,
                             na.rm = TRUE)[1,2]

# How many new bats are above the threshold?

batdf %>% 
  as_tibble() %>%
  filter(!(betacov == 1)) %>%
  dplyr::select(host_species, pred2b) %>%
  arrange(-pred2b) %>% 
  filter(pred2b > thresh) -> not.df

nrow(not.df)

# How's the AUC look

auc.roc.plot(data.frame(training))

# File export

batdf %>% dplyr::select(host_species,
                 betacov,
                 pred2b) %>% 
  rename(pred = pred2b) %>% as_tibble() %>% 
  write.csv('CarlsonDartCitations.csv')

