
library(Bolstad2)

BatModels %>%
  rename(Trait.1 = R.Gut1,
         Trait.2 = R.Car3,
         Trait.3 = R.Alb,
         Network.1 = R.Po2,
         Network.2 = R.Po3,
         Network.3 = R.Dal1,
         Network.4 = R.Far1,
         Hybrid.1 = R.Stock1,
         Ensemble = PropRank) %>%
  select(Sp, Trait.1, Trait.2, Trait.3, Hybrid.1,
         Network.1, Network.2, Network.3, Network.4) -> BatModels

auc.df <- left_join(BatModels, BatWeb %>% select(Sp, `New data`))

models = c("Trait.1", "Trait.2", "Trait.3", "Hybrid.1",
           "Network.1", "Network.2", "Network.3", "Network.4")

l <- lapply(c(1:8), function(x) {

blankdf <- data.frame(Model = NA, Thresh = c(0:1000)/1000, PredPos = NA, Acc = NA)
for (i in 1:nrow(blankdf)) {
  blankdf$Model <- models[x]
  blankdf$PredPos[i] <- sum(na.omit(auc.df[,models[x]]) < blankdf$Thresh[i]) # Less than because it's rank
  blankdf$Acc[i] <- sum(na.omit(auc.df[auc.df$`New data`=="New data",models[x]]) < blankdf$Thresh[i])
  print(i)
}
blankdf$PredPos <- blankdf$PredPos/max(blankdf$PredPos)
blankdf$Acc <- blankdf$Acc/max(blankdf$Acc)

  return(blankdf)
  })

d <- bind_rows(l)

scores <- sapply(models, function(j) {
  sintegral(d %>% filter(Model == j) %>% pull(PredPos),
            d %>% filter(Model == j) %>% pull(Acc))$int })

scores <- data.frame(Model = models, Score = scores)
rownames(scores) <- NULL
scores %>% arrange(-Score) -> scores

scores

w1 <- scores$Score - min(scores$Score)

ensemble <- rowMeans(auc.df[,2:8], na.rm = TRUE) 
ensemble2 <- matrixStats::rowWeightedMeans(as.matrix(auc.df[,2:9]), 
                                           w = w1, 
                                           na.rm = TRUE)

auc.df$Ensemble.1 <- ensemble
auc.df$Ensemble.2 <- ensemble2

l2 <- lapply(c(1:2), function(x) {
  
  models = c("Ensemble.1", "Ensemble.2")
  
  blankdf <- data.frame(Model = NA, Thresh = c(0:1000)/1000, PredPos = NA, Acc = NA)
  for (i in 1:nrow(blankdf)) {
    blankdf$Model <- models[x]
    blankdf$PredPos[i] <- sum(na.omit(auc.df[,models[x]]) < blankdf$Thresh[i]) # Less than because it's rank
    blankdf$Acc[i] <- sum(na.omit(auc.df[auc.df$`New data`=="New data",models[x]]) < blankdf$Thresh[i])
    print(i)
  }
  blankdf$PredPos <- blankdf$PredPos/max(blankdf$PredPos)
  blankdf$Acc <- blankdf$Acc/max(blankdf$Acc)
  
  return(blankdf)
})

d2 <- bind_rows(l2)
d <- bind_rows(d, d2)

d %>% 
  filter(!(Model %in% c("Ensemble.2"))) %>% 
  mutate(Model = recode(Model, !!!c("Ensemble.1" = "Ensemble"))) %>% 
  mutate(Model = factor(Model, levels = c("Network.1", "Network.2", "Network.3", "Network.4",
                                          "Trait.1", "Trait.2", "Trait.3", "Hybrid.1",
                                          "Ensemble"))) %>% 
  ggplot(aes(x = PredPos, y = Acc, color = Model, group = Model, label = factor(Model))) + 
  scale_color_manual(values = c("indianred",  "indianred1",  "darkred",  "red",
                       "lightblue",  "lightskyblue",  "lightskyblue3", "plum3", "black")) + 
  geom_abline(slope = 1, intercept = 0, col = 'grey70') + 
  geom_line(lwd = 0.70, alpha = 0.7) + 
  xlab("Predicted positivity rate (%)") + 
  ylab("New hosts correctly identified (%)") + 
  theme_bw(base_size = 15) + 
  theme(legend.position = c(0.87, 0.25),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"),
        axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 7)) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1))) -> g2

scores2 <- sapply(unique(d2$Model), function(j) {
  sintegral(d2 %>% filter(Model == j) %>% pull(PredPos),
            d2 %>% filter(Model == j) %>% pull(Acc))$int })

# Threshold that bad boy

d %>% filter(Model=="Ensemble.2") %>% 
  mutate(Diff = Acc - PredPos) %>% 
  ggplot(aes(x = Thresh, y = Diff)) + 
  geom_line() 

d %>% filter(Model=="Ensemble.2") %>% 
  mutate(Diff = Acc - PredPos) -> tdf

th <- tdf$Thresh[which(tdf$Diff == max(tdf$Diff))]

table(auc.df$Ensemble.2 < th)

table(auc.df$`New data`, auc.df$Ensemble.2 < th)

########## Threshold these with 90% sensitivity 

auc.df %>% select(Sp, `New data`,
                  Ensemble.2) %>%
  mutate(Binary = recode(`New data`, !!!c("Reported" = 1,
                                          "New data" = 1,
                                          "Unreported" = 0))) %>%
  mutate(Negative = 1 - Ensemble.2) %>%
  select(Sp, Binary, Negative, Ensemble.2) -> threshdf

tvalues <- optimal.thresholds(threshdf[,c("Sp","Binary","Negative")],
                              threshold = 10001,
                              opt.methods = 10,
                              req.sens = 0.9,
                              na.rm = TRUE)

threshdf %>% mutate(EnsembleBinary = (Ensemble.2 < (1 - tvalues$Negative))) %>%
  select(-Negative) %>%
  write_csv("WeightedEnsemble2020.csv")

names(w1) <- scores$Model
write_rds(w1, "EnsembleWeights.rds")
