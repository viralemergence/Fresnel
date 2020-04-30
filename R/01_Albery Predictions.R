
# 01_Albery Predictions ####

# Prediction ####

library(ggrepel); library(tidyverse); library(SpRanger); library(cowplot); library(patchwork)
library(ggregplot)

theme_set(theme_cowplot())

Panth1 <- read.delim("Data/PanTHERIA_1-0_WR05_Aug2008.txt") %>%
  dplyr::rename_all(~str_replace(.x, "MSW05_", "h")) %>%
  rename(Sp = hBinomial)

Panth1$Sp <- Panth1$Sp %>% str_replace(" ", "_")

# load("~/LargeFiles/MammalStackFullMercator.Rdata")

PredictedNetwork <- fread("Data/AlberyPredicted.csv")

PredictedNetwork %>% as.matrix -> PredictedNetwork

PredictedNetwork <- 
  PredictedNetwork[,-1]

rownames(PredictedNetwork) <- colnames(PredictedNetwork)

NetworkPredict(c("Rhinolophus_affinis"), (PredictedNetwork)) %>%
  as.data.frame() %>% left_join(Panth1, by = "Sp") %>%
  #filter(!hOrder == "Chiroptera") %>% 
  dplyr::select(1:3, Sp, hOrder, hFamily, hGenus) ->
  affinisPredictedBats

affinisPredictedBats %>% nrow

NetworkPredict(c("Rhinolophus_affinis"), as.matrix(PredictedNetwork)) %>%
  as.data.frame() %>% left_join(Panth1, by = "Sp") %>%
  filter(!hOrder == "Chiroptera") %>%
  dplyr::select(1:3, Sp, hOrder, hFamily, hGenus) ->
  
  affinisPredicted

saveRDS(affinisPredicted, file = "Intermediate/AlberyPredicted.rds")
saveRDS(affinisPredictedBats, file = "Intermediate/AlberyPredictedBats.rds")

affinisPredicted %>% nrow

affinisPredicted %>% ggplot(aes(1, Count)) + geom_text(aes(label = Sp))

affinisPredictedBats %>%
  ggplot(aes(Rank, Count)) +
  labs(y = "Sharing Probability") +
  geom_point() +
  geom_label_repel(data = affinisPredictedBats %>% filter(Count>0.65),
                   aes(label = Sp), xlim = c(-4000, -500)) +
  #lims(y = c(0, 1)) +
  scale_x_reverse(limits = c(4200, -250)) -> 
  
  BatPredictions

affinisPredicted %>%
  ggplot(aes(Rank, Count)) +
  labs(y = "Sharing Probability") +
  geom_point() +
  geom_label_repel(data = affinisPredicted %>% filter(Count>0.27),
                   aes(label = Sp), #xlim = c(-4000, -500), #direction = "x", 
                   force = 10) +
  scale_x_reverse(limits = c(4200, -250)) ->
  
  NonBatPredictions

BatPredictions/NonBatPredictions

NonBatPredictions + geom_vline(xintercept = 200)
NonBatPredictions + geom_vline(xintercept = 400)

affinisPredicted %>% 
  #  filter(Rank<400) %>%
  SinaGraph("hFamily", "Count", Order = T, Just = T, Scale = "width") + 
  labs(y = "Mean probability") +
  theme(legend.position = "none")

affinisPredicted %>% 
  #  filter(Rank<400) %>%
  SinaGraph("hOrder", "Count", Order = T, Just = T, Scale = "width") + 
  labs(y = "Mean probability") +
  theme(legend.position = "none")

affinisPredicted %>% 
  filter(Rank<400) %>%
  group_by(hFamily) %>% 
  summarise(SumProbs = sum(Count), MeanProbs = mean(Count)) %>%
  arrange(desc(MeanProbs))

affinisPredicted %>% 
  filter(Rank<200) %>%
  group_by(hFamily) %>% 
  summarise(N = n(),
            SumProbs = sum(Count), 
            MeanProbs = mean(Count)) %>%
  arrange(desc(MeanProbs))


affinisPredicted %>% filter(hFamily == "Mustelidae") %>%
  SinaGraph("hGenus", "Count", Order = T, Just = T, Scale = "width")



