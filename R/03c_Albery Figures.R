
# 03c_Albery Figures ####

library(ggregplot); library(tidyverse)

Panth1 <- 
  read.delim("Data/PanTHERIA_1-0_WR05_Aug2008.txt") %>%
  dplyr::rename_all(~str_replace(.x, "MSW05_", "h")) %>%
  rename(Sp = hBinomial)

Panth1$Sp <- Panth1$Sp %>% str_replace(" ", "_")

R_malayanusPredictedNonBats <-
  readRDS(file = "Github/Repos/albery-betacov/Intermediate/AlberyPredictedNonBats_R_malayanus.rds")

R_malayanusPredictedBats <-
  readRDS(file = "Github/Repos/albery-betacov/Intermediate/AlberyPredictedBats_R_malayanus.rds")

R_affinisPredictedNonBats <-
  readRDS(file = "Github/Repos/albery-betacov/Intermediate/AlberyPredictedNonBats_R_affinis.rds")

R_affinisPredictedBats <-
  readRDS(file = "Github/Repos/albery-betacov/Intermediate/AlberyPredictedBats_R_affinis.rds")

# Figures ####

TextSize <- 8.5

R_affinisPredictedBats %>% left_join(Panth1) %>%
  SinaGraph("hOrder", "Count", Order = T, Just = T) +
  scale_colour_discrete_sequential(palette = AlberPalettes[[1]],
                                   nmax = 30, rev = F) +
  theme(legend.position = "none") +
  ggtitle(expression(italic("Rhinolophus affinis"))) +
  theme(axis.text.x = element_text(size = TextSize)) +
  labs(y = "Viral sharing probability", x = "Host order") +
  lims(y = c(0, 1)) +
  #ggsave("Figures/AlberyR_affinis.jpeg")
  
  R_malayanusPredictedBats %>% left_join(Panth1) %>%
  SinaGraph("hOrder", "Count", Order = T, Just = T) +
  scale_colour_discrete_sequential(palette = AlberPalettes[[1]],
                                   nmax = 30, rev = F) +
  theme(legend.position = "none") +
  ggtitle(expression(italic("Rhinolophus malayanus"))) +
  theme(axis.text.x = element_text(size = TextSize)) +
  # labs(y = "Viral sharing probability", x = "Host order") +
  labs(y = NULL, x = "Host order") +
  lims(y = c(0, 1)) +
  plot_annotation(tag_levels = "A") +
  ggsave("Figures/AlberyRhinolophus.jpeg", units = "mm", height = 150, width = 250)


R_affinisPredictedBats %>% left_join(Panth1) %>% mutate(SharingSp = "R_affinis") %>%
  bind_rows(R_malayanusPredictedBats %>% left_join(Panth1) %>% mutate(SharingSp = "R_malayanus")) ->
  AggregDF

AggregDF %>% group_by(hOrder, SharingSp) %>% 
  summarise(Mean = mean(Count), N = n(), SD = sd(Count)) %>%
  mutate(SE = SD/(N^0.5)) -> 
  
  MeanDF

AggregDF %>% ggplot(aes(hOrder))


(R_affinisPredictedBats %>% left_join(Panth1) %>%
    filter(!hOrder == "Chiroptera") %>%
    SinaGraph("hFamily", "Count", Order = T, Just = T) +
    scale_colour_discrete_sequential(palette = AlberPalettes[[1]],
                                     nmax = 130, rev = F) +
    theme(legend.position = "none") +
    ggtitle(expression(italic("Rhinolophus affinis"))) +
    theme(axis.text.x = element_text(size = TextSize)) +
    labs(y = "Viral sharing probability", x = "Host order") +
    lims(y = c(0, 0.35))) /
  
  ( R_malayanusPredictedBats %>% left_join(Panth1) %>%
      filter(!hOrder == "Chiroptera") %>%
      SinaGraph("hFamily", "Count", Order = T, Just = T) +
      scale_colour_discrete_sequential(palette = AlberPalettes[[1]],
                                       nmax = 130, rev = F) +
      theme(legend.position = "none") +
      ggtitle(expression(italic("Rhinolophus malayanus"))) +
      theme(axis.text.x = element_text(size = TextSize)) +
      # labs(y = "Viral sharing probability", x = "Host order") +
      labs(y = NULL, x = "Host order") +
      lims(y = c(0, 0.35))) +
  plot_annotation(tag_levels = "A") +
  ggsave("Figures/AlberyRhinolophus_Family.jpeg", 
         units = "mm", height = 250, width = 300, dpi = 300)

