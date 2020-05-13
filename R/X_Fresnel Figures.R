
# Fresnel Figures ####

library(tidyverse); library(cowplot); library(colorspace); library(patchwork); library(ggpubr)

theme_set(theme_cowplot() + theme(strip.background = element_rect(fill = "white")))

AlberPalettes <- c("YlGnBu","Reds","BuPu", "PiYG")

AlberColours <- sapply(AlberPalettes, function(a) RColorBrewer::brewer.pal(5, a)[4])

Relabel <- c(
  
  "Network-based.1", 
  "Network-based.2",
  "Phylogenetic.1" ,
  "Phylogenetic.2",
  "Phylogenetic.3",
  "Trait-based.1",
  "Trait-based.2",
  "Trait-based.3"
  
)

Relabel <- c(
  
  "Network.1", 
  "Network.2",
  "Phylog.1" ,
  "Phylog.2",
  "Phylog.3",
  "Trait.1",
  "Trait.2",
  "Trait.3"
  
)

names(Relabel) <- c("R.Po1", "R.Po2",
                    "R.Bec", "R.Alb", "R.Far",
                    "R.Gut", "R.Car", "R.Dal")

Relabel[intersect(names(Relabel), names(Models))] ->
  
  Relabel

# Figure 1: Observed v Predicted panels ####

Models %>% 
  gather("Key", "Value", -c(Sp, Betacov, Rank, PropRank)) %>%
  #filter(str_detect(Key, "Alb|Car|Dal|Gut")) %>%
  #mutate_at("Key", ~.x %>% recode(!!!Relabel)) %>%
  ggplot(aes(Value, Betacov)) + 
  geom_point(alpha = 0.3, colour = AlberColours[[3]]) + 
  #geom_smooth(method = lm, fill = NA, colour = "black") +
  geom_smooth(fill = NA, colour = "black") +
  facet_wrap(~Key, nrow = 2) +
  stat_cor(label.y = 1.2) +
  lims(y = c(-0.2, 1.2)) + coord_fixed() +
  scale_x_continuous(breaks = c(0, 0.5, 1.3)) +
  scale_y_continuous(breaks = c(0:5/5)) +
  labs(x = "Proportional rank") -> 
  
  SingleCorrelations

Models %>% 
  mutate(Key = "Multi-model ensemble") %>%
  ggplot(aes(PropRank, Betacov)) + 
  #ggtitle("Model assemblage") +
  geom_point(alpha = 0.6, colour = AlberColours[[3]], position = position_jitter(h = 0.1)) + 
  #geom_smooth(method = lm, fill = NA, colour = "black") +
  geom_smooth(fill = NA, colour = "black") +
  coord_fixed() + 
  lims(x = c(0, 1), 
       y = c(-0.2, 1.25)) +
  scale_y_continuous(breaks = c(0:5/5)) +
  stat_cor(label.y = 1.2, method = "spearman") +
  facet_wrap(~Key) +
  labs(x = "Proportional rank") ->
  
  OverallCorrelations

(SingleCorrelations|OverallCorrelations) + 
  ggsave("Figures/Obs_Pred_CorrelationsHorizontal.jpeg", 
         units = "mm", width = 250, height = 150)

(OverallCorrelations/(SingleCorrelations + facet_wrap(~Key, nrow = 2))) + 
  plot_layout(heights = c(1, 1.1)) +
  ggsave("Figures/Obs_Pred_CorrelationsVertical.jpeg", 
         units = "mm", width = 150, height = 250)


# Figure 2: Tile plot correlations ####

Models %>% 
  rename_all(~ifelse(.x %in% names(Relabel), recode(.x, !!!Relabel), .x)) %>% 
  select(as.character(Relabel)) %>% cor(use = "complete.obs", method = "spearman") -> CorDF

CorDF %>% rowSums %>% sort %>% rev %>% names -> ModelLimits

CorDF %>% reshape2::melt() %>% #slice(which(lower.tri(CorDF)))

  mutate_at("Var1", ~factor(.x, levels = c(ModelLimits))) %>%
  mutate_at("Var2", ~factor(.x, levels = rev(ModelLimits))) %>%
  ggplot(aes(Var1, Var2)) + geom_tile(aes(fill = value)) + #scale_y_reverse()
  
  coord_fixed() + 
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
  
  # scale_fill_continuous_diverging(palette = "Tropic", limits = c(-1, 1)) +
  scale_fill_gradient2(low = AlberColours[[1]], mid = "white", high = AlberColours[[3]],
                       midpoint = 0, limits = c(-1, 1)) +
  
  labs(x = NULL, y = NULL, fill = "Correlation") +
  
  NULL -> TilePlot

TilePlot

# Figure 3: Bump and agreements ####

CorDF %>% rowSums %>% sort %>% rev %>% names -> ModelLimits

Models %>%
  rename_all(~ifelse(.x %in% names(Relabel), recode(.x, !!!Relabel), .x)) %>% 
  filter(Betacov == 0) %>%
  slice(1:10) %>% 
  gather("Key", "Value", -c(Sp, Betacov, Rank, PropRank)) %>%
  mutate_at("Value", ~.x*1) %>%
  mutate_at("Key", ~factor(.x, levels = ModelLimits)) %>%
  filter(!is.na(Value)) %>% 
  mutate(KeyJitter = as.numeric(as.factor(Key)) + runif(n(), -0.05, 0.05)) -> 
  TopPredictions

Models %>% 
  rename_all(~ifelse(.x %in% names(Relabel), recode(.x, !!!Relabel), .x)) %>% 
  gather("Key", "Value", -c(Sp, Betacov, Rank, PropRank)) %>%
  mutate_at("Value", ~.x*1) %>%
  mutate_at("Key", ~factor(.x, levels = ModelLimits)) %>%
  mutate_at("Key", ~.x %>% recode(!!!Relabel)) %>%
  filter(!is.na(Value)) -> LongModels

LongModels %<>% mutate(KeyJitter = as.numeric(as.factor(Key)) + runif(n(), -0.05, 0.05)) %>%
  anti_join(TopPredictions %>% dplyr::select(Sp), by = "Sp")

LongModels %>%
  ggplot(aes(KeyJitter, (Value))) + 
  #geom_point(alpha = 0.3) + 
  geom_line(aes(group = Sp), alpha = 0.025) +
  #gghighlight(PropRank<0.05) +
  geom_line(data = TopPredictions, aes(group = Sp, colour = Sp), size = 1.5) +
  # theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.title.y = element_text(vjust = -5)) +
  scale_x_continuous(breaks = 1:length(ModelLimits), 
                     labels = ModelLimits) +
  scale_y_reverse() +
  scale_colour_discrete_sequential(palette = AlberPalettes[[3]], rev = F, nmax = 12) +
  labs(x = "Model", y = "Proportional rank", colour = "Top 10 unknown hosts") ->
  
  BumpPlot

(TilePlot/BumpPlot) + 
  #plot_layout(widths = c(1)) +
  plot_annotation(tag_levels = "A") +
  ggsave("Figures/Model_Prediction_Correlations.jpeg", 
         units = "mm", width = 200, height = 250)


# Not sure what this is #####

Models %>% group_by(Betacov) %>% 
  slice(1:10) %>% 
  gather("Key", "Value", -c(Sp, Betacov, Rank, PropRank)) %>%
  mutate_at("Value", ~.x*1) %>%
  mutate_at("Key", ~factor(.x, levels = ModelLimits)) %>%
  filter(!is.na(Value)) -> TopPredictions2

Models %>% 
  gather("Key", "Value", -c(Sp, Betacov, Rank, PropRank)) %>%
  mutate_at("Value", ~.x*1) %>%
  mutate_at("Key", ~factor(.x, levels = ModelLimits)) %>%
  filter(!is.na(Value)) %>%
  ggplot(aes(as.numeric(as.factor(Key)), (Value))) + 
  geom_point(alpha = 0.3) + 
  geom_line(aes(group = Sp), alpha = 0.05) +
  #gghighlight(PropRank<0.05) +
  geom_line(data = TopPredictions2, aes(group = Sp, colour = Sp), size = 2) +
  # theme(legend.position = "none") +
  scale_x_continuous(breaks = 1:length(ModelLimits), 
                     labels = ModelLimits) +
  scale_y_reverse() +
  labs(x = "Model", y = "Proportional rank", colour = "Top 10") +
  facet_wrap(~Betacov)

Models %>% 
  gather("Key", "Value", -c(Sp, Betacov, Rank, PropRank)) %>%
  mutate_at("Sp", ~.x %>% fct_reorder(Value, median)) %>%
  ggplot(aes(Sp, Value)) + geom_point(alpha = 0.1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

# Auxiliary plots ####

# Comparative density ####

Models %>% 
  gather("Key", "Value", -c(Sp, Betacov, Rank, PropRank)) %>%
  filter(str_detect(Key, "Alb|Car|Dal|Gut")) %>%
  mutate_at("Key", ~.x %>% recode(!!!Relabel)) %>%
  ggplot(aes(Value)) + 
  geom_density(data = Models, inherit.aes = F, aes(x = PropRank), colour = "black", size = 3) +
  geom_density(aes(colour = Key)) +
  scale_colour_discrete_sequential(palette = AlberPalettes[[1]], nmax = 6, rev = F)
