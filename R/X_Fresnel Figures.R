
# Fresnel Figures ####

library(tidyverse); library(cowplot); library(colorspace)

theme_set(theme_cowplot() + theme(strip.background = element_rect(fill = "white")))

# Figure 1: Observed v Predicted panels ####

Models %>% gather("Key", "Value", -c(Sp, Betacov, Rank, PropRank)) %>%
  ggplot(aes(Value, Betacov)) + geom_point() + 
  geom_smooth(method = lm) +
  facet_wrap(~Key, nrow = 2) +
  ggpubr::stat_cor() +
  lims(y = c(0, 1)) + coord_fixed() +
  scale_x_continuous(breaks = c(0, 0.5, 1)) +
  labs(x = "Proportional rank") -> 
  
  ModelCorrelations

Models %>% 
  ggplot(aes(PropRank, Betacov)) + 
  geom_point() + geom_smooth(method = lm) +
  coord_fixed() + lims(x = c(0, 1), y = c(0, 1)) +
  ggpubr::stat_cor()  ->
  
  OverallCorrelations

(ModelCorrelations|OverallCorrelations) + 
  plot_layout(widths = c(3.5, 2)) +
  ggsave("Obs_Pred_Correlations.jpeg", 
         units = "mm", width = 250, height = 150)


# Figure 2: Tile plot correlations ####

Models %>% select(R.Alb:R.Po2) %>% cor(use = "complete.obs") -> CorDF

CorDF %>% rowSums %>% sort %>% rev %>% names -> ModelLimits

CorDF %>% reshape2::melt() %>% #slice(which(lower.tri(CorDF)))

  mutate_at("Var1", ~factor(.x, levels = c(ModelLimits))) %>%
  mutate_at("Var2", ~factor(.x, levels = rev(ModelLimits))) %>%
  ggplot(aes(Var1, Var2)) + geom_tile(aes(fill = value)) + #scale_y_reverse()
  
  coord_fixed() + 
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  scale_fill_continuous_diverging(palette = "Tropic", limits = c(-1, 1)) +
  
  labs(x = NULL, y = NULL) +
  
  NULL -> TilePlot



# Figure 3: Bump and agreements ####

CorDF %>% rowSums %>% sort %>% rev %>% names -> ModelLimits

Models %>%
  filter(Betacov == 0) %>%
  slice(1:10) %>% 
  gather("Key", "Value", -c(Sp, Betacov, Rank, PropRank)) %>%
  mutate_at("Value", ~.x*1) %>%
  mutate_at("Key", ~factor(.x, levels = ModelLimits)) %>%
  filter(!is.na(Value)) -> TopPredictions

Models %>% 
  gather("Key", "Value", -c(Sp, Betacov, Rank, PropRank)) %>%
  mutate_at("Value", ~.x*1) %>%
  mutate_at("Key", ~factor(.x, levels = ModelLimits)) %>%
  filter(!is.na(Value)) %>%
  ggplot(aes(as.numeric(as.factor(Key)), (Value))) + 
  #geom_point(alpha = 0.3) + 
  geom_line(aes(group = Sp), alpha = 0.025) +
  #gghighlight(PropRank<0.05) +
  geom_line(data = TopPredictions, aes(group = Sp, colour = Sp), size = 1.5) +
  # theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = 1:length(ModelLimits), 
                     labels = ModelLimits) +
  scale_y_reverse() +
  labs(x = "Model", y = "Proportional rank", colour = "Top 10 unknown hosts") ->
  
  BumpPlot

(TilePlot/BumpPlot) + 
  plot_layout(widths = c(1)) +
  ggsave("Model_Prediction_Correlations.jpeg", 
         units = "mm", width = 200, height = 250)


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


# Figure 4: GGtree ####
