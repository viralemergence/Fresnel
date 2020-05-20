
# Fresnel Figures ####

library(tidyverse); library(cowplot); library(colorspace); library(patchwork); library(ggpubr); library(ggregplot)

theme_set(theme_cowplot() + theme(strip.background = element_rect(fill = "white")))

AlberPalettes <- c("YlGnBu","Reds","BuPu", "PiYG")

AlberColours <- sapply(AlberPalettes, function(a) RColorBrewer::brewer.pal(5, a)[4])

AlberColours[length(AlberColours)+1:2] <- 
  
  RColorBrewer::brewer.pal(11, AlberPalettes[[4]])[c(2,10)]

AlberColours <- c(AlberColours, Pink = "#FD6396", Blue = "#3C78D8")

Relabel <- c(glue::glue("Network.{1:4}"), 
             glue::glue("Trait.{1:3}"))

names(Relabel) <- c("R.Po2", "R.Po3",
                    "R.Dal1","R.Far1",
                    "R.Gut1", "R.Car3",
                    "R.Alb")

Relabel[intersect(names(Relabel), names(NonBatModels_IS))] ->
  
  Relabel

# Figure 1: Observed v Predicted panels ####

NonBatModels_IS %>% 
  gather("Key", "Value", -c(Sp, Betacov, Rank, PropRank, InSample)) %>%
  #filter(str_detect(Key, "Alb|Car|Dal|Gut")) %>%
  mutate_at("Key", ~.x %>% recode(!!!Relabel)) %>%
  ggplot(aes(Value, Betacov)) + 
  geom_point(alpha = 0.3, colour = AlberColours[[3]]) + 
  geom_smooth(method = glm, 
              method.args = list(family = "binomial"),
              fill = NA, colour = "black") +
  #geom_smooth(method = lm, fill = NA, colour = "black") +
  #geom_smooth(fill = NA, colour = "black") +
  facet_wrap(~Key, nrow = 2) +
  stat_cor(label.y = 1.2,
           aes(label = ..rr.label..)) +
  #coord_fixed() +
  scale_x_continuous(breaks = c(0, 0.5, 1.3)) +
  scale_y_continuous(breaks = c(0:5/5), limits = c(-0, 1.25)) +
  labs(x = "Proportional rank") -> 
  
  SingleCorrelations

NonBatModels_IS %>% 
  mutate(Key = "Multi-model ensemble") %>%
  ggplot(aes(PropRank, Betacov)) + 
  #ggtitle("Model assemblage") +
  geom_point(alpha = 0.6, colour = AlberColours[[3]], 
             position = position_jitter(h = 0.05)) + 
  #geom_smooth(method = lm, fill = NA, colour = "black") +
  geom_smooth(method = glm, 
              method.args = list(family = "binomial"),
              fill = NA, colour = "black") +
  # geom_smooth(fill = NA, colour = "black") +
  #coord_fixed() + 
  lims(x = c(0, 1)) +
  scale_y_continuous(breaks = c(0:5/5), 
                     limits = c(-0.1, 1.25)) +
  stat_cor(label.y = 1.2, method = "spearman",
           aes(label = ..rr.label..)) +
  facet_wrap(~Key) +
  labs(x = "Proportional rank") ->
  
  OverallCorrelations

(SingleCorrelations|OverallCorrelations) + 
  plot_layout(widths = c(1.35,1)) +
  ggsave("Figures/MammalObs_Pred_CorrelationsHorizontal.jpeg", 
         units = "mm", width = 325, height = 150)

# Figure 2: Inter-Model Agreement ####

# Tile plot ####

{
  
  NonBatModels_IS %>% 
    rename_all(~ifelse(.x %in% names(Relabel), recode(.x, !!!Relabel), .x)) %>% 
    select(as.character(Relabel)) %>% cor(use = "complete.obs", method = "spearman") -> 
    
    CorDF_IS
  
  CorDF_IS %>% rowSums %>% sort %>% rev %>% names -> ModelLimits
  
  Relabel -> ModelLimits
  
  CorDF_IS %>% reshape2::melt() %>% #slice(which(lower.tri(CorDF_IS)))
    
    mutate_at("Var1", ~factor(.x, levels = c(ModelLimits))) %>%
    mutate_at("Var2", ~factor(.x, levels = rev(ModelLimits))) %>%
    ggplot(aes(Var1, Var2)) + geom_tile(aes(fill = value)) + #scale_y_reverse()
    
    # coord_fixed() + 
    
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
    
    # scale_fill_continuous_diverging(palette = "Tropic", limits = c(-1, 1)) +
    scale_fill_gradient2(low = AlberColours[[1]], 
                         mid = "white", 
                         high = AlberColours[[5]],
                         # high = AlberColours[["Pink"]],
                         midpoint = 0, limits = c(-1, 1)) +
    
    labs(x = NULL, y = NULL, fill = "Correlation") +
    
    NULL -> TilePlot_IS
  
  TilePlot_IS
  
  # Bump and agreements ####
  
  CorDF_IS %>% rowSums %>% #sort %>% 
    #rev %>% 
    names -> ModelLimits
  
  Relabel -> ModelLimits
  
  NonBatModels_IS %>%
    arrange(PropRank) %>%
    rename_all(~ifelse(.x %in% names(Relabel), recode(.x, !!!Relabel), .x)) %>% 
    filter(Betacov == 0) %>%
    slice(1:10) %>% 
    mutate_at("Sp", ~glue::glue(" {1:n()}. {.x}")) %>%
    gather("Key", "Value", -c(Sp, Betacov, Rank, PropRank, InSample)) %>%
    mutate_at("Value", ~.x*1) %>%
    mutate_at("Key", ~factor(.x, levels = ModelLimits)) %>%
    mutate_at("Sp", ~factor(.x, levels = unique(.x))) %>%
    filter(!is.na(Value)) %>% 
    mutate(KeyJitter = as.numeric(as.factor(Key)) + runif(n(), -0.05, 0.05)) -> 
    TopPredictions_IS
  
  NonBatModels_IS %>% 
    rename_all(~ifelse(.x %in% names(Relabel), recode(.x, !!!Relabel), .x)) %>% 
    gather("Key", "Value", -c(Sp, Betacov, Rank, PropRank, InSample)) %>%
    mutate_at("Value", ~.x*1) %>%
    mutate_at("Key", ~factor(.x, levels = ModelLimits)) %>%
    mutate_at("Key", ~.x %>% recode(!!!Relabel)) %>%
    filter(!is.na(Value)) -> 
    LongNonBatModels_IS
  
  LongNonBatModels_IS %<>% 
    mutate(KeyJitter = as.numeric(as.factor(Key)) + runif(n(), -0.05, 0.05)) %>%
    anti_join(TopPredictions_IS %>% dplyr::select(Sp), by = "Sp")
  
  LongNonBatModels_IS %>%
    ggplot(aes(KeyJitter, (Value))) + 
    #geom_point(alpha = 0.3) + 
    geom_line(aes(group = Sp), alpha = 0.025) +
    #gghighlight(PropRank<0.05) +
    geom_line(data = TopPredictions_IS, aes(group = Sp, colour = Sp), size = 1.5) +
    # theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(axis.title.y = element_text(vjust = -5)) +
    scale_x_continuous(breaks = 1:length(ModelLimits), 
                       labels = ModelLimits) +
    scale_y_reverse() +
    scale_colour_discrete_sequential(palette = AlberPalettes[[3]], rev = F, nmax = 12, 
                                     labels = levels(TopPredictions_IS$Sp) %>% 
                                       str_replace(" ", " <i>") %>% str_c("</i>")) +    
    theme(legend.text = element_markdown()) +    
    labs(x = "Model", y = "Proportional rank", colour = "Top 10 unknown hosts") ->
    
    BumpPlot_IS

}

(TilePlot_IS/
  
  (BumpPlot_IS + 
     theme(legend.position = "right") +
     guides(colour = guide_legend(direction = "vertical",
                                  hjust = 0.5,
                                  ncol = 1,
                                  nrow = 10)))) +
  
  # plot_layout(heights = c(1, 1)) +
  plot_annotation(tag_levels = "A") +
  ggsave("Figures/MammalIn_Out_ModelAgreement.jpeg", units = "mm", height = 200, width = 175)


# Auxiliary plots ####

# Comparative density ####

NonBatModels_IS %>% 
  gather("Key", "Value", -c(Sp, Betacov, Rank, PropRank, InSample)) %>%
  #filter(str_detect(Key, "Alb|Car|Dal|Gut")) %>%
  mutate_at("Key", ~.x %>% recode(!!!Relabel)) %>%
  ggplot(aes(Value)) + 
  geom_density(data = NonBatModels_IS, inherit.aes = F, aes(x = PropRank), colour = "black", size = 3) +
  geom_density(aes(colour = Key)) +
  scale_colour_discrete_sequential(palette = AlberPalettes[[1]], nmax = 6, rev = F)

# Comparing in and out-sample prediction of new hosts ####

NonBatModels_OS %>% filter(Betacov == 0) %>% 
  slice(1:200) %>% mutate_at("InSample", factor) %>% 
  mutate(Rank = 1:n()) %>% 
  #SinaGraph("InSample", "PropRank") %>% 
  SinaGraph("InSample", "PropRank")

NonBatModels_OS %>% #filter(Betacov == 0) %>% 
  slice(1:200) %>% mutate_at("InSample", factor) %>% 
  mutate(Rank = 1:n()) %>% 
  #SinaGraph("InSample", "PropRank") %>% 
  SinaGraph("InSample", "PropRank")

NonBatModels_OS %>% filter(Betacov == 0) %>% 
  slice(1:200) %>% mutate_at("InSample", factor) %>% 
  mutate(Rank = 1:n()) %>% 
  #SinaGraph("InSample", "PropRank") %>% 
  SinaGraph("InSample", "Rank")

NonBatModels_OS %>% #filter(Betacov == 0) %>% 
  slice(1:200) %>% mutate_at("InSample", factor) %>% 
  mutate(Rank = 1:n()) %>% 
  #SinaGraph("InSample", "PropRank") %>% 
  SinaGraph("InSample", "Rank")
