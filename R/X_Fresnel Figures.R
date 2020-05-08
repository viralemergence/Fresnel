
# Fresnel Figures ####

library(tidyverse); library(cowplot); library(colorspace)

theme_set(theme_cowplot() + theme(strip.background = element_rect(fill = "white")))

# Figure 1: Observed v Predicted panels ####

Models %>% gather("Key", "Value", -c(Sp, Betacov, Rank, PropRank)) %>%
  ggplot(aes(Value, Betacov)) + geom_point() + 
  geom_smooth(method = lm) +
  facet_wrap(~Key) +
  ggpubr::stat_cor() +
  lims(y = c(0, 1))


# Figure 2: Tile plot correlations ####


Models %>% select(R.Alb:R.Po2) %>% cor(use = "complete.obs") -> CorDF

CorDF %>% reshape2::melt() %>% #slice(which(lower.tri(CorDF)))

  mutate_at("Var1", ~factor(.x, levels = rev(levels(.x)))) %>%
  ggplot(aes(Var1, Var2)) + geom_tile(aes(fill = value)) + #scale_y_reverse()
  
  coord_fixed() + 
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  scale_fill_continuous_diverging(palette = "Tropic", limits = c(-1, 1)) +
  
  labs(x = NULL, y = NULL) +
  
  NULL



# Figure 3: Bump and agreements ####

Models %>% gather("Key", "Value", -c(Sp, Betacov, Rank, PropRank))



# Figure 4: GGtree ####
