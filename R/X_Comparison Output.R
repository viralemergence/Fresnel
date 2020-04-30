
# X_Comparison ####

library(colorspace)

AlberyPredictedBats %>% 
  rename(AlberyRank = Rank) %>%
  full_join(Damas, by = c("Sp" = "Species")) %>%
  full_join(CarlsonPredicted %>% rename(CarlsonRank = Rank), 
            by = c("Sp" = "Host_species")) %>%
  mutate_at("DamasRank", ~ifelse(is.na(.x), "Unknown", .x) %>% factor(levels = c("Unknown", "5", "4"))) ->
  
  FullPredictions

FullPredictions %>% 
  filter(hOrder == "Chiroptera") %>%
  select(AlberyRank, CarlsonRank, DamasRank) %>%
  mutate_at("AlberyRank", GregCube) %>%
  mutate_all(~1 - .x) %>%
  GGally::ggpairs(lower = list(continuous = "smooth"))

FullPredictions %>% 
  filter(hOrder == "Chiroptera") %>%
  ggplot(aes(GregCube(AlberyRank), DamasRank)) + 
  geom_point() + ggpubr::stat_cor() + 
  labs(x = expression(AlberyRank^{3})) +
  geom_smooth(method = lm)


FullPredictions %>% 
  filter(hOrder == "Chiroptera") %>%
  ggplot(aes(GregCube(AlberyRank), DamasRank)) + 
  geom_point(aes(colour = DamasRank)) + 
  ggpubr::stat_cor() + 
  labs(x = expression(AlberyRank^{3})) +
  geom_smooth(method = lm)


FullPredictions %>% 
  filter(hOrder == "Chiroptera") %>%
  ggplot(aes(GregCube(AlberyRank), CarlsonRank)) + 
  geom_point(aes(size = DamasRank, alpha = DamasRank, colour = as.factor(Betacov))) +
  ggpubr::stat_cor() + 
  labs(x = expression(AlberyRank^{3})) +
  geom_smooth(method = lm, colour = AlberColours[[1]], fill = NA) +
  stat_smooth(geom = "ribbon", method = lm, 
              colour = AlberColours[[1]], fill = NA, lty = 2) +
  #facet_wrap(~Betacov)
  scale_colour_manual(values = c(AlberColours[[1]], AlberColours[[3]]))


FullPredictions %>% 
  filter(AlberyRank<(6^3)&CarlsonRank<300) %>%
  filter(hOrder == "Chiroptera") %>%
  ggplot(aes(GregCube(AlberyRank), CarlsonRank))+
  ggpubr::stat_cor() + 
  labs(x = expression(AlberyRank^{3})) +
  geom_smooth(method = lm, colour = AlberColours[[1]], fill = NA) +
  stat_smooth(geom = "ribbon", method = lm, 
              colour = AlberColours[[1]], fill = NA, lty = 2) +
  #facet_wrap(~Betacov)
  scale_colour_manual(values = c(AlberColours[[1]], AlberColours[[3]])) + 
  geom_text(aes(label = Sp, size = DamasRank, alpha = DamasRank, colour = as.factor(Betacov))) 
