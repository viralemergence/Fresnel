
# X_Comparison ####

library(colorspace); library(cowplot)

theme_set(theme_cowplot())

AlberyPredictedBats %>% 
  rename(AlberyRank = Rank) %>%
  full_join(Damas, by = c("Sp" = "Species")) %>%
  full_join(CarlsonPredicted %>% rename(CarlsonRank = Rank), 
            by = c("Sp" = "Host_species")) %>%
  mutate_at("DamasRank", ~ifelse(is.na(.x), "Unknown", .x) %>%
              factor(levels = c("Unknown", "5", "4"))) %>% 
  full_join(FarrellPredicted, by = c("Sp" = "Host")) %>% 
  full_join(PoisotPredicted, by = c("Sp" = "Host")) %>% 
  full_join(BeckerPredicted, by = c("Sp")) ->
  
  FullPredictions

FullPredictions %>% mutate(FarrellBinary = as.numeric(!is.na(FarrellRank))) ->
  
  FullPredictions

VarNames <- paste0(c("Albery", "Carlson", "Damas", "Farrell", "Poisot", "Becker"), "Rank")

GregLimit <- 7
ColinLimit <- 300

FullPredictions %>% 
  filter(hOrder == "Chiroptera") %>%
  select(all_of(VarNames)) %>%
  mutate_at("AlberyRank", GregCube) %>%
  #mutate_all(~1 - .x) %>%
  GGally::ggpairs(lower = list(continuous = "smooth"))


FullPredictions %>% 
  filter(hOrder == "Chiroptera") %>%
  ggplot(aes(GregCube(AlberyRank), as.numeric(DamasRank))) + 
  geom_point() + ggpubr::stat_cor() + 
  labs(x = expression(AlberyRank^{3})) +
  geom_smooth()

FullPredictions %>% 
  filter(hOrder == "Chiroptera") %>%
  ggplot(aes(GregCube(AlberyRank), CarlsonRank)) + 
  geom_point() + ggpubr::stat_cor() + 
  labs(x = expression(AlberyRank^{3})) +
  geom_smooth(colour = AlberColours[[1]])

(OutsetPredictionPlot <- FullPredictions %>% 
  filter(hOrder == "Chiroptera") %>%
  ggplot(aes(GregCube(AlberyRank), CarlsonRank)) + 
  
  geom_rect(xmin = -Inf, ymin = -Inf, xmax = GregLimit, ymax = ColinLimit,
            fill = NA, lty = 2, alpha = 0.3, colour = "grey") +
  
  geom_point(aes(size = DamasRank, alpha = DamasRank, colour = as.factor(Betacov))) +
  ggpubr::stat_cor() + 
  labs(x = expression(AlberyRank^{3})) +
  geom_smooth(method = lm, colour = AlberColours[[1]], fill = NA) +
  stat_smooth(geom = "ribbon", method = lm, 
              colour = AlberColours[[1]], fill = NA, lty = 2) +
  # scale_x_continuous(breaks = c(1:16), labels = c(1:16)^3) +
  scale_colour_manual(values = c(AlberColours[[1]], AlberColours[[3]]))) %>% plot

(InsetPredictionPlot <- FullPredictions %>% 
  filter(AlberyRank<(GregLimit^3)&CarlsonRank<ColinLimit) %>%
  filter(hOrder == "Chiroptera") %>%
  ggplot(aes(GregCube(AlberyRank), CarlsonRank))+
  # ggpubr::stat_cor() + 
  labs(x = expression(AlberyRank^{3})) +
  #geom_smooth(method = lm, colour = AlberColours[[1]], fill = NA) +
  #stat_smooth(geom = "ribbon", method = lm, 
  #            colour = AlberColours[[1]], fill = NA, lty = 2) +
  #facet_wrap(~Betacov)
  scale_colour_manual(values = c(AlberColours[[1]], AlberColours[[3]])) + 
  scale_alpha_manual(values = c(0.6, 0.8, 1)) + 
  scale_size_manual(values = c(0.6, 0.8, 1)*5) + 
  geom_text(aes(label = Sp, size = DamasRank, alpha = DamasRank, colour = as.factor(Betacov)))) %>% plot

# OutsetPredictionPlot + ggsave("Shot.jpeg")
# InsetPredictionPlot + ggsave("Chaser.jpeg")

AddPreds <- c("Cynopterus sphinx",
              "Taphozous melanopogon",
              "Hipposideros lekaguli",
              "Rhinolophus shameli",
              "Scotophilus heathii",
              "Megaderma lyra") %>% 
  str_replace_all(" ", "_")

(FullPredictions %>% 
  filter(hOrder == "Chiroptera") %>%
  ggplot(aes(GregCube(AlberyRank), CarlsonRank)) + 
  
  geom_rect(xmin = -Inf, ymin = -Inf, xmax = GregLimit, ymax = ColinLimit,
            fill = NA, lty = 2, alpha = 0.3, colour = "grey") +
  
  geom_point(aes(size = DamasRank, alpha = DamasRank, 
                 shape = Sp %in% AddPreds,
                 colour = as.factor(Betacov))) +
  ggpubr::stat_cor() + 
  labs(x = expression(AlberyRank^{3})) +
  geom_smooth(method = lm, colour = AlberColours[[1]], fill = NA) +
  stat_smooth(geom = "ribbon", method = lm, 
              colour = AlberColours[[1]], fill = NA, lty = 2) +
  # scale_x_continuous(breaks = c(1:16), labels = c(1:16)^3) +
    scale_alpha_manual(values = c(0.6, 0.8, 1)) + 
    facet_wrap(~Sp %in% AddPreds) +
  scale_colour_manual(values = c(AlberColours[[1]], AlberColours[[3]]))) %>% plot

# Adding Phylofactor ####

(FullPredictions %>% 
    filter(hOrder == "Chiroptera") %>%
    ggplot(aes(GregCube(AlberyRank), CarlsonRank)) + 
    
    geom_rect(xmin = -Inf, ymin = -Inf, xmax = GregLimit, ymax = ColinLimit,
              fill = NA, lty = 2, alpha = 0.3, colour = "grey") +
    
    geom_point(aes(size = DamasRank, alpha = DamasRank, 
                   shape = as.factor(BeckerRank),
                   colour = as.factor(Betacov))) +
    
    ggpubr::stat_cor() + 
    labs(x = expression(AlberyRank^{3})) +
    geom_smooth(method = lm, colour = AlberColours[[1]], fill = NA) +
    stat_smooth(geom = "ribbon", method = lm, 
                colour = AlberColours[[1]], fill = NA, lty = 2) +
    # scale_x_continuous(breaks = c(1:16), labels = c(1:16)^3) +
    scale_alpha_manual(values = c(0.6, 0.8, 1)) + 
     facet_wrap(~BeckerRank) +
    scale_colour_manual(values = c(AlberColours[[1]], AlberColours[[3]]))) %>% plot


# Adding Poisot ####

(FullPredictions %>% 
   filter(hOrder == "Chiroptera") %>%
   ggplot(aes(GregCube(AlberyRank), CarlsonRank)) + 
   
   geom_rect(xmin = -Inf, ymin = -Inf, xmax = GregLimit, ymax = ColinLimit,
             fill = NA, lty = 2, alpha = 0.3, colour = "grey") +
   
   geom_point(aes(size = DamasRank, 
                  alpha = PoisotRank, 
                  shape = as.factor(BeckerRank),
                  colour = as.factor(Betacov))) +
   
   ggpubr::stat_cor() + 
   labs(x = expression(AlberyRank^{3})) +
   geom_smooth(method = lm, colour = AlberColours[[1]], fill = NA) +
   stat_smooth(geom = "ribbon", method = lm, 
               colour = AlberColours[[1]], fill = NA, lty = 2) +
   # scale_x_continuous(breaks = c(1:16), labels = c(1:16)^3) +
   # scale_alpha_manual(values = c(0.6, 0.8, 1)) + 
   facet_wrap(~is.na(PoisotRank)) +
   scale_colour_manual(values = c(AlberColours[[1]], AlberColours[[3]]))) %>% plot


# Adding Farrell ####

(FullPredictions %>% 
   filter(hOrder == "Chiroptera") %>%
   ggplot(aes(GregCube(AlberyRank), CarlsonRank)) + 
   
   geom_rect(xmin = -Inf, ymin = -Inf, xmax = GregLimit, ymax = ColinLimit,
             fill = NA, lty = 2, alpha = 0.3, colour = "grey") +
   
   geom_point(aes(size = DamasRank, 
                  alpha = PoisotRank, 
                  shape = as.factor(BeckerRank),
                  colour = as.factor(Betacov))) +
   
   ggpubr::stat_cor() + 
   labs(x = expression(AlberyRank^{3})) +
   geom_smooth(method = lm, colour = AlberColours[[1]], fill = NA) +
   stat_smooth(geom = "ribbon", method = lm, 
               colour = AlberColours[[1]], fill = NA, lty = 2) +
   # scale_x_continuous(breaks = c(1:16), labels = c(1:16)^3) +
   # scale_alpha_manual(values = c(0.6, 0.8, 1)) + 
   facet_wrap(~FarrellBinary) +
   scale_colour_manual(values = c(AlberColours[[1]], AlberColours[[3]]))) %>% plot
