
# 03_Damas Predictions
# Adding the Damas dataset for ####
# https://www.biorxiv.org/content/10.1101/2020.04.16.045302v1.supplementary-material

Damas <- read.csv("Data/Damas_Ace2.csv", header = T)

Damas %>% 
  rename(DamasRank = Sort.by.predicted.susceptibility) %>%
  arrange(DamasRank) %>% 
  mutate_at("Species", ~.x %>% str_trim %>% str_replace_all(" ", "_")) ->
  
  Damas

affinisPredicted %>% left_join(Damas, by = c("Sp" = "Species")) %>%
  ggplot(aes(Rank, DamasRank)) + geom_point() + 
  geom_label_repel(aes(label = Sp))
