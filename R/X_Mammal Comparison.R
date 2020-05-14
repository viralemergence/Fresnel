
# Run Fresnel import first

assoc <- read.csv("~/Github/virionette/03_interaction_data/virionette.csv")
assoc %>% select(host_order, host_species) %>% 
  rename(Sp = host_species) %>%
  unique() %>% left_join(NonBatModels) %>% 
  filter(!(host_order == 'Chiroptera')) -> NonBats

View(NonBats)

library(WVPlots)
NonBats$Betacov <- factor(as.character(NonBats$Betacov))
PairPlot(NonBats, 
         colnames(NonBats)[4:11], 
         "Model agreement", 
         group_var = "Betacov",
         alpha = 0.35) + 
  theme_bw() 

# My best guess:
# R.Alb, R.Dal1, R.Far1, R.Po1, R.Po3?