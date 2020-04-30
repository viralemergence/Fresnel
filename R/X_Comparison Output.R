
# X_Comparison ####

AlberyPredicted %>% rename(AlberyRank = Rank) %>%
  full_join(CarlsonPredicted %>% rename(CarlsonRank = Rank), 
            by = c("Sp" = "Host_species")) ->
  
  FullPredictions
