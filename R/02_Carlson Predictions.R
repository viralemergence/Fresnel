
# 2_Carlson Predictions #####

CarlsonPredicted %>% rename_all(CamelConvert) %>% 
  mutate_at("Host_species", ~.x %>% str_trim %>% str_replace_all(" ", "_")) ->
  
  CarlsonPredicted
