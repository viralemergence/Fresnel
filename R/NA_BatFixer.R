
traits <- read_csv('~/Github/virionette/04_predictors/Han-BatTraits.csv')

BatsVsOther <- read_csv("~/Github/Fresnel/Data/BatsVsOther.csv")[,-1] %>%
  rename_all(CamelConvert) %>% 
  mutate_at("Bats", ~.x %>% as.character %>% CamelConvert)


rbind(data.frame(name = traits$Pan[!(traits$Pan %in% BatsVsOther$Tree)],
           Bats = "Bats"),
      BatsVsOther %>% rename(name = Tree)) -> BatsVsOther

write_csv(BatsVsOther, '~/Github/Fresnel/Data/BatsVsOther.csv')
nrow(BatsVsOther)
nrow(unique(BatsVsOther))
