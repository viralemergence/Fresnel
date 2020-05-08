
assoc <- read_csv('./cleanbats_betacov/clean data/BatCoV-assoc_compatible.csv')

Models %>% mutate(InAssoc = (Sp %in% assoc$host_species)) -> Models

Models %>% filter(InAssoc==TRUE) %>%arrange(PropRank)

