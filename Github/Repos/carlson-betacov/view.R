library(tidyverse)

bart.c <- read.csv("CarlsonBartCitations.csv")
dart.c <- read.csv("CarlsonDartCitations.csv")
bart.u <- read.csv("CarlsonBartUncorrected.csv")
dart.u <- read.csv("CarlsonDartUncorrected.csv")

View(bart.c %>% filter(betacov==0))
View(dart.c %>% filter(betacov==0))
View(bart.u %>% filter(betacov==0))
View(dart.u %>% filter(betacov==0))
