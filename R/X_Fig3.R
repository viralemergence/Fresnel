
source("~/Github/Fresnel/R/01_Fresnel Import.R")
source("~/Github/Fresnel/R/X_Thresholder.R")
source("~/Github/Fresnel/R/X_Scorekeeper.R")
source("~/Github/Fresnel/R/X_BetterEnsemble.R")

library(patchwork)
g2 + g1 + plot_annotation(tag_levels = 'A')
