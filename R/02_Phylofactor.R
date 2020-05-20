## phylofactor betacov ensemble
## danbeck@iu.edu
# 05162020

## clean environment & plots

## source helper files for gpf

source('R/02a_Phylofactor helper.R')

#detach(package:dplyr)

## packages
library(ape)
library(phylofactor)
library(tidyverse)
library(data.table)
library(ggtree)
library(plyr)
library(caper)
library(patchwork)
library(ggpubr)

## load ensemble data

batin = BatModels_IS
batout = BatModels_OS

batin <- read.csv("Cleaned Files/BatModels_IS.csv")
batout <- read.csv("Cleaned Files/BatModels_OS.csv")

## mammals too

mamin = NonBatModels_IS
mamout = NonBatModels_OS

mamin2 <- read.csv("Cleaned Files/NonBatModels_IS.csv")
mamout2 <- read.csv("Cleaned Files/NonBatModels_OS.csv")

## fix mamout
mamout = mamout[!is.na(mamout$PropRank), ]

## load mammal supertree

tree = readRDS(paste0(here::here(), 
                      "/Github/Repos/virionette/04_predictors/Full Supertree.rds"))

## extract node 6580 (Chiroptera)
btree = extract.clade(tree, 6580)
length(btree$tip.label)

## make tree names
batin$treenames = gsub(' ', '_', batin$Sp)
batout$treenames = gsub(' ', '_', batout$Sp)
mamin$treenames = gsub(' ', '_', mamin$Sp)
mamout$treenames = gsub(' ', '_', mamout$Sp)

## load in taxonomy

taxonomy = read.csv(paste0("Github/Repos/becker-betacov/", 
                           'mammal taxonomy.csv'), 
                    header = T)

taxonomy$X = NULL
taxonomy$Sp = NULL

## trim to genus
taxonomy = taxonomy[!duplicated(taxonomy$hGenus), ]

## get genus
batin$hGenus = sapply(strsplit(batin$treenames, '_'), function(x) x[1])
batout$hGenus = sapply(strsplit(batout$treenames, '_'), function(x) x[1])
mamin$hGenus = sapply(strsplit(mamin$treenames, '_'), function(x) x[1])
mamout$hGenus = sapply(strsplit(mamout$treenames, '_'), function(x) x[1])

## merge
batin = merge(batin, taxonomy, by = 'hGenus', all.x = T)
batout = merge(batout, taxonomy, by = 'hGenus', all.x = T)

## fix batout
batout$hFamily = as.character(batout$hFamily)
batout$hFamily2 = revalue(batout$hGenus, 
                          c('Aproteles' = 'Pteropodidae', 
                            'Paracoelops' = 'Hipposideridae'))
batout$hFamily = ifelse(is.na(batout$hFamily), batout$hFamily2, batout$hFamily)
batout$hFamily2 = NULL
batout$hOrder = 'Chiroptera'

## mammal missing
test = merge(mamin, taxonomy, by = 'hGenus', all.x = T); table(is.na(test$hFamily))
set = test[is.na(test$hFamily), ]
set = set[!duplicated(set$hGenus), ]
set = set[c('hGenus', 'hFamily', 'hOrder')]

## revalue
set$hFamily2 = revalue(set$hGenus, 
                       c('Balaenoptera' = 'Balaenopteridae', 
                         'Caluromys' = 'Didelphidae', 
                         'Camelus' = 'Camelidae', 
                         'Chironectes' = 'Didelphidae', 
                         'Delphinapterus' = 'Monodontidae', 
                         'Delphinus' = 'Delphinidae', 
                         'Didelphis' = 'Didelphidae', 
                         'Dorcopsis' = 'Macropodidae', 
                         'Eschrichtius' = 'Eschrichtiidae', 
                         'Galerella' = 'Herpestidae', 
                         'Globicephala' = 'Delphinidae', 
                         'Isoodon' = 'Peramelidae', 
                         'Lama' = 'Camelidae', 
                         'Macropus' = 'Macropodidae', 
                         'Marmosa' = 'Didelphidae', 
                         'Metachirus' = 'Didelphidae', 
                         'Orcinus' = 'Delphinidae', 
                         'Perameles' = 'Peramelidae', 
                         'Philander' = 'Didelphidae', 
                         'Phocoena' = 'Phocoenidae', 
                         'Physeter' = 'Physeteridae', 
                         'Stenella' = 'Delphinidae', 
                         'Taurotragus' = 'Bovidae', 
                         'Trichosurus' = 'Phalangeridae', 
                         'Tursiops' = 'Tursiops'))

## make order
set$hOrder2 = revalue(set$hFamily2, 
                      c('Balaenopteridae' = 'Artiodactyla', 
                        'Didelphidae' = 'Didelphimorphia', 
                        'Camelidae' = 'Artiodactyla', 
                        'Monodontidae' = 'Artiodactyla', 
                        'Delphinidae' = 'Artiodactyla', 
                        'Macropodidae' = 'Diprotodontia', 
                        'Eschrichtiidae' = 'Artiodactyla', 
                        'Herpestidae' = 'Carnivora', 
                        'Peramelidae' = 'Peramelemorphia', 
                        'Phocoenidae' = 'Artiodactyla', 
                        'Physeteridae' = 'Artiodactyla', 
                        'Bovidae' = 'Artiodactyla', 
                        'Phalangeridae' = 'Diprotodontia', 
                        'Tursiops' = 'Artiodactyla'))

## trim
set$hFamily = NULL
set$hOrder = NULL

## merge into taxonomy
test = merge(taxonomy, set, by = 'hGenus', all = T)

## fix
test$hFamily = as.character(test$hFamily)
test$hOrder = as.character(test$hOrder)
test$hFamily = ifelse(is.na(test$hFamily), test$hFamily2, test$hFamily)
test$hOrder = ifelse(is.na(test$hOrder), test$hOrder2, test$hOrder)
test$hFamily2 = NULL
test$hOrder2 = NULL

## new taxonomy
taxonomy = test
rm(test, set)

## merge taxa into mammals
mamin = merge(mamin, taxonomy, by = 'hGenus', all.x = T)
mamout = merge(mamout, taxonomy, by = 'hGenus', all.x = T)

## make taxonomy within each file
batin$taxonomy = with(batin, paste(hOrder, hFamily, hGenus, treenames, sep = '; '))
batout$taxonomy = with(batout, paste(hOrder, hFamily, hGenus, treenames, sep = '; '))
mamin$taxonomy = with(mamin, paste(hOrder, hFamily, hGenus, treenames, sep = '; '))
mamout$taxonomy = with(mamout, paste(hOrder, hFamily, hGenus, treenames, sep = '; '))

## trim each to tree
btree_in = keep.tip(btree, batin$treenames)
btree_out = keep.tip(btree, btree$tip.label[btree$tip.label%in%batout$treenames])
tree_in = keep.tip(tree, mamin$treenames)
tree_out = keep.tip(tree, mamout$treenames)

## merge into phylogeny order
batin = batin[match(btree_in$tip.label, batin$treenames), ]
batout = batout[match(btree_out$tip.label, batout$treenames), ]
mamin = mamin[match(tree_in$tip.label, mamin$treenames), ]
mamout = mamout[match(tree_out$tip.label, mamout$treenames), ]

## caper objects
bdata_in = comparative.data(phy = btree_in, data = batin, names.col = treenames, vcv = T, na.omit = F, warn.dropped = T)
bdata_out = comparative.data(phy = btree_out, data = batout, names.col = treenames, vcv = T, na.omit = F, warn.dropped = T)
data_in = comparative.data(phy = tree_in, data = mamin, names.col = treenames, vcv = T, na.omit = F, warn.dropped = T)
data_out = comparative.data(phy = tree_out, data = mamout, names.col = treenames, vcv = T, na.omit = F, warn.dropped = T)

## clean up
# rm(batin, batout, btree, mamin, mamout, tree, tree_in, tree_out, btree_in, btree_out)

## rename
bdata_in$data$treenames = rownames(bdata_in$data)
bdata_out$data$treenames = rownames(bdata_out$data)
data_in$data$treenames = rownames(data_in$data)
data_out$data$treenames = rownames(data_out$data)

## add Species
bdata_in$data$Species = rownames(bdata_in$data)
bdata_out$data$Species = rownames(bdata_out$data)
data_in$data$Species = rownames(data_in$data)
data_out$data$Species = rownames(data_out$data)

# Bat Phylofactor ####

## bat phylofactor, in sample
set.seed(1)
batin_pf = gpf(Data = bdata_in$data, 
               tree = bdata_in$phy, 
               frmla.phylo = PropRank~phylo, 
               family = gaussian, algorithm = 'phylo', 
               nfactors = 2)

## results
batin_results = pfsum(batin_pf)

## split data from results
batin_data = batin_results$set
batin_results = batin_results$results

## revalue
batin_results$taxa = revalue(batin_results$taxa, 
                             c('Natalidae, Mystacinidae, Mormoopidae, Phyllostomidae, Molossidae, Vespertilionidae' = 'Yangochiroptera'))

## bat phylofactor, out of sample
set.seed(1)
batout_pf = gpf(Data = bdata_out$data, 
                tree = bdata_out$phy, 
                frmla.phylo = PropRank~phylo, 
                family = gaussian, 
                algorithm = 'phylo', 
                nfactors = 8)

## results
batout_results = pfsum(batout_pf)

## split data from results
batout_data = batout_results$set
batout_results = batout_results$results

# Mammal Phylofactor ####

Mammal <- T

if(Mammal){
  
  ## mammal phylofactor, in sample
  set.seed(1)
  mamin_pf = gpf(Data = data_in$data, tree = data_in$phy, 
                 frmla.phylo = PropRank~phylo, 
                 family = gaussian, algorithm = 'phylo', nfactors = 5)
  
  ## results
  mamin_results = pfsum(mamin_pf)
  
  ## split data from results
  mamin_data = mamin_results$set
  mamin_results = mamin_results$results
  
  ## mammal phylofactor, out of sample
  set.seed(1)
  mamout_pf = gpf(Data = data_out$data, tree = data_out$phy, 
                  frmla.phylo = PropRank~phylo, 
                  family = gaussian, algorithm = 'phylo', nfactors = 15)
  
  ## results
  mamout_results = pfsum(mamout_pf)
  
  ## split data from results
  mamout_data = mamout_results$set
  mamout_results = mamout_results$results
  
}

## visualize
gg = ggtree(bdata_in$phy, 
            size = 0.15, 
            layout = 'circular') + 
  theme(legend.position = "none")


## add clades
for(i in 1:nrow(batin_results)){
  
  gg = gg+
    geom_hilight(node = batin_results$node[i], 
                 alpha = 0.5, 
                 fill = ifelse(batin_results$clade<
                                 batin_results$other, pcols[2], pcols[1])[i])+
    geom_cladelabel(node = batin_results$node[i], 
                    label = batin_results$factor[i], 
                    offset = 25, 
                    offset.text = 20)
}

## visualize
p1 = gg+
  
  ## title
  #ggtitle('bat rank, in-sample')+
  #theme(plot.title = element_text(hjust = 0.5))+
  
  ## add predictions
  geom_segment(data = segfun(bdata_in, 25), 
               aes(x = x, y = y, xend = xend, yend = yend, colour = Betacov), size = 0.5)+
  scale_colour_manual(values = c('grey70', 'black'))#+

# ## add clade
# geom_cladelabel(node = batin_results$node[1], 
#                 label = batin_results$factor[1], 
#                 offset = 25, 
#                 offset.text = 20)

## visualize
gg = ggtree(bdata_out$phy, 
            size = 0.1, 
            layout = 'circular') + 
  theme(legend.position = "none")

## add clades
for(i in 1:nrow(batout_results)){
  
  gg = gg+
    geom_hilight(node = batout_results$node[i], 
                 alpha = 0.5, 
                 fill = ifelse(batout_results$clade<
                                 batout_results$other, pcols[2], pcols[1])[i])+
    geom_cladelabel(node = batout_results$node[i], 
                    label = batout_results$factor[i], 
                    offset = 25, 
                    offset.text = 20)
}

## finish
p2 = gg+
  
  ## title
  #ggtitle('bat rank, out-of-sample')+
  #theme(plot.title = element_text(hjust = 0.5))+
  
  ## add predictions
  geom_segment(data = segfun(bdata_out, 25), 
               aes(x = x, y = y, xend = xend, yend = yend, colour = Betacov), size = 0.15)+
  scale_colour_manual(values = c('grey70', 'black'))#+

# ## add clades
# geom_cladelabel(node = batout_results$node[1], 
#                 label = 'Noctilionoidea', 
#                 offset = 25, 
#                 offset.text = 20, 
#                 hjust = 0.5)+
# geom_cladelabel(node = batout_results$node[2], 
#                 label = 'Emballonuridae', 
#                 offset = 25, 
#                 offset.text = 20, 
#                 hjust = 0)+
# geom_cladelabel(node = batout_results$node[5], 
#                 label = 'Rousettinae\nEpomophorinae\nCynopterinae', 
#                 offset = 25, 
#                 offset.text = 10, 
#                 hjust = 0, 
#                 extend = 10)+
# geom_cladelabel(node = batout_results$node[7], 
#                 label = 'Rhinolophidae', 
#                 offset = 25, 
#                 offset.text = 20, 
#                 hjust = 0.5)

## visualize
gg = ggtree(data_in$phy, 
            size = 0.2, 
            layout = 'circular') + 
  theme(legend.position = "none")

## add clades
for(i in 1:nrow(mamin_results)){
  
  gg = gg+
    geom_hilight(node = mamin_results$node[i], 
                 alpha = 0.5, 
                 fill = ifelse(mamin_results$clade<
                                 mamin_results$other, pcols[2], pcols[1])[i])+
    geom_cladelabel(node = mamin_results$node[i], 
                    label = mamin_results$factor[i], 
                    offset = 55, 
                    offset.text = 50)
}

## finish
p3 = gg+
  
  ## title
  #ggtitle('mammal rank, in-sample')+
  #theme(plot.title = element_text(hjust = 0.5))+
  
  ## add predictions
  geom_segment(data = segfun(data_in, 55), 
               aes(x = x, y = y, xend = xend, yend = yend, colour = Betacov), size = 0.25)+
  scale_colour_manual(values = c('grey70', 'black'))

## visualize
gg = ggtree(data_out$phy, 
            size = 0.05, 
            layout = 'circular') + 
  theme(legend.position = "none")

mamout_results %<>% arrange(desc(tips))

i <- 1

## add clades
for(i in i:nrow(mamout_results)){
  
  gg = gg+
    geom_hilight(node = mamout_results$node[i], 
                 alpha = ifelse(mamout_results$clade[i]<
                                  mamout_results$other[i], 0.5, 0.3)[1], 
                 fill = ifelse(mamout_results$clade<
                                 mamout_results$other, pcols[2], pcols[1])[i]) +
    
    geom_cladelabel(node = mamout_results$node[i], 
                    label = mamout_results$factor[i], 
                    offset = 50, 
                    offset.text = 45)
  
  # plot(gg)
  
}

## finish
p4 = gg+
  
  ## title
  #ggtitle('mammal rank, out-of-sample')+
  #theme(plot.title = element_text(hjust = 0.5))+
  
  ## add predictions
  geom_segment(data = segfun(data_out, 50), 
               aes(x = x, y = y, xend = xend, yend = yend, colour = Betacov), 
               size = 0.05)+
  scale_colour_manual(values = c('grey70', 'black'))

## write

png("Figures/Phylofactor_pred ensemble.png", width = 7, height = 6.5, units = "in", res = 600)
ggpubr::ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2, 
                  labels = c('bat rank, in-sample', 
                             'bat rank, out-of-sample', 
                             'mammal rank, in-sample', 
                             'mammal rank, out-of-sample'), 
                  hjust = c(-0.85, -0.65, -0.5, -0.4), 
                  vjust = c(1.75, 1.75, 1, 1), 
                  font.label = list(size = 11, face = 'plain'))
#p1+p2+p3+p4+plot_layout(ncol = 2)
dev.off()

## assign scale
batin_results$scale = 'bats'
batout_results$scale = 'bats'
mamin_results$scale = 'mammals'
mamout_results$scale = 'mammals'

## assign in/out
batin_results$sample = 'in'
batout_results$sample = 'out'
mamin_results$sample = 'in'
mamout_results$sample = 'out'

## combine
results = rbind.data.frame(batin_results, 
                           batout_results, 
                           mamin_results, 
                           mamout_results)
results = results[c('scale', 'sample', 'factor', 'taxa', 'tips', 'clade', 'other')]

## export

write.csv(results, 'Output Files/ensemble phylofactor results.csv')

## Albery Rhinolophus sp sharing Phylofactor ####

bat_ra = readRDS('Github/Repos/albery-betacov/Intermediate/AlberyPredictedBats_R_affinis.rds')
bat_rm = readRDS('Github/Repos/albery-betacov/Intermediate/AlberyPredictedBats_R_malayanus.rds')

## taxonomy
bat_ra$taxonomy = with(bat_ra, paste(hOrder, hFamily, hGenus, Sp, sep = '; '))
bat_rm$taxonomy = with(bat_rm, paste(hOrder, hFamily, hGenus, Sp, sep = '; '))

## Species
bat_ra$Species = bat_ra$Sp
bat_rm$Species = bat_rm$Sp

## trim tree
ra_tree = keep.tip(tree, bat_ra$Sp)
rm_tree = keep.tip(tree, bat_rm$Sp)

## merge into phylogeny order
bat_ra = bat_ra[match(ra_tree$tip.label, bat_ra$Sp), ]
bat_rm = bat_rm[match(rm_tree$tip.label, bat_rm$Sp), ]

## caper objects
bat_ra = comparative.data(phy = ra_tree, data = bat_ra, names.col = Sp, vcv = T, na.omit = F, warn.dropped = T)
bat_rm = comparative.data(phy = rm_tree, data = bat_rm, names.col = Sp, vcv = T, na.omit = F, warn.dropped = T)

## treenames
bat_ra$data$treenames = rownames(bat_ra$data)
bat_rm$data$treenames = rownames(bat_rm$data)

## split into mammals and bats
mam_ra = bat_ra[-which(bat_ra$data$hOrder == 'Chiroptera'), ]
mam_rm = bat_rm[-which(bat_rm$data$hOrder == 'Chiroptera'), ]
bat_ra = bat_ra[which(bat_ra$data$hOrder == 'Chiroptera'), ]
bat_rm = bat_rm[which(bat_rm$data$hOrder == 'Chiroptera'), ]

## ra phylofactor bats
set.seed(1)
batra_pf = gpf(Data = bat_ra$data, tree = bat_ra$phy, 
               frmla.phylo = Rank~phylo, 
               family = gaussian, algorithm = 'phylo', nfactors = 11)

## results
batra_results = pfsum(batra_pf)

## split data from results
batra_data = batra_results$set
batra_results = batra_results$results

## rm phylofactor
set.seed(1)
batrm_pf = gpf(Data = bat_rm$data, tree = bat_rm$phy, 
               frmla.phylo = Rank~phylo, 
               family = gaussian, algorithm = 'phylo', nfactors = 10)

## results
batrm_results = pfsum(batrm_pf)

## split data from results
batrm_data = batrm_results$set
batrm_results = batrm_results$results

## mammal ra pf
set.seed(1)
mamra_pf = gpf(Data = mam_ra$data, tree = mam_ra$phy, 
               frmla.phylo = Rank~phylo, 
               family = gaussian, algorithm = 'phylo', nfactors = 11)

## results
mamra_results = pfsum(mamra_pf)

## split data from results
mamra_data = mamra_results$set
mamra_results = mamra_results$results

## mammal rm pf
set.seed(1)
mamrm_pf = gpf(Data = mam_rm$data, tree = mam_rm$phy, 
               frmla.phylo = Rank~phylo, 
               family = gaussian, algorithm = 'phylo', nfactors = 11)

## results
mamrm_results = pfsum(mamrm_pf)

## split data from results
mamrm_data = mamrm_results$set
mamrm_results = mamrm_results$results

## visualize
gg = ggtree(bat_ra$phy, 
            size = 0.1, 
            layout = 'circular') + 
  theme(legend.position = "none")

## add clades
for(i in 1:nrow(batra_results[-which(batra_results$tips == 1), ])){
  
  gg = gg+
    geom_hilight(node = batra_results[-which(batra_results$tips == 1), ]$node[i], 
                 alpha = 0.5, 
                 fill = ifelse(batra_results[-which(batra_results$tips == 1), ]$clade<
                                 batra_results[-which(batra_results$tips == 1), ]$other, pcols[2], pcols[1])[i])+
    geom_cladelabel(node = batra_results[-which(batra_results$tips == 1), ]$node[i], 
                    label = batra_results[-which(batra_results$tips == 1), ]$factor[i], 
                    offset = 25, 
                    offset.text = 20)
}

## visualize
p1 = gg+
  
  ## add predictions
  geom_segment(data = segfun2(bat_ra, 15), 
               aes(x = x, y = y, xend = xend, yend = yend), size = 0.05)

## visualize rm
gg = ggtree(bat_rm$phy, 
            size = 0.1, 
            layout = 'circular')

## add clades
for(i in 1:nrow(batrm_results[-which(batrm_results$tips == 1), ])){
  
  gg = gg+
    geom_hilight(node = batrm_results[-which(batrm_results$tips == 1), ]$node[i], 
                 alpha = 0.5, 
                 fill = ifelse(batrm_results[-which(batrm_results$tips == 1), ]$clade<
                                 batrm_results[-which(batrm_results$tips == 1), ]$other, pcols[2], pcols[1])[i])+
    geom_cladelabel(node = batrm_results[-which(batrm_results$tips == 1), ]$node[i], 
                    label = batrm_results[-which(batrm_results$tips == 1), ]$factor[i], 
                    offset = 25, 
                    offset.text = 20)
}

## visualize
p2 = gg+
  
  ## add predictions
  geom_segment(data = segfun2(bat_rm, 15), 
               aes(x = x, y = y, xend = xend, yend = yend), size = 0.05)

## mammal ra
gg = ggtree(mam_ra$phy, 
            size = 0.1, 
            layout = 'circular')

## add clades
for(i in 1:nrow(mamra_results[-which(mamra_results$tips == 1), ])){
  
  gg = gg+
    geom_hilight(node = mamra_results$node[i], 
                 alpha = 0.5, 
                 fill = ifelse(mamra_results$clade<
                                 mamra_results$other, pcols[2], pcols[1])[i])+
    geom_cladelabel(node = mamra_results$node[i], 
                    label = mamra_results$factor[i], 
                    offset = 25, 
                    offset.text = 20)
}

## visualize
p3 = gg+
  
  ## add predictions
  geom_segment(data = segfun2(mam_ra, 15), 
               aes(x = x, y = y, xend = xend, yend = yend), size = 0.025)

## mammal rm
gg = ggtree(mam_rm$phy, 
            size = 0.1, 
            layout = 'circular')

## add clades
for(i in 1:nrow(mamrm_results[-which(mamrm_results$tips == 1), ])){
  
  gg = gg+
    geom_hilight(node = mamrm_results$node[i], 
                 alpha = 0.5, 
                 fill = ifelse(mamrm_results$clade<
                                 mamrm_results$other, pcols[2], pcols[1])[i])+
    geom_cladelabel(node = mamrm_results$node[i], 
                    label = mamrm_results$factor[i], 
                    offset = 25, 
                    offset.text = 20)
}

## visualize
p4 = gg+
  
  ## add predictions
  geom_segment(data = segfun2(mam_rm, 15), 
               aes(x = x, y = y, xend = xend, yend = yend), size = 0.025)

## write

png("Figures/Phylofactor_pred bat sharing.png", width = 7, height = 6.5, units = "in", res = 600)
ggpubr::ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2, 
                  labels = c('bat rank, R. affinis', 
                             'bat rank, R. malayanus', 
                             'mammal rank, R. affinis', 
                             'mammal rank, R. malayanus'), 
                  hjust = c(-0.85, -0.65, -0.5, -0.4), 
                  vjust = c(1.75, 1.75, 1, 1), 
                  font.label = list(size = 11, face = 'plain'))
#p1+p2+p3+p4+plot_layout(ncol = 2)
dev.off()

## assign scale
batra_results$scale = 'bats'
batrm_results$scale = 'bats'
mamra_results$scale = 'mammals'
mamrm_results$scale = 'mammals'

## species
batra_results$share = 'R. affinis'
batrm_results$share = 'R. malayanus'
mamra_results$share = 'R. affinis'
mamrm_results$share = 'R. malayanus'

## combine
results = rbind.data.frame(batra_results, 
                           batrm_results, 
                           mamra_results, 
                           mamrm_results)

results = results[c('scale', 'share', 'factor', 'taxa', 'tips', 'clade', 'other')]

## export

write.csv(results, 'Output Files/bat sharing phylofactor results.csv')
