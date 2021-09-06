## phylofactor betacov data & ensemble 1
## danbeck@ou.edu

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
#batin <- read.csv("Cleaned Files/BatModels_IS.csv")
#batout <- read.csv("Cleaned Files/BatModels_OS.csv")

## restrict BatModels_OS to out-of-sample
batout=batout[batout$InSample==0,]

## load mammal supertree
tree = readRDS(paste0(here::here(), 
                      "/Github/Repos/virionette/04_predictors/Full Supertree.rds"))

## extract node 6580 (Chiroptera)
btree = extract.clade(tree, 6580)
length(btree$tip.label)

## ladderize
btree=ladderize(btree)

## make bats
bats=rbind.data.frame(batin[intersect(names(batin),names(batout))],
                 batout[intersect(names(batin),names(batout))])
bats=bats[c("Sp","Betacov","PropRank","InSample")]

## make tree names
bats$treenames = gsub(' ', '_', bats$Sp)

## load in taxonomy
taxonomy = read.csv(paste0("Github/Repos/becker-betacov/", 
                           'mammal taxonomy.csv'), 
                    header = T)
taxonomy$X = NULL
taxonomy$Sp = NULL

## trim to genus
taxonomy = taxonomy[!duplicated(taxonomy$hGenus), ]

## get genus
bats$hGenus = sapply(strsplit(bats$treenames, '_'), function(x) x[1])

## merge
bats = merge(bats, taxonomy, by = 'hGenus', all.x = T)

## fix bats
bats$hFamily = as.character(bats$hFamily)
bats$hFamily2 = revalue(bats$hGenus, 
                        c('Aproteles' = 'Pteropodidae', 
                          'Paracoelops' = 'Hipposideridae'))
bats$hFamily = ifelse(is.na(bats$hFamily), bats$hFamily2, bats$hFamily)
bats$hFamily2 = NULL

## make taxonomy 
bats$taxonomy = with(bats, paste(hOrder, hFamily, hGenus, treenames, sep = '; '))

## split into batin and batout
batin=bats[bats$InSample==1,]
batout=bats[bats$InSample==0,]

## trim each to tree
btree_all = keep.tip(btree, intersect(btree$tip.label,bats$treenames))
btree_in = keep.tip(btree, intersect(btree$tip.label,batin$treenames))
btree_out = keep.tip(btree, intersect(btree$tip.label,batout$treenames))

## merge into phylogeny order
bats = bats[match(btree_all$tip.label, bats$treenames), ]
batin = batin[match(btree_in$tip.label, batin$treenames), ]
batout = batout[match(btree_out$tip.label, batout$treenames), ]

## caper objects
bdata_all = comparative.data(phy = btree_all, data = bats, names.col = treenames, vcv = T, na.omit = F, warn.dropped = T)
bdata_in = comparative.data(phy = btree_in, data = batin, names.col = treenames, vcv = T, na.omit = F, warn.dropped = T)
bdata_out = comparative.data(phy = btree_out, data = batout, names.col = treenames, vcv = T, na.omit = F, warn.dropped = T)

## rename
bdata_all$data$treenames = rownames(bdata_all$data)
bdata_in$data$treenames = rownames(bdata_in$data)
bdata_out$data$treenames = rownames(bdata_out$data)

## add Species
bdata_all$data$Species = rownames(bdata_all$data)
bdata_in$data$Species = rownames(bdata_in$data)
bdata_out$data$Species = rownames(bdata_out$data)

# Bat Phylofactor ####

## bat phylofactor, raw data
set.seed(1)
batall_pf = gpf(Data = bdata_all$data, 
                tree = bdata_all$phy, 
                frmla.phylo = Betacov~phylo, 
                family = binomial, algorithm = 'phylo', 
                nfactors = 4)

## results
batall_results = pfsum(batall_pf)

## split data from results
batall_data = batall_results$set
batall_results = batall_results$results

## bat phylofactor, in sample
set.seed(1)
batin_pf = gpf(Data = bdata_in$data, 
               tree = bdata_in$phy, 
               frmla.phylo = PropRank~phylo, 
               family = gaussian, algorithm = 'phylo', 
               nfactors = 4)

## results
batin_results = pfsum(batin_pf)

## split data from results
batin_data = batin_results$set
batin_results = batin_results$results

## bat phylofactor, out of sample
set.seed(1)
batout_pf = gpf(Data = bdata_out$data, 
                tree = bdata_out$phy, 
                frmla.phylo = PropRank~phylo, 
                family = gaussian, 
                algorithm = 'phylo', 
                nfactors = 10)

## results
batout_results = pfsum(batout_pf)

## split data from results
batout_data = batout_results$set
batout_results = batout_results$results

## visualize raw patterns
gg = ggtree(bdata_all$phy, 
            size = 0.1, 
            layout = 'circular') + 
  theme(legend.position = "none")

## add clades
for(i in 1:nrow(batall_results)){
  
  gg = gg+
    geom_hilight(node = batall_results$node[i], 
                 alpha = 0.5, 
                 fill = ifelse(batall_results$clade>
                                 batall_results$other, 
                               pcols[2], pcols[1])[i])+
    geom_cladelabel(node = batall_results$node[i], 
                    label = batall_results$factor[i], 
                    offset = 25, 
                    offset.text = 20)
}

## set x max
plus=5

## make bat base
base=gg$data

## tips only
base=base[which(base$isTip==T),]

## make data frame
preds=data.frame(x=base$x,
                 y=base$y,
                 yend=base$y,
                 xend=ifelse(bdata_all$data$Betacov==0,unique(base$x)[1],
                             unique(base$x)[1]+plus),
                 Betacov=factor(bdata_all$data$Betacov))

## visualize
p1 = gg+
  
  ## add predictions
  geom_segment(data = preds, 
               aes(x = x, y = y, xend = xend, yend = yend, colour = Betacov), size = 0.5)+
  scale_colour_manual(values = c('grey70', 'black'))

## visualize in-sample
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
                                 batin_results$other, 
                               pcols[2], pcols[1])[i])+
    geom_cladelabel(node = batin_results$node[i], 
                    label = batin_results$factor[i], 
                    offset = 25, 
                    offset.text = 20)
}

## visualize
p2 = gg+
  
  ## add predictions
  geom_segment(data = segfun(bdata_in, 25), 
               aes(x = x, y = y, xend = xend, yend = yend, colour = Betacov), size = 0.5)+
  scale_colour_manual(values = c('grey70', 'black'))#+

## visualize out-sample
gg = ggtree(bdata_out$phy, 
            size = 0.1, 
            layout = 'circular') + 
  theme(legend.position = "none")

## add clades
for(i in 1:(nrow(batout_results)-1)){
  
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

## add final clade with offset
for(i in nrow(batout_results):nrow(batout_results)){
  
  gg = gg+
    geom_hilight(node = batout_results$node[i], 
                 alpha = 0.5, 
                 fill = ifelse(batout_results$clade<
                                 batout_results$other, pcols[2], pcols[1])[i])+
    geom_cladelabel(node = batout_results$node[i], 
                    label = batout_results$factor[i], 
                    offset = 25, 
                    offset.text = 10)
}

## finish
p3 = gg+
  
  ## add predictions
  geom_segment(data = segfun(bdata_out, 25), 
               aes(x = x, y = y, xend = xend, yend = yend, colour = Betacov), size = 0.15)+
  scale_colour_manual(values = c('grey70', 'black'))#+

## save each as panels for manual input
png("Figures/Figure 2_raw.png",width=4,height=4,units="in",res=300)
p1
dev.off()

## save as PDF
pdf("Figures/Figure 2_raw.pdf",width=4,height=4)
p1
dev.off()

## save each as panels for manual input
png("Figures/Figure 2_in.png",width=4,height=4,units="in",res=300)
p2
dev.off()

## pdf
pdf("Figures/Figure 2_in.pdf",width=4,height=4)
p2
dev.off()

## save each as panels for manual input
png("Figures/Figure 2_out.png",width=4,height=4,units="in",res=300)
p3
dev.off()

## pdf
pdf("Figures/Figure 2_out.pdf",width=4,height=4)
p3
dev.off()

## assign data set
batall_results$sample = 'raw'
batin_results$sample = 'in'
batout_results$sample = 'out'

## combine
results = rbind.data.frame(batall_results,
                           batin_results, 
                           batout_results)
results = results[c('sample', 'factor', 'taxa', 'tips', 'clade', 'other')]

## export

write.csv(results, 'Output Files/ensemble phylofactor results.csv')