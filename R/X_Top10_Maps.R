
assoc <- read_csv('~/Github/virionette/03_interaction_data/virionette.csv')

BatModels %>% as_tibble %>% mutate(InAssoc = (Sp %in% assoc$host_species)) -> BatModels

thresh <- read_csv("~/Github/Fresnel/BinaryWebsite.csv")

BatModels %>% left_join(thresh %>% select(Sp, Ensemble)) -> BatModels

#
library(fasterize)
library(rgdal)
library(raster)
library(sf)

iucn <- st_read(dsn = "C:/Users/cjcar/Dropbox/CurrentIUCN", layer='MAMMALS')

r <- disaggregate(getData("worldclim",var="alt",res=2.5)*0,2) # Make a blank raster

######################################################## TOP 50 DE NOVO BETACOV-

BatModels %>% filter(Betacov==0,
                     Ensemble %in% c("True +",
                                     "Suspected"),
                     InAssoc==FALSE) %>% 
              dplyr::pull(Sp) -> 
              unsampled.0

#unsampled.0 <- unsampled.0[1:50] # Top 50 predictions

### Some manual fixes
unsampled.0[unsampled.0=='Eptesicus nasutus'] <- 'Rhyneptesicus nasutus'
unsampled.0[unsampled.0=='Chaerephon leucogaster'] <- 'Chaerephon pumilus'
unsampled.0[unsampled.0=='Myonycteris angolensis'] <- 'Lissonycteris angolensis'
unsampled.0[unsampled.0=='Falsistrellus affinis'] <- 'Hypsugo affinis'
unsampled.0[unsampled.0=='Myotis oxygnathus'] <- 'Myotis blythii'
unsampled.0[unsampled.0=='Megaderma lyra'] <- 'Lyroderma lyra'
unsampled.0[unsampled.0=='Neoromicia somalicus'] <- 'Neoromicia somalica'
unsampled.0[unsampled.0=='Pteropus leucopterus'] <- 'Desmalopex leucopterus'
unsampled.0[unsampled.0=='Harpiocephalus mordax'] <- 'Harpiocephalus harpia'
unsampled.0[unsampled.0=='Pipistrellus deserti'] <- 'Pipistrellus kuhlii'
unsampled.0[unsampled.0=='Rhinolophus imaizumii'] <- 'Rhinolophus perditus'
unsampled.0[unsampled.0=='Paracoelops megalotis'] <- 'Hipposideros pomona'
unsampled.0[unsampled.0=='Arielulus aureocollaris'] <- 'Thainycteris aureocollaris'
unsampled.0[unsampled.0=='Rousettus bidens'] <- 'Boneia bidens'
unsampled.0[unsampled.0=='Hypsugo bodenheimeri'] <- 'Hypsugo ariel'
unsampled.0[unsampled.0=='Murina silvatica'] <- 'Murina ussuriensis'
unsampled.0[unsampled.0=='Scotoecus hindei'] <- 'Scotoecus hirundo'
unsampled.0[unsampled.0=='Scotoecus albigula'] <- 'Scotoecus hirundo'
unsampled.0[unsampled.0=='Murina grisea'] <- 'Harpiola grisea'
unsampled.0[unsampled.0=='Myotis hosonoi'] <- 'Myotis ikonnikovi' 
unsampled.0[unsampled.0=='Myotis yesoensis'] <- 'Myotis ikonnikovi' 
unsampled.0[unsampled.0=='Myotis ozensis'] <- 'Myotis ikonnikovi' 
unsampled.0[unsampled.0=='Eptesicus matroka'] <- 'Neoromicia matroka'
###

# Pipistrellus tenuis is unmapped

iucn.sub <- iucn[iucn$binomial %in% unsampled.0,] # Subset IUCN maps to the right ones

length(unique(iucn.sub$binomial)) # How many of the species made it in?

unsampled.0[!(unsampled.0 %in% iucn$binomial)] # Which names are bonked?

unsampled50.map <- fasterize(iucn.sub, r, fun="sum")

######################################################## TOP 50 IN-SAMPLE BETACOV-

BatModels %>% filter(Betacov==0,
                     Ensemble %in% c("True +",
                                     "Suspected"),
                     InAssoc==TRUE) %>% 
  dplyr::pull(Sp) -> 
  sampled.0

### Some manual fixes
sampled.0[sampled.0=='Pipistrellus subflavus'] <- 'Perimyotis subflavus'
### 

iucn.sub <- iucn[iucn$binomial %in% sampled.0,] # Subset IUCN maps to the right ones

length(unique(iucn.sub$binomial)) # How many of the species made it in?

sampled.0[!(sampled.0 %in% iucn$binomial)] # Which names are bonked?

sampled50.map <- fasterize(iucn.sub, r, fun="sum")

######################################################## KNOWN BETACOV+
BatModels %>% filter(Betacov == 1) %>% dplyr::pull(Sp)-> true.1
### Some manual fixes
true.1[true.1=='Myotis ricketti'] <- 'Myotis pilosus'
true.1[true.1=='Artibeus phaeotis'] <- 'Dermanura phaeotis'
true.1[true.1=='Hipposideros commersoni'] <- 'Macronycteris commersoni'
true.1[true.1=='Rhinolophus monoceros'] <- 'Rhinolophus pusillus'
true.1[true.1=='Rhinolophus cornutus'] <- 'Rhinolophus pusillus'

###
iucn.sub <- iucn[iucn$binomial %in% true.1,] # Subset IUCN maps to the right ones
length(unique(iucn.sub$binomial)) # How many of the species made it in?
true.1[!(true.1 %in% iucn$binomial)] # Which names are bonked?
true1.map <- fasterize(iucn.sub, r, fun="sum")

#outline <- rasterToPolygons(r, dissolve=TRUE)

library(maps)
library(rasterVis)
library(RColorBrewer)

par(mfrow=c(3,1))

true1.map <- sum(true1.map,r,na.rm=TRUE)
true1.map <- true1.map + r

sampled50.map <- sum(sampled50.map,r,na.rm=TRUE)
sampled50.map <- sampled50.map + r

unsampled50.map <- sum(unsampled50.map, r, na.rm=TRUE)
unsampled50.map <- sum(unsampled50.map, r)

maps <- stack(true1.map,
              sampled50.map,
              unsampled50.map)

#outline <- rasterToPolygons(r, dissolve=TRUE)

mycolors <- colorRampPalette(rev(brewer.pal(10,"Spectral")))(21)
mycolors[1] <- "#C0C0C0"

rasterVis::levelplot(maps,  
                     col.regions = mycolors,
                     #at = seq(0, 20, 1),
                     alpha = 0.5, 
                     scales=list(alternating=FALSE),
                     par.strip.text=list(cex=0),
                     xlab = NULL, ylab = NULL,
                     maxpixels = 5e6) #+

