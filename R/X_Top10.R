
assoc <- read_csv('~/Github/virionette/03_interaction_data/virionette.csv')

BatModels %>% as_tibble %>% mutate(InAssoc = (Sp %in% assoc$host_species)) -> BatModels
BatModels %>% filter(Betacov==0) %>% filter(InAssoc==TRUE) %>% arrange(PropRank)
BatModels %>% filter(Betacov==0) %>% filter(InAssoc==FALSE) %>% arrange(PropRank)

read_csv("~/Github/virionette/04_predictors/Han-BatTraits.csv") %>%
  mutate(Pan = gsub("_"," ",Pan)) %>% dplyr::select(Pan) -> batnames

NonBatModels %>% as_tibble %>% 
  mutate(InAssoc = (Sp %in% assoc$host_species)) %>%
  mutate(NonBat = !(Sp %in% batnames$Pan)) -> NonBatModels
NonBatModels %>% filter(Betacov==0) %>% filter(NonBat==1) %>% filter(InAssoc==TRUE) %>% arrange(PropRank)
NonBatModels %>% filter(Betacov==0) %>% filter(NonBat==1) %>% filter(InAssoc==FALSE) %>% arrange(PropRank)





#
library(fasterize)
library(rgdal)
library(raster)
library(sf)

iucn <- st_read(dsn = "C:/Users/cjcar/Desktop/TERRESTRIAL_MAMMALS", layer='TERRESTRIAL_MAMMALS')

r <- disaggregate(getData("worldclim",var="alt",res=2.5)*0,2) # Make a blank raster

######################################################## TOP 50 DE NOVO BETACOV-

BatModels %>% filter(Betacov==0) %>% filter(InAssoc==FALSE) %>% arrange(PropRank) %>% dplyr::pull(Sp)-> unsampled.0
unsampled.0 <- unsampled.0[1:50] # Top 50 predictions

### Some manual fixes
unsampled.0[unsampled.0=='Eptesicus nasutus'] <- 'Rhyneptesicus nasutus'
unsampled.0[unsampled.0=='Chaerephon leucogaster'] <- 'Chaerephon pumilus'
###

iucn.sub <- iucn[iucn$binomial %in% unsampled.0,] # Subset IUCN maps to the right ones

length(unique(iucn.sub$binomial)) # How many of the species made it in?

unsampled.0[!(unsampled.0 %in% iucn$binomial)] # Which names are bonked?

unsampled50.map <- fasterize(iucn.sub, r, fun="sum")

######################################################## TOP 50 IN-SAMPLE BETACOV-

BatModels %>% filter(Betacov==0) %>% filter(InAssoc==TRUE) %>% arrange(PropRank) %>% dplyr::pull(Sp)-> sampled.0
sampled.0 <- sampled.0[1:50] # Top 50 predictions

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
true.1[true.1=='Rhinolophus monoceros'] <- 'Rhinolophus pusillus'
true.1[true.1=='Rhinolophus cornutus'] <- 'Rhinolophus pusillus'
###

iucn.sub <- iucn[iucn$binomial %in% true.1,] # Subset IUCN maps to the right ones

length(unique(iucn.sub$binomial)) # How many of the species made it in?

true.1[!(true.1 %in% iucn$binomial)] # Which names are bonked?

true1.map <- fasterize(iucn.sub, r, fun="sum")

###############################

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

levelplot(maps,  
          col.regions = mycolors,
          at = seq(0, 20, 1),
          alpha = 0.5, 
          scales=list(alternating=FALSE),
          par.strip.text=list(cex=0),
          xlab = NULL, ylab = NULL,
          maxpixels = 5e6) #+
  #layer(sp.polygons(outline, col = 'black'))
  

