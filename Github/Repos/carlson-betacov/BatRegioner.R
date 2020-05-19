
# Get the extent

library(rgdal)

SHAPEFILE <- readOGR(layer="GBD-Adjusted",dsn='D:/DECIMALS/Regions')
SSA <- c('Asia (Southeast)',
         'Asia (East)')
REGIONS <- SHAPEFILE[SHAPEFILE$Region %in% SSA,]

mammals <- readOGR(layer="TERRESTRIAL_MAMMALS",dsn='C:/Users/cjcar/Dropbox/HowManyHelminths2019')

batdf$seasia <- NA
batdf$easia <- NA

for(i in 1:nrow(batdf)) {
  batdf$
}