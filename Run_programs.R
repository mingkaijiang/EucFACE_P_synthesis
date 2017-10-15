#### Master script 
#### To run the modules to generate EucFACE P synthesis

###### ---------------- setting up runs -------------------- ######
#### clear wk space
rm(list=ls(all=TRUE))

#### Source functions and packages
source("programs/prepare.R")

#### turn warnings off globally
options(warn=-1)

###### ---------------- Generating stuffs -------------------- ######
#### Ring-specific bulk density
soil_bulk_density <- make_soil_bulk_density()

#### Soil pools
soil_p_content <- make_soil_p_content(soil_bulk_density)

#### Microbial P pool
microbial_p_pool <- make_microbial_p_pool(soil_bulk_density)


#### Fine root P data will be available in 3 weeks time

#### Hedley fractionation data will be available in late October

#### Canopy P data (Sally Power)
canopy_p_pool <- make_canopy_p_pool()


###### ---------------- Making tables -------------------- ######










###### ---------------- End -------------------- ######
#### clear wk space
rm(list=ls(all=TRUE))
options(war=0)