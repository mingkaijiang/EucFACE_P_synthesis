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

#### Soil P pools
soil_p_content <- make_soil_p_content(soil_bulk_density)

#### Soil phosphate production flux
soil_phosphate_production <- make_soil_phosphate_production(soil_bulk_density)

#### Soil P mienralization flux
soil_p_mineralization <- make_soil_p_mineralization_flux(soil_bulk_density)

#### Microbial P pool
microbial_p_pool <- make_microbial_p_pool(soil_bulk_density)

#### Canopy P data (Sally Power)
canopy_p_pool <- make_canopy_p_pool()

#### Frass P production
frass_p_production <- make_frass_p_production_flux()

#### Leaching P flux


#### Fine root P data will be available in 3 weeks time

#### Hedley fractionation data will be available in late October


###### ---------------- Making tables -------------------- ######

#### Source the functions
#source("programs/make_tables.R")

#### Ring-specific table
#make_EucFACE_ring_table()









###### ---------------- End -------------------- ######
#### clear wk space
rm(list=ls(all=TRUE))
options(war=0)





