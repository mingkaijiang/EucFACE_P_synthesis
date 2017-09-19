#### Master script 
#### To run the modules to generate EucFACE P synthesis

###### ---------------- setting up runs -------------------- ######
#### clear wk space
rm(list=ls(all=TRUE))

#### Source functions and packages
source("programs/prepare.R")

###### ---------------- Generating stuffs -------------------- ######
#### Ring-specific bulk density
soil_bulk_density <- make_soil_bulk_density()

#### Soil pools
soil_p_content <- make_soil_p_content(soil_bulk_density)


#### Microbial P pool
microbial_p_pool <- make_microbial_p_pool(soil_bulk_density)
