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

#### Soil P concentrations and pools
soil_p_concentration <- make_soil_p_concentration()
soil_p_content <- make_soil_p_content(soil_bulk_density)

#### Soil phosphate production flux 
#### Not sure whether the unit is already per day or not
#### Currently assuming it is, because PO4 is rapidly turning over, 
#### so the infrequent measurement is indicative of the accumulation of the previous 1 day
#### But this assumption may be entirely incorrect!
soil_phosphate_production <- make_soil_phosphate_production(soil_bulk_density)

#### Soil P mienralization flux
soil_p_mineralization <- make_soil_p_mineralization_flux(soil_bulk_density)

#### Microbial P pool and concentration
microbial_p_concentration <- make_microbial_p_concentration()
microbial_p_pool <- make_microbial_p_pool(soil_bulk_density)

#### Canopy P pool - return a data list - green leaf and dead leaf
canopy_p_concentration <- make_canopy_p_concentration()
canopy_p_pool <- make_canopy_p_pool()

#### Litter P flux and concentration
leaf_litter_p_concentration <- make_leaflitter_p_concentration()
leaf_litter_p_flux <- make_leaflitter_p_flux()  

#### Wood P pool   - use tree id!!!
wood_p_concentration <- make_wood_p_concentration()
# for wood p pool, still missing year 2012 wood measurement data
# to make John's code working. 


#### leaf p retranslocation coefficient
#### currently based on green and senesced leaves,
#### it was suggested to group senesced leaf together with leaflitter
leaf_p_retranslocation <- make_leaf_p_retranslocation()

#### Frass P production
frass_p_production <- make_frass_p_production_flux()

#### Fine root P biomass pool
fineroot_p_pool <- make_fineroot_p_pool()

#### Fine root P production flux
fineroot_p_production <- make_fineroot_p_production()

#### Understorey P pool, assume both species contributed equally
understorey_p_pool <- make_understorey_p_pool()

#### Leaching P flux - we have Shun's data, but needs drainage value

#### Hedley fractionation data will be available in early January

### mycorrhizal P content




###### ---------------- Making other important variables -------------------- ######

#### leaf p retranslocation coefficient
leaf_p_retranslocation <- make_leaf_p_retranslocation()


### Standing P stock
### P requirements
### P uptake from soil
### Uptake/requirement
### MRT
### Standing PUE
### P turnover rate in root, branch




#### Source the functions
#source("programs/make_tables.R")

#### Ring-specific table
#make_EucFACE_ring_table()

###### ---------------- End -------------------- ######
#### clear wk space
rm(list=ls(all=TRUE))
options(war=0)





