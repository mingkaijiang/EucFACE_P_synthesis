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
#### This returns % of P, not % of PO4!
soil_phosphate_concentration <- make_soil_phosphate_concentration()

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
frass_p_concentration <- make_frass_p_concentration()
frass_p_production <- make_frass_p_production_flux()

#### Fine root P biomass pool
fineroot_P_concentration <- make_fineroot_p_concentration()
fineroot_p_pool <- make_fineroot_p_pool()

#### Fine root P production flux
fineroot_p_production <- make_fineroot_p_production()

#### Understorey P pool, assume both species contributed equally
understorey_p_concentration <- make_understorey_p_concentration()
understorey_p_pool <- make_understorey_p_pool()

#### Leaching P flux - we have Shun's data, but needs drainage value

#### Hedley fractionation data will be available in early January

### mycorrhizal P content




###### ---------------- Making other important variables -------------------- ######
### P concentration by treatment and ring
source("programs/make_conc_summary_table_by_treatment.R")
summary_table_concentration_by_treatment <- make_conc_summary_table_by_treatment()

### leaf p retranslocation coefficient
leaf_p_retranslocation <- make_leaf_p_retranslocation()


### standing P stock, i.e. canopy P + wood P + fine root P pools
### not able to produce due to incomplete of wood P pool



### P requirements, i.e. NPP * P conc for canopy, wood and fine root, no data on twig and branch


### total P retranslocation, i.e. canopy P - litterfall P + wood P increment



### P uptake from soil, i.e. P requirement - P retranslocation


### Uptake/requirement


### MRT, i.e. Standing P / Uptake


### Standing PUE, i.e. NPP / P Uptake


### P pools and fluxes by treatment and ring




###### ---------------- Output results -------------------- ######
if (rmarkdown = TRUE) {
    ### Create a R markdown summary report
    
    
} else {
    #### clear wk space
    rm(list=ls(all=TRUE))
    options(war=0)
}

###### ---------------- End -------------------- ######


