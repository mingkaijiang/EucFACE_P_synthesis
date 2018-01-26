#### Master script 
#### To run the modules to generate EucFACE P synthesis

###### ---------------- setting up runs -------------------- ######
#### clear wk space
rm(list=ls(all=TRUE))

#### Source functions and packages
source("programs/prepare.R")

#### turn warnings off globally
options(warn=-1)

###### ---------------- Generating P concentrations -------------------- ######
#### Soil P concentrations
soil_p_concentration <- make_soil_p_concentration()

#### Soil phosphate conc, this returns % of P, not % of PO4!
soil_phosphate_concentration <- make_soil_phosphate_concentration()

#### Microbial P conc.
microbial_p_concentration <- make_microbial_p_concentration()

#### Canopy P conc.
canopy_p_concentration <- make_canopy_p_concentration()

#### Leaf litter P conc. 
leaflitter_p_concentration <- make_leaflitter_p_concentration()

#### Wood P conc. 
wood_p_concentration <- make_wood_p_concentration()

#### Frass P conc.
frass_p_concentration <- make_frass_p_concentration()

#### Fineroot P conc.
fineroot_p_concentration <- make_fineroot_p_concentration()

#### Understorey P conc.
understorey_p_concentration <- make_understorey_p_concentration()

#### Mycorrhizal P conc.
### Data not available yet.


###### ----------- Preparing C and other variables  ----------- ######
#### For all C pools, output annual biomass (year, ring, biomass)
#### For all C fluxes, output rate in unit of mg C m-2 d-1, and the period over which this rate applies
#### Then assign the P concentration to C pools and fluxes. 
#### Note: % P of total dry biomass should not be directly applied to C result, 
#### as amount of C is not amount of dry weight !!!

#### First, output all C pools and fluxes. 
#### For now, copy and paste all C-related codes. 
#### In the future, consider sourcing the code from the online C repository (stored in bitbucket).

#### Canopy related variables (SLA, LAI, Canopy biomass)
lai_variable <- make_lai_variable()
sla_variable <- make_sla_variable()
canopy_biomass_pool <- make_canopy_biomass_pool(lai_variable, sla_variable)

#### Wood C pool
# still waiting for year 2011-12 data
# wood_biomass_pool <- make_wood_c_pool()

#### Fineroot pools and production
# this is total fineroot biomass for 0-30cm
# currently implemented whole plant C fraction
# need to update with fineroot specific C fraction value
fineroot_c_pool <- make_fineroot_c_pool(c_fraction_fr)
fineroot_c_production_flux <- make_fineroot_c_production_flux(c_fraction_fr)

#### Understorey aboveground biomass



#### Frass production



#### Leaf litter production



#### Soil C content



#### Microbial C pool






#### Ring-specific bulk density
soil_bulk_density <- make_soil_bulk_density()


###### ----------- Generating P pools and fluxes  ----------- ######

#### Soil P pools
soil_p_content <- make_soil_p_content(soil_bulk_density)

#### Soil phosphate production
### Not sure whether the unit is already per day or not
### Currently assuming it is, because PO4 is rapidly turning over, 
### so the infrequent measurement is indicative of the accumulation of the previous 1 day
### But this assumption may be entirely incorrect!
soil_phosphate_production <- make_soil_phosphate_production(soil_bulk_density)

#### Soil P mienralization flux
soil_p_mineralization <- make_soil_p_mineralization_flux(soil_bulk_density)

#### Microbial P pool 
microbial_p_pool <- make_microbial_p_pool(soil_bulk_density)

#### Canopy P pool - return a data list - green leaf and dead leaf
canopy_p_pool <- make_canopy_p_pool()

#### Litter P production flux 
leaflitter_p_flux <- make_leaflitter_p_flux()  

#### Wood P pool   



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


###### ---------------- Making P budgeting variables and tables -------------------- ######
### P concentration by treatment and ring
source("programs/make_conc_summary_table_by_treatment.R")
summary_table_concentration_by_treatment <- make_conc_summary_table_by_treatment()

### leaf p retranslocation coefficient
source("programs/make_leaf_p_retranslocation_coefficient.R")
leaf_p_retrans_coefficient <- make_leaf_p_retranslocation_coefficient()


### standing P stock, i.e. canopy P + wood P + fine root P pools
### not able to produce due to incomplete of wood P pool



### P requirements, i.e. NPP * P conc for canopy, wood, twig and fine root, no data on understorey yet
source("programs/make_p_requirement.R")
p_requirement_table <- make_p_requirement_table(summary_table_concentration_by_treatment)


### total P retranslocation, i.e. canopy P - litterfall P + wood P increment
source("programs/make_total_p_retranslocation.R")
total_p_retranslocation <- make_total_p_retranslocation(summary_table_concentration_by_treatment)

### P uptake from soil, i.e. P requirement - P retranslocation
temDF <- rbind(p_requirement_table[5,2:3], total_p_retranslocation)
pupDF <- temDF[1,] - temDF[2,]

### Uptake/requirement
up_over_req <- pupDF/temDF[1,]

### MRT, i.e. Standing P / Uptake


### Standing PUE, i.e. NPP / P Uptake


### P pools and fluxes by treatment and ring

###### ---------------- Import local met data -------------------- ######


###### ---------- Compute statistical analyses ----------------- ######





###### ---------------- End -------------------- ######
#### clear wk space
rm(list=ls(all=TRUE))
options(war=0)

