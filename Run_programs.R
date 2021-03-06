####################################################################################################################
##### ---------------------------------------------------------------------------------------------------------##### 
#####                            Master script for EucFACE Phosphorus Budget                                   #####
##### ---------------------------------------------------------------------------------------------------------##### 
####################################################################################################################
####
#### Written by: Mingkai Jiang (m.jiang@westernsydney.edu.au)
#### 
#### Code structure:
#### 1. Compute phosphorus concentrations for major pools and fluxes
#### 2. Compute biomass pools, production fluxes and litter fluxes
#### 3. Generate P pools and fluxes
#### 4. Generate summary tables, based on unnormalized responses
#### 5. Generate un-normalized plots
#### 6. Generate normalized responses 
#### 7. Generate normalized plots

##### ---------------------------------------------------------------------------------------------------------##### 
##### Step 0: Prepare the repository (clean and read in necessary packages)
#### clear wk space
rm(list=ls(all=TRUE))

#### Source functions and packages
source("programs/prepare.R")

#### turn warnings off globally
# options(warn=-1)



##### ---------------------------------------------------------------------------------------------------------##### 
##### Step 1: Generate P concentrations

#### 1.1: Soil P concentrations 
soil_p_concentration <- make_soil_p_concentration()


#### 1.2: Soil phosphate conc, this returns % of P, not % of PO4!
#### Only top 10 cm!
soil_phosphate_concentration <- make_soil_phosphate_concentration()

#### 1.3 Microbial P conc.
#### Only top 10 cm!
microbial_p_concentration <- make_microbial_p_concentration()


#### 1.4 Canopy P conc.
canopy_p_concentration <- make_canopy_p_concentration()

#canopy_p_concentration <- make_canopy_p_concentration_new()
#compare_canopy_p_conc_datasets(inDF1=canopy_p_concentration_limited_data, inDF2=canopy_p_concentration)
#canopy_p_concentration <- make_canopy_p_concentration_fitted_with_age()



#### 1.5 Leaf litter P conc. 
leaflitter_p_concentration <- make_leaflitter_p_concentration()
#leaflitter_p_concentration <- make_leaflitter_p_concentration_new()


#### 1.6 Wood P conc. 
wood_p_concentration <- make_wood_p_concentration()


#### 1.7 Frass P conc.
frass_p_concentration <- make_frass_p_concentration()


#### 1.8 Fineroot P conc.
fineroot_p_concentration <- make_fineroot_p_concentration()


#### 1.9 Understorey P conc.
understorey_p_concentration <- make_understorey_p_concentration()

#### 1.10 Understorey litter P conc.
understorey_litter_p_concentration <- make_understorey_litter_p_concentration()


#### 1.11 Hedley fractionation dataset
soil_hedley_p_concentration <- make_soil_hedley_p_concentration()




##### ---------------------------------------------------------------------------------------------------------##### 
##### Step 2: preparing C related variables
#### For all C pools, unit in g C m-2,
#### For all C fluxes, output rate in unit of mg C m-2 d-1, and the period over which this rate applies
#### Then assign the P concentration to C pools and fluxes. 
#### Note: % P of total dry biomass should not be directly applied to C result, 
#### as amount of C is not amount of dry weight !!!


#### 2.1 Canopy related variables (SLA, LAI, Canopy biomass)
lai_variable <- make_lai_variable()
sla_variable <- make_sla_variable()

canopy_c_pool <- make_canopy_c_pool(lai_variable, sla_variable, sla_option="variable")


#### 2.2 Litter production (leaf, twig, bark, seed)
litter_c_production_flux <- make_litter_c_flux(c_fraction)

leaflitter_c_production_flux <- litter_c_production_flux[,c("Date", "Ring", "leaf_flux", "Start_date", "End_date", "Days")]
twiglitter_c_production_flux <- litter_c_production_flux[,c("Date", "Ring", "twig_flux", "Start_date", "End_date", "Days")]
barklitter_c_production_flux <- litter_c_production_flux[,c("Date", "Ring", "bark_flux", "Start_date", "End_date", "Days")]
seedlitter_c_production_flux <- litter_c_production_flux[,c("Date", "Ring", "seed_flux", "Start_date", "End_date", "Days")]

#### 2.3 Canopy C production
## assume it's the same as litterfall C production
canopy_c_production_flux <- leaflitter_c_production_flux

## based on change in leaf area and litterfall
## calculate change in leaf pool as well as litterfall
#dLEAF_litter_flux <- make_dLAI_litter(litter=leaflitter_c_production_flux, sla_variable=sla_variable)
#canopy_c_production_flux_new <- make_canopy_c_production_flux_new(inDF=dLEAF_litter_flux)

#### 2.4 Wood C pool
# year 2011-12 data on local directory
wood_c_pool <- make_wood_c_pool(ring_area=FACE_ring_area,
                                c_frac=c_fraction)

# wood c without excluding mortality
# mortality is not a big issue here as only one tree was marked dead!
# missing data? Tree shrinking? Measurement error?
#wood_c_pool_total <- make_wood_c_pool_total(ring_area=FACE_ring_area,
#                                            c_frac=c_fraction)

## standing dead wood c pool
standing_dead_c_pool <- make_standing_dead_c_pool(ring_area=FACE_ring_area,
                                                       c_frac=c_fraction)

#### 2.5 Wood C production
wood_c_production <- make_wood_production_flux(wood_c_pool)

## standing dead wood c flux
standing_dead_c_flux <- make_standing_dead_c_flux(standing_dead_c_pool)

#### 2.6 Fineroot pool
fineroot_c_pool <- make_fineroot_c_pool()

#### 2.7 Fineroot production
fineroot_c_production_flux <- make_fineroot_c_production_flux()

#### 2.8 Understorey aboveground biomass - 1: Varsha's clipping; 2: Matthias's stereo camera
understorey_c_pool <- make_understorey_aboveground_c_pool(c_fraction_ud,
                                                          strip_area)

understorey_c_pool_2 <- make_understorey_aboveground_c_pool_2(c_fraction_ud)

#make_understorey_pool_size_comparison(understorey_c_pool,
#                                      understorey_c_pool_2,
#                                      plotting = T)


#### 2.9 Understorey production flux - 1: Varsha's clipping; 2: Matthias's stereo camera
understorey_c_flux <- make_understorey_aboveground_production_flux(c_fraction_ud)

understorey_c_flux_2 <- make_understorey_aboveground_production_flux_2(c_fraction_ud)

#### 2.10 understorey litter flux
understorey_litter_c_flux <- make_understorey_litter_flux(c_fraction_ud)

### estimate biomass growth based on cover data
#make_understorey_aboveground_growth_estimate(plotting = T)

### estimate % live and % dead
source("programs/summary_variables/unnormalized/make_understorey_percent_live_estimate.R")
understorey_live_percent <- make_understorey_percent_live_estimate()

#### 2.11 Frass production
frass_c_production_flux <- make_frass_c_production_flux()

#### 2.12 Soil C content
## Ring-specific bulk density
soil_bulk_density <- make_soil_bulk_density()

# return sum of all depths
soil_c_pool <- make_soil_c_pool(soil_bulk_density)

#### 2.13 Microbial C pool
# this pool has data only at 0-10cm depth - Cat's data
microbial_c_pool <- make_microbial_c_pool(soil_bulk_density)

### Yolima's data
#microbial_c_pool2 <- make_microbial_pool2(soil_bulk_density)

#### 2.14 Soil mycorrhizal production
mycorrhizal_c_pool <- make_mycorrhizal_c_pool(microbial_c_pool)

#### 2.15 Coarse root C pool 
coarse_root_c_pool <- make_coarse_root_pool(c_fraction, fr_pool=fineroot_c_pool) 

#### 2.16 Coarse root C production
coarse_root_c_flux <- make_coarse_root_production_flux(coarse_root_c_pool) 

#### 2.17 Leaf litter pool
leaflitter_c_pool <- make_leaflitter_pool(c_fraction)



##### ---------------------------------------------------------------------------------------------------------##### 
##### Step 3: Generating P pools and fluxes
#### 3.1 Soil P pool
soil_p_pool <- make_soil_p_pool(p_conc=soil_p_concentration,
                                bk_density=soil_bulk_density)

#### 3.2 Soil phosphate pool
### This is additional to the microbial PO4-P pool
### The microbial pool needs to have one step further to break the cell when extracting
### So this pool is more readily available to plants. 
soil_phosphate_pool <- make_soil_phosphate_pool(p_conc=soil_phosphate_concentration,
                                                bk_density=soil_bulk_density)

#### 3.3 Soil P pool of different bioavailability
soil_p_pool_hedley <- make_soil_p_pool_hedley(p_conc=soil_hedley_p_concentration,
                                              bk_density=soil_bulk_density)


#### 3.4 Soil P mineralization flux
#### It is assumed that the mineralization data is for top 10 cm only!
soil_p_mineralization <- make_soil_p_mineralization_flux(soil_bulk_density)

#### Soil P leaching rate
soil_p_leaching <- make_soil_p_leaching_flux()

#### 3.5 Microbial P pool 
#### Top 10 cm
microbial_p_pool <- make_microbial_p_pool(p_conc=microbial_p_concentration,
                                          bk_density=soil_bulk_density)

#### 3.6 Canopy P pool - only for green leaves
canopy_p_pool <- make_canopy_p_pool(p_conc=canopy_p_concentration,
                                    biom=canopy_c_pool)

#canopy_p_pool <- make_canopy_p_pool_smoothed(biom=dLEAF_litter_flux)

### this is a new function yet to be written
leaflitter_p_pool <- make_leaflitter_p_pool(p_conc=leaflitter_p_concentration,
                                            c_pool=leaflitter_c_pool,
                                            c_frac=c_fraction)

#### 3.7 Canopy production flux
canopy_p_flux <- make_canopy_p_production(p_conc=canopy_p_concentration,
                                          c_flux=canopy_c_production_flux,
                                          c_frac=c_fraction)

## considered both change in LAI and litterfall
#canopy_p_flux <- make_canopy_p_production_new(c_flux=canopy_c_production_flux_new,
#                                                  c_frac=c_fraction)


#### 3.8 Litter P production flux 
#### Literfall biomass (not C) will be calculated within the function
#### for data points where we have C but not P, we can create a separte script
#### and gap-fill P concentration based on average values
leaflitter_p_flux <- make_leaflitter_p_flux(p_conc=leaflitter_p_concentration,
                                            c_flux=leaflitter_c_production_flux,
                                            c_frac=c_fraction)  
#leaflitter_p_flux_gap_fill <- make_leaflitter_p_flux_gap_fill(p_conc=leaflitter_p_concentration) 


#### 3.9 Fine root litter P production
### assuming P retranslocation coefficient for fine root is 50%
### and fine root c production flux is fine root c litter flux
fineroot_litter_p_flux <- make_fineroot_litter_p_production(p_conc=fineroot_p_concentration,
                                                            c_flux=fineroot_c_production_flux,
                                                            p_retrans=0.5)

#### 3.10 Other litterfall
twig_litter_p_flux <- make_twiglitter_p_flux(p_conc=wood_p_concentration, litter_flux=twiglitter_c_production_flux)  
bark_litter_p_flux <- make_barklitter_p_flux(p_conc=wood_p_concentration, litter_flux=barklitter_c_production_flux)  
seed_litter_p_flux <- make_seedlitter_p_flux(p_conc=wood_p_concentration, litter_flux=seedlitter_c_production_flux)  

#### 3.11 Wood P pool   
sapwood_p_pool <- make_wood_p_pool(p_conc=wood_p_concentration,
                                c_pool=wood_c_pool,
                                case_consideration = "sapwood")


wood_p_pool <- make_wood_p_pool(p_conc=wood_p_concentration,
                                c_pool=wood_c_pool,
                                case_consideration = "total")


#### standing dead p pool
standing_dead_p_pool <- make_wood_p_pool(p_conc=wood_p_concentration,
                                         c_pool=standing_dead_c_pool,
                                         case_consideration = "sapwood")

#### 3.12 wood p flux
wood_p_flux <- make_wood_p_production(p_conc=wood_p_concentration,
                                      c_flux=wood_c_production)

#### Standing dead P flux
standing_dead_p_flux <- make_standing_dead_p_flux(p_conc=wood_p_concentration,
                                               c_flux=standing_dead_c_flux)

#### 3.13 Frass P production
#### Used C fraction for frass to convert c production back to frass biomass
#### We have more p conc than c flux so no need to gap fill. 
frass_c_fraction <- make_frass_c_fraction()
frass_p_production <- make_frass_p_production_flux(p_conc=frass_p_concentration,
                                                   c_flux=frass_c_production_flux,
                                                   c_frac=frass_c_fraction)

#### 3.14 Fine root P biomass pool
fineroot_p_pool <- make_fineroot_p_pool(p_conc=fineroot_p_concentration,
                                        c_pool=fineroot_c_pool)

#### 3.15 Fine root P production flux
fineroot_p_production <- make_fineroot_p_production(p_conc=fineroot_p_concentration,
                                                    c_flux=fineroot_c_production_flux)

#### 3.16 Understorey P pool, assume both species contributed equally
#### Also because p_conc and c_pool do not match in time,
#### we are taking the average of p_conc and apply it to c_pool
#### Here we can use Matthias's stereo camera estimate (2) or 
#### Varsha's harvest data (1) to extrapolate for p pool
#### It makes more sense to use harvest at this stage in time!
#### Also, if use Varsha's harvest data, we can use either total or live part of biomass
understorey_p_pool <- make_understorey_p_pool(p_conc=understorey_p_concentration,
                                              p_lit_conc=understorey_litter_p_concentration,
                                              c_pool=understorey_c_pool,
                                              c_frac=c_fraction_ud,
                                              live_or_total = "Total")

#### 3.17 Understorey production flux
#### Here we can use either stereo camera estimate of biomass (2) or
#### Harvest biomass data (1) to calculate p flux
#### Currently, we are using harvest estimate
understorey_p_flux <- make_understorey_p_flux(p_conc=understorey_p_concentration,
                                              c_flux=understorey_c_flux,
                                              c_frac=c_fraction_ud)

understorey_litter_p_flux <- make_understorey_litter_p_flux(p_conc=understorey_p_concentration,
                                              c_flux=understorey_litter_c_flux,
                                              c_frac=c_fraction_ud)

### 3.18 Coarse root P pool
### should add heartwood P concentration to replace part of coarse root P pool
coarse_root_p_pool <- make_coarse_root_p_pool(p_conc=wood_p_concentration,
                                                c_pool=coarse_root_c_pool,
                                                c_frac=c_fraction)


### 3.19 Coarse root P flux
coarse_root_p_flux <- make_coarse_root_p_flux(p_conc=wood_p_concentration,
                                                c_flux=coarse_root_c_flux,
                                                c_frac=c_fraction)


### 3.20 delta P pools
delta_soil_p_pool <- make_yearly_delta_pool_function(inDF=soil_p_pool, var.col=3)

delta_canopy_p_pool <- make_yearly_delta_pool_function(inDF=canopy_p_pool, var.col=3)

delta_wood_p_pool <- make_yearly_delta_pool_function(inDF=wood_p_pool, var.col=3)

delta_fineroot_p_pool <- make_yearly_delta_pool_function(inDF=fineroot_p_pool, var.col=3)

delta_coarse_root_p_pool <- make_yearly_delta_pool_function(inDF=coarse_root_p_pool, var.col=3)

delta_understorey_p_pool <- make_yearly_delta_pool_function(inDF=understorey_p_pool, var.col=3)

delta_microbial_p_pool <- make_yearly_delta_pool_function(inDF=microbial_p_pool, var.col=3)




##### ---------------------------------------------------------------------------------------------------------##### 
##### Step 4: Making P budgeting variables and tables, based on raw data
#### 4.1 Summary Tables
source("programs/summary_tables/unnormalized/make_conc_summary_table.R")
summary_table_concentration <- make_conc_summary_table()

### P pools by treatment and ring
source("programs/summary_tables/unnormalized/make_pool_summary_table.R")
summary_table_pool <- make_pool_summary_table()

### P fluxes by treatment and ring
source("programs/summary_tables/unnormalized/make_flux_summary_table.R")
summary_table_flux <- make_flux_summary_table()

### C pools by treatment and ring
source("programs/summary_tables/unnormalized/make_c_pool_summary_table.R")
summary_table_c_pool <- make_c_pool_summary_table()

### C fluxes by treatment and ring
source("programs/summary_tables/unnormalized/make_c_flux_summary_table.R")
summary_table_c_flux <- make_c_flux_summary_table()


### CP ratios
source("programs/summary_tables/unnormalized/make_cp_ratios.R")
summary_cp_ratios <- make_cp_ratios(c_pool=summary_table_c_pool,
                                    p_pool=summary_table_pool,
                                    c_flux=summary_table_c_flux,
                                    p_flux=summary_table_flux)


#### 4.2 retranslocation coefficients
### canopy leaf n retranslocation coefficient
leaf_p_retrans_coefficient <- make_canopy_leaf_p_retranslocation_coefficient(df1=canopy_p_concentration,
                                                                             df2=leaflitter_p_concentration)

### understorey leaf p retranslocation coefficient
understorey_p_retrans_coefficient <- make_understorey_p_retranslocation_coefficient(df1=understorey_p_concentration,
                                                                                    df2=understorey_litter_p_concentration)

### fineroot retrans
### assumed value
fineroot_p_retrans_coefficient <- make_fineroot_p_retrans_coefficient(retrans=0.5)

### wood retrans
wood_p_retrans_coefficient <- make_stem_p_retrans_coefficient(sapwood=wood_p_concentration)

### coarseroot retrans
coarseroot_p_retrans_coefficient <- make_stem_p_retrans_coefficient(sapwood=wood_p_concentration)



#### 4.3 Summary variables
### vegetation standing P stocks
vegetation_standing_p_stock <- make_vegetation_standing_p_stock(leaf=canopy_p_pool,
                                                                wood=wood_p_pool,
                                                                fineroot=fineroot_p_pool,
                                                                coarseroot=coarse_root_p_pool,
                                                                understorey=understorey_p_pool)



### total plant P requirement flux, retranslocation flux, and uptake flux
### for total retranslocation flux and uptake flux,
total_plant_p_fluxes <- make_total_plant_p_fluxes(sumDF=summary_table_flux,
                                                  wood_retrans_coef=wood_p_retrans_coefficient)


### P mean residence time in plant
plant_p_MRT <- make_plant_P_mean_residence_time(p_stand=vegetation_standing_p_stock,
                                                p_flux=total_plant_p_fluxes)

### Plant P use efficiency
plant_p_use_efficiency <- make_plant_P_use_efficiency(c_flux=summary_table_c_flux,
                                                      p_flux=total_plant_p_fluxes)


#### 4.4 P budget summary
### Calculate all N budgeting variables
total_p_budget <- make_total_p_budget()



##### ---------------------------------------------------------------------------------------------------------##### 
##### Step 5. Plotting P budget figures, based on unnormalized data
#### Note that you need to go into each function to plot!
norm <- "unnormalized"

##### Note: for the following plotting script, you will need to go into the function
####        and then plot. 
####        So, firstly, copy and paste inDF = total_p_budget_norm in your console,
####        then open the function, then plot. 
source("programs/plot_scripts/make_p_budget_summary_plots.R")
make_p_budget_summary_plots(inDF=total_p_budget,
                            norm=norm)

### Concentration
source("programs/plot_scripts/make_p_concentration_summary_plots.R")
make_p_concentration_summary_plots(inDF=summary_table_concentration,
                                   norm=norm)

### P pool
source("programs/plot_scripts/make_p_pools_summary_plots.R")
make_p_pools_summary_plots(inDF=summary_table_pool,
                           norm=norm)

### P flux
source("programs/plot_scripts/make_p_fluxes_summary_plots.R")
make_p_fluxes_summary_plots(inDF=summary_table_flux,
                            norm=norm)


#### Individial rings
source("programs/plot_scripts/make_p_budget_ring_plots.R")
make_p_budget_ring_plots(inDF=total_p_budget,
                         norm=norm)


### Soil hedley P pools
source("programs/plot_scripts/make_soil_p_budget_summary_plots.R")
make_soil_p_budget_summary_plots(inDF=summary_table_pool,
                                 norm=norm)




###### ---------------------------------------------------------------------------------------------------------##### 
###### Step 6: Normalize all responses to a pretreatment soil conditions
#
#### 6.1 Summary Tables
source("programs/summary_tables/normalized/make_normalized_concentration_summary_table.R")
summary_table_concentration_norm <- make_normalized_concentration_summary_table(inDF=summary_table_concentration)

### P pools by treatment and ring
source("programs/summary_tables/normalized/make_normalized_pool_summary_table.R")
summary_table_pool_norm <- make_normalized_pool_summary_table(inDF=summary_table_pool)

### P fluxes by treatment and ring
source("programs/summary_tables/normalized/make_normalized_flux_summary_table.R")
summary_table_flux_norm <- make_normalized_flux_summary_table(inDF=summary_table_flux)

### C pools by treatment and ring
source("programs/summary_tables/normalized/make_normalized_c_pool_summary_table.R")
summary_table_c_pool_norm <- make_normalized_c_pool_summary_table(inDF=summary_table_c_pool)

### C fluxes by treatment and ring
source("programs/summary_tables/normalized/make_normalized_c_flux_summary_table.R")
summary_table_c_flux_norm <- make_normalized_c_flux_summary_table(inDF=summary_table_c_flux)


### CP ratios
source("programs/summary_tables/normalized/make_normalized_cp_ratios.R")
summary_cp_ratios_norm <- make_normalized_cp_ratios(c_pool=summary_table_c_pool_norm,
                                                    p_pool=summary_table_pool_norm,
                                                    c_flux=summary_table_c_flux_norm,
                                                    p_flux=summary_table_flux_norm)


#### 6.2 retranslocation coefficients
#### canopy leaf n retranslocation coefficient
#leaf_p_retrans_coefficient <- make_canopy_leaf_p_retranslocation_coefficient(df1=canopy_p_concentration,
#                                                                             df2=leaflitter_p_concentration)
#
#### understorey leaf p retranslocation coefficient
#understorey_p_retrans_coefficient <- make_understorey_p_retranslocation_coefficient(df1=understorey_p_concentration,
#                                                                                    df2=understorey_litter_p_concentration)
#
#### fineroot retrans
#### assumed value
#fineroot_p_retrans_coefficient <- make_fineroot_p_retrans_coefficient(retrans=0.5)
#
#### wood retrans
#wood_p_retrans_coefficient <- make_stem_p_retrans_coefficient(sapwood=wood_p_concentration)
#
#### coarseroot retrans
#coarseroot_p_retrans_coefficient <- make_stem_p_retrans_coefficient(sapwood=wood_p_concentration)



#### 6.3 Summary variables
### vegetation standing P stocks
vegetation_standing_p_stock_norm <- make_normalized_vegetation_standing_p_stock(inDF=summary_table_pool_norm)



### total plant P requirement flux, retranslocation flux, and uptake flux
### for total retranslocation flux and uptake flux,
total_plant_p_fluxes_norm <- make_total_plant_p_fluxes(sumDF=summary_table_flux_norm,
                                                       wood_retrans_coef=wood_p_retrans_coefficient)


### P mean residence time in plant
plant_p_MRT_norm <- make_plant_P_mean_residence_time(p_stand=vegetation_standing_p_stock_norm,
                                                p_flux=total_plant_p_fluxes_norm)

### Plant P use efficiency
plant_p_use_efficiency_norm <- make_plant_P_use_efficiency(c_flux=summary_table_c_flux_norm,
                                                      p_flux=total_plant_p_fluxes_norm)


#### 6.4 P budget summary
total_p_budget_norm <- make_normalized_total_p_budget()


###### ---------------------------------------------------------------------------------------------------------##### 
###### Step 7. Plotting P budget figures, based on normalized responses
norm <- "normalized"


##### Note: for the following plotting script, you will need to go into the function
####        and then plot. 
####        So, firstly, copy and paste inDF = total_p_budget_norm in your console,
####        then open the function, then plot. 
source("programs/plot_scripts/make_p_budget_summary_plots.R")
make_p_budget_summary_plots(inDF=total_p_budget_norm,
                            norm=norm)

### Concentration
source("programs/plot_scripts/make_p_concentration_summary_plots.R")
make_p_concentration_summary_plots(inDF=summary_table_concentration_norm,
                                   norm=norm)

### P pool
source("programs/plot_scripts/make_p_pools_summary_plots.R")
make_p_pools_summary_plots(inDF=summary_table_pool_norm,
                           norm=norm)

### P flux
source("programs/plot_scripts/make_p_fluxes_summary_plots.R")
make_p_fluxes_summary_plots(inDF=summary_table_flux_norm,
                            norm=norm)


#### Individial rings
source("programs/plot_scripts/make_p_budget_ring_plots.R")
make_p_budget_ring_plots(inDF=total_p_budget_norm,
                         norm=norm)


### Soil hedley P pools
source("programs/plot_scripts/make_soil_p_budget_summary_plots.R")
make_soil_p_budget_summary_plots(inDF=summary_table_pool_norm,
                                 norm=norm)










###### ---------------- End -------------------- ######
#### clear wk space
rm(list=ls(all=TRUE))
options(war=0)

