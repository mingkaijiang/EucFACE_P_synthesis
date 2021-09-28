####################################################################################################################
##### ---------------------------------------------------------------------------------------------------------##### 
#####                            Master script for EucFACE Phosphorus Budget                                   #####
##### ---------------------------------------------------------------------------------------------------------##### 
####################################################################################################################
####
#### Written by: Mingkai Jiang (m.jiang@westernsydney.edu.au)
#### 
####                              CODE STRUCTURE                
####
#### 1. Compute phosphorus concentrations for major pools and fluxes
####
#### 2. Compute biomass pools, production fluxes and litter fluxes
####
#### 3. Generate P pools and fluxes
####
#### 4. Generate summary tables, based on unnormalized responses
####
#### 5. Generate un-normalized plots
####
#### 6. Generate normalized responses 
####
#### 7. Generate normalized plots

##### ---------------------------------------------------------------------------------------------------------##### 
##### Step 0: Prepare the repository (clean and read in necessary packages)
#### clear wk space
rm(list=ls(all=TRUE))

#### Source functions and packages
source("programs/prepare.R")

#### turn warnings off globally
# options(warn=-1)

#### Ring-specific bulk density
soil_bulk_density <- make_soil_bulk_density()


########################################################################################## 
########################################################################################## 
#####
##### Step 1: Generate P concentrations
#####

############################## Soil ###############################


#belowground_P_working_sheet()


#### Soil P concentrations 
### three depth: 0 - 10 cm
###              10 - 20 cm
###              20 - 30 cm, but mostly missing
soil_p_concentration <- make_soil_p_concentration()


#### Soil phosphate conc, this returns % of P, not % of PO4!
#### Only top 10 cm in the earlier datasets (with additional 0 - 2 cm depth)
#### but Johanna's 2018 data has a complete depth profile (up to transition zone)
soil_phosphate_concentration <- make_soil_phosphate_concentration()


#### Microbial P conc.
#### Top 60 cm
microbial_p_concentration <- make_microbial_p_concentration()

#### Hedley fractionation dataset
### top 10 cm
soil_hedley_p_concentration <- make_soil_hedley_p_concentration()


############################## Plant ###############################

#### Canopy P conc.
canopy_p_concentration <- make_canopy_p_concentration()

#canopy_p_concentration <- make_canopy_p_concentration_new()
#compare_canopy_p_conc_datasets(inDF1=canopy_p_concentration_limited_data, inDF2=canopy_p_concentration)
#canopy_p_concentration <- make_canopy_p_concentration_fitted_with_age()



#### Leaf litter P conc. 
leaflitter_p_concentration <- make_leaflitter_p_concentration()
#leaflitter_p_concentration <- make_leaflitter_p_concentration_new()


#### Wood P conc. 
sapwood_p_concentration <- make_wood_p_concentration()


#### Frass P conc.
### frass flux needs to added to canopy P demand each year.
frass_p_concentration <- make_frass_p_concentration()


#### Fineroot P conc.
### top 30 cm of soil
fineroot_p_concentration <- make_fineroot_p_concentration()


#### Understorey P conc.
understorey_p_concentration <- make_understorey_p_concentration()


#### Understorey litter P conc.
understorey_litter_p_concentration <- make_understorey_litter_p_concentration()




########################################################################################## 
########################################################################################## 
#####
##### Step 2: Calculate retranslocation coefficients
#####

plant_p_retranslocation_coefficients <- calculate_plant_p_retranslocation_coefficients(canopy_p_concentration,
                                                                                       leaflitter_p_concentration,
                                                                                       understorey_p_concentration,
                                                                                       understorey_litter_p_concentration,
                                                                                       sapwood_p_concentration,
                                                                                       fineroot_retranslocation_coefficient = retrans_froot,
                                                                                       plot.option = T)



########################################################################################## 
########################################################################################## 
#####
##### Step 3: Preparing C related variables
#####
##### Note: 
##### For all C pools, unit in g C m-2,
##### For all C fluxes, output rate in unit of mg C m-2 d-1, 
##### and the period over which this rate applies.
##### Then assign the P concentration to C pools and fluxes. 
##### Note: % P of total dry biomass should not be directly applied to C result, 
##### as amount of C is not amount of dry weight !!!


############################## Pools ###############################

#### Canopy related variables (SLA, LAI, Canopy biomass)
lai_variable <- make_lai_variable()
sla_variable <- make_sla_variable()

canopy_c_pool <- make_canopy_c_pool(lai_variable, 
                                    sla_variable, 
                                    sla_option="variable")


#### Wood C pool
# year 2011-12 data on local directory
# we have sapwood and heartwood in the dataframe
# excluded dead trees (before 2018)
wood_c_pool <- make_wood_c_pool(ring_area=FACE_ring_area,
                                c_frac=c_fraction)


### standing dead wood c pool
### only 4 rings have mortality data - dataset incomplete
### We know there are more trees died.
### only report the max standing dead (i.e. 2018 value)
standing_dead_c_pool <- make_standing_dead_c_pool(ring_area=FACE_ring_area,
                                                  c_frac=c_fraction)


#### Fineroot pool
fineroot_c_pool <- make_fineroot_c_pool()


#### Understorey aboveground biomass 
### - 1: Varsha's clipping
### - 2: Matthias's stereo camera
### we have live and dead fraction based on clipping method
### We decide to use clipping method to estimate the relative proportion of live and dead 
### then we use stereo camera approach to estimate the total biomass

### estimate % live and % dead
understorey_c_pool_clipping <- make_understorey_aboveground_c_pool_clipping(c_fraction_ud,
                                                                            strip_area)

understorey_c_pool <- make_understorey_aboveground_c_pool_camera(c_frac=c_fraction_ud,
                                                                 plot.option=T)

make_understorey_pool_size_comparison(inDF1=understorey_c_pool_clipping,
                                      inDF2=understorey_c_pool,
                                      plot.option = T)



#### Soil C content

#### return sum of all depths
soil_c_pool <- make_soil_c_pool(soil_bulk_density)


#### Microbial C pool
#### this pool has data only at 0-10cm depth - Cat's data
microbial_c_pool <- make_microbial_c_pool(soil_bulk_density)


#### Yolima's data
### data no longer exists, ignore this
#microbial_c_pool2 <- make_microbial_pool2(soil_bulk_density)



#### Soil mycorrhizal pool
#### But we don't have a P concentration for it and 
#### therefore it's not included in the P budget
mycorrhizal_c_pool <- make_mycorrhizal_c_pool(microbial_c_pool)


#### Coarse root C pool 
coarse_root_c_pool <- make_coarse_root_pool(c_fraction, 
                                            fr_pool=fineroot_c_pool) 


#### Leaf litter pool - forest floor leaf litter pool
leaflitter_c_pool <- make_leaflitter_pool(c_fraction)



############################## Fluxes ###############################

#### Ltter production (leaf, twig, bark, seed)
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
dLEAF_litter_flux <- make_dLAI_litter(litter=leaflitter_c_production_flux, 
                                      sla_variable=sla_variable)

canopy_c_production_flux_new <- make_canopy_c_production_flux_new(inDF=dLEAF_litter_flux)

make_canopy_production_flux_comparison(inDF1=canopy_c_production_flux,
                                       inDF2=canopy_c_production_flux_new,
                                       plot.option = T)


#### 2.5 Wood C production
wood_c_production <- make_wood_production_flux(wood_c_pool)

## standing dead wood c flux
#standing_dead_c_flux <- make_standing_dead_c_flux(standing_dead_c_pool)


#### Fineroot production
fineroot_c_production_flux <- make_fineroot_c_production_flux()



#### Understorey production flux 
#### - 1: Varsha's clipping
#### - 2: Matthias's stereo camera
understorey_c_flux_clipping <- make_understorey_aboveground_production_flux_clipping(c_fraction_ud)

### we need to follow the C budget and therefore this is ignored.
#understorey_c_flux_camera <- make_understorey_aboveground_production_flux_camera(c_frac=c_fraction_ud)
#
#
#make_canopy_production_flux_comparison(inDF1=understorey_c_flux_clipping,
#                                       inDF2=understorey_c_flux_camera,
#                                       plot.option = T)
### estimate biomass growth based on cover data
#make_understorey_aboveground_growth_estimate(plotting = T)


#### understorey litter flux
### basically understorey dead 
understorey_litter_c_flux <- make_understorey_litter_flux(c_fraction_ud)


#### Frass production
frass_c_production_flux <- make_frass_c_production_flux()


#### Coarse root C production
coarse_root_c_flux <- make_coarse_root_production_flux(coarse_root_c_pool) 




########################################################################################## 
########################################################################################## 
#####
##### Step 4: Generating P pools and fluxes
#####


############################## P Pools ###############################

#### Soil P pool
#### top 10 cm only
soil_p_pool <- make_soil_p_pool(p_conc=soil_p_concentration,
                                bk_density=soil_bulk_density)

#### Soil phosphate pool
#### Top 10 cm only
#### Note:
#### This is additional to the microbial PO4-P pool
#### The microbial pool needs to have one step further 
#### to break the cell when extracting.
#### So this pool is more readily available to plants. 
soil_phosphate_pool <- make_soil_phosphate_pool(p_conc=soil_phosphate_concentration,
                                                bk_density=soil_bulk_density)


#### Soil P pool of different bioavailability
soil_p_pool_hedley <- make_soil_p_pool_hedley(p_conc=soil_hedley_p_concentration,
                                              bk_density=soil_bulk_density)


#### Microbial P pool 
#### Top 10 cm
microbial_p_pool <- make_microbial_p_pool(p_conc=microbial_p_concentration,
                                          bk_density=soil_bulk_density)



#### Canopy P pool - only for green leaves
canopy_p_pool <- make_canopy_p_pool(p_conc=canopy_p_concentration,
                                    biom=canopy_c_pool)

canopy_p_pool_new <- make_canopy_p_pool_smoothed(biom=dLEAF_litter_flux)


make_canopy_P_pool_comparison(inDF1=canopy_p_pool,
                              inDF2=canopy_p_pool_new,
                              plot.option = T)



### Forest floor leaf litter pool
leaflitter_p_pool <- make_leaflitter_p_pool(p_conc=leaflitter_p_concentration,
                                            c_pool=leaflitter_c_pool,
                                            c_frac=c_fraction)



#### Wood P pool 
wood_p_pool <- make_wood_p_pool(p_conc=sapwood_p_concentration,
                                c_pool=wood_c_pool,
                                case_consideration = "total")

sapwood_p_pool <- make_wood_p_pool(p_conc=sapwood_p_concentration,
                                   c_pool=wood_c_pool,
                                   case_consideration = "sapwood")


heartwood_p_pool <- make_wood_p_pool(p_conc=sapwood_p_concentration,
                                     c_pool=wood_c_pool,
                                     case_consideration = "heartwood")


#### Standing dead p pool
standing_dead_p_pool <- make_wood_p_pool(p_conc=sapwood_p_concentration,
                                         c_pool=standing_dead_c_pool,
                                         case_consideration = "total")


#### Fine root P biomass pool
fineroot_p_pool <- make_fineroot_p_pool(p_conc=fineroot_p_concentration,
                                        c_pool=fineroot_c_pool)


#### Understorey P pool, assume both species contributed equally
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



### 3.18 Coarse root P pool
### currently assuming sapwood P concentration
coarse_root_p_pool <- make_coarse_root_p_pool(p_conc=sapwood_p_concentration,
                                              c_pool=coarse_root_c_pool,
                                              c_frac=c_fraction)





############################## P fluxes ###############################

#### Soil P mineralization flux
#### Note:
#### It is assumed that the mineralization data is for top 10 cm only!
#### Also, this is no longer mineralization flux as according to Yolima's new definition
#### Will need to consult Yolima to revise this flux.
soil_p_mineralization <- make_soil_p_mineralization_flux(soil_bulk_density)


#### Soil P leaching rate
soil_p_leaching <- make_soil_p_leaching_flux()


#### Canopy production flux
canopy_p_flux <- make_canopy_p_production(p_conc=canopy_p_concentration,
                                          c_flux=canopy_c_production_flux,
                                          c_frac=c_fraction)


## considered both change in LAI and litterfall
canopy_p_flux_new <- make_canopy_p_production_new(c_flux=canopy_c_production_flux_new,
                                                  c_frac=c_fraction)


make_canopy_P_flux_comparison(inDF1=canopy_p_flux,
                              inDF2=canopy_p_flux_new,
                              plot.option = T)

#### Frass P production
#### Used C fraction for frass to convert c production back to frass biomass
#### We have more p conc than c flux so no need to gap fill. 
#### This needs to be added to new canopy p production requirement and the
#### total P budget. 
frass_c_fraction <- make_frass_c_fraction()
frass_p_production <- make_frass_p_production_flux(p_conc=frass_p_concentration,
                                                   c_flux=frass_c_production_flux,
                                                   c_frac=frass_c_fraction)


#### 3.8 Litter P production flux 
#### Literfall biomass (not C) will be calculated within the function
#### for data points where we have C but not P, we can create a separte script
#### and gap-fill P concentration based on average values
leaflitter_p_flux <- make_leaflitter_p_flux(p_conc=leaflitter_p_concentration,
                                            c_flux=leaflitter_c_production_flux,
                                            c_frac=c_fraction)  

#leaflitter_p_flux_gap_fill <- make_leaflitter_p_flux_gap_fill(p_conc=leaflitter_p_concentration) 


#### Fine root P production flux
fineroot_p_production <- make_fineroot_p_production(p_conc=fineroot_p_concentration,
                                                    c_flux=fineroot_c_production_flux)


#### Fine root litter P production
### assuming P retranslocation coefficient for fine root is 50%
### and fine root c production flux is fine root c litter flux
fineroot_litter_p_flux <- make_fineroot_litter_p_production(p_conc=fineroot_p_concentration,
                                                            c_flux=fineroot_c_production_flux,
                                                            p_retrans=retrans_froot)


#### Other litterfall
twig_litter_p_flux <- make_twiglitter_p_flux(p_conc=sapwood_p_concentration, 
                                             litter_flux=twiglitter_c_production_flux)  


## bark P concentration provided by Kristine
bark_litter_p_flux <- make_barklitter_p_flux(p_conc=sapwood_p_concentration, 
                                             litter_flux=barklitter_c_production_flux)  

## assume leaf p concentration
seed_litter_p_flux <- make_seedlitter_p_flux(p_conc=canopy_p_concentration, 
                                             litter_flux=seedlitter_c_production_flux)  


#### Wood p flux
wood_p_flux <- make_wood_p_production(p_conc=sapwood_p_concentration,
                                      c_flux=wood_c_production)


### Coarse root P flux
coarse_root_p_flux <- make_coarse_root_p_flux(p_conc=sapwood_p_concentration,
                                              c_flux=coarse_root_c_flux,
                                              c_frac=c_fraction)



#### Understorey production flux
#### Here we can use either stereo camera estimate of biomass (2) or
#### Harvest biomass data (1) to calculate p flux
#### Currently, we are using harvest estimate
understorey_p_flux <- make_understorey_p_flux(p_conc=understorey_p_concentration,
                                              c_flux=understorey_c_flux_clipping,
                                              c_frac=c_fraction_ud)


understorey_litter_p_flux <- make_understorey_litter_p_flux(p_conc=understorey_litter_p_concentration,
                                                            c_flux=understorey_c_flux_clipping,
                                                            #c_flux=understorey_litter_c_flux,
                                                            c_frac=c_fraction_ud)



############################## P retranslocation fluxes ###############################

canopy_P_retranslocation_flux <- calculate_canopy_P_retranslocation_flux(tflux=canopy_p_flux,
                                                                         lflux=leaflitter_p_flux,
                                                                         retransDF=plant_p_retranslocation_coefficients)


fineroot_P_retranslocation_flux <- calculate_fineroot_P_retranslocation_flux(tflux=fineroot_p_production,
                                                                             lflux=fineroot_litter_p_flux,
                                                                             retransDF=plant_p_retranslocation_coefficients)


understorey_P_retranslocation_flux <- calculate_understorey_P_retranslocation_flux(tflux=understorey_p_flux,
                                                                                   lflux=understorey_litter_p_flux,
                                                                                   retransDF=plant_p_retranslocation_coefficients)


sapwood_P_retranslocation_flux <- calculate_sapwood_P_retranslocation_flux(tflux=wood_p_flux,
                                                                           retransDF=plant_p_retranslocation_coefficients)


coarseroot_P_retranslocation_flux <- calculate_coarseroot_P_retranslocation_flux(tflux=coarse_root_p_flux,
                                                                                 retransDF=plant_p_retranslocation_coefficients)


############################## delta P Pools ###############################

delta_soil_p_pool <- make_yearly_delta_pool_function(inDF=soil_p_pool, 
                                                     var.col=3)

delta_canopy_p_pool <- make_yearly_delta_pool_function(inDF=canopy_p_pool, 
                                                       var.col=3)

delta_wood_p_pool <- make_yearly_delta_pool_function(inDF=wood_p_pool, 
                                                     var.col=3)

delta_fineroot_p_pool <- make_yearly_delta_pool_function(inDF=fineroot_p_pool, 
                                                         var.col=3)

delta_coarse_root_p_pool <- make_yearly_delta_pool_function(inDF=coarse_root_p_pool, 
                                                            var.col=3)

delta_understorey_p_pool <- make_yearly_delta_pool_function(inDF=understorey_p_pool, 
                                                            var.col=3)

delta_microbial_p_pool <- make_yearly_delta_pool_function(inDF=microbial_p_pool, 
                                                          var.col=3)




########################################################################################## 
########################################################################################## 
#####
##### Step 5: Making P budgeting variables and tables, based on raw data
#####

############################## summary tables ###############################

#### Summary Tables
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


############################## Budget tables ###############################

### vegetation standing P stocks
vegetation_standing_p_stock <- make_vegetation_standing_p_stock(leaf=canopy_p_pool,
                                                                wood=wood_p_pool,
                                                                fineroot=fineroot_p_pool,
                                                                coarseroot=coarse_root_p_pool,
                                                                understorey=understorey_p_pool,
                                                                dead=standing_dead_p_pool,
                                                                forestfloor=leaflitter_p_pool)



### P mean residence time in plant
plant_p_MRT <- make_plant_P_mean_residence_time(p_stand=vegetation_standing_p_stock,
                                                p_flux=summary_table_flux)

### Plant P use efficiency
plant_p_use_efficiency <- make_plant_P_use_efficiency(c_flux=summary_table_c_flux,
                                                      p_flux=summary_table_flux)


#### P budget summary
total_p_budget <- make_total_p_budget(summary_table_flux,
                                      summary_table_pool,
                                      vegetation_standing_p_stock,
                                      plant_p_MRT,
                                      plant_p_use_efficiency)


########################################################################################## 
########################################################################################## 
#####
##### Step 6. Plotting P budget figures, based on unnormalized data
#####
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

### compare microbial CNP vs global dataset (Xu et al., 2013)
microbial_concentration_global_comparison(microbial_p_concentration)





########################################################################################## 
########################################################################################## 
#####
###### Step 7: Normalize all responses to a pretreatment soil conditions
#
#### Summary Tables
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
###### Step 8. Plotting P budget figures, based on normalized responses
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

