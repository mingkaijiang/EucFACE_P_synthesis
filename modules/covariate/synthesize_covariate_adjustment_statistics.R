synthesize_covariate_adjustment_statistics <- function(corDF.adj,
                                                       return.outcome="model",
                                                       plot.comparison=T,
                                                       covariate.name,
                                                       canopy_c_pool,
                                                       wood_c_pool,
                                                       standing_dead_c_pool,
                                                       fineroot_c_pool,
                                                       coarse_root_c_pool,
                                                       understorey_c_pool,
                                                       soil_c_pool,
                                                       microbial_c_pool,
                                                       mycorrhizal_c_pool,
                                                       leaflitter_c_pool,
                                                       leaflitter_c_production_flux,
                                                       twiglitter_c_production_flux,
                                                       barklitter_c_production_flux,
                                                       seedlitter_c_production_flux,
                                                       canopy_c_production_flux,
                                                       wood_c_production,
                                                       fineroot_c_production_flux,
                                                       coarse_root_c_flux,
                                                       understorey_c_flux_clipping,
                                                       understorey_litter_c_flux,
                                                       frass_c_production_flux,
                                                       soil_p_pool,
                                                       soil_inorganic_p_pool,
                                                       soil_organic_p_pool,
                                                       soil_phosphate_pool,
                                                       soil_p_pool_hedley,
                                                       microbial_p_pool,
                                                       canopy_p_pool,
                                                       leaflitter_p_pool,
                                                       wood_p_pool,
                                                       sapwood_p_pool,
                                                       heartwood_p_pool,
                                                       standing_dead_p_pool,
                                                       fineroot_p_pool,
                                                       understorey_p_pool,
                                                       coarse_root_p_pool,
                                                       soil_p_mineralization,
                                                       soil_p_leaching,
                                                       canopy_p_flux,
                                                       frass_p_production,
                                                       leaflitter_p_flux,
                                                       fineroot_p_production,
                                                       fineroot_litter_p_flux,
                                                       twig_litter_p_flux,
                                                       bark_litter_p_flux,
                                                       seed_litter_p_flux,
                                                       wood_p_flux,
                                                       coarse_root_p_flux,
                                                       understorey_p_flux,
                                                       understorey_litter_p_flux) {
    
    ### Need to read in all the pools and fluxes calculated at the main script
    ### then set var.col, with.depth.profile for each variable
    
    ### canopy c pool
    canopy_c_pool_stat <- adjust_p_variables_with_covariate(inDF=canopy_c_pool, 
                                                            corDF.adj=corDF.adj, 
                                                            var.col=3,
                                                            poolORflux="pool",
                                                            with.depth.profile=F,
                                                            plot.comparison=plot.comparison,
                                                            return.outcome=return.outcome) 
    
    
    #### Wood C pool
    wood_c_pool_stat <- adjust_p_variables_with_covariate(inDF=wood_c_pool, 
                                                          corDF.adj=corDF.adj, 
                                                          var.col=3,
                                                          poolORflux="pool",
                                                          with.depth.profile=F,
                                                          plot.comparison=plot.comparison,
                                                          return.outcome=return.outcome) 
    
    
    
    ### standing dead wood c pool
    #standing_dead_c_pool_stat <- adjust_p_variables_with_covariate(inDF=standing_dead_c_pool, 
    #                                                               corDF.adj=corDF.adj, 
    #                                                               var.col=3,
    #                                                               with.depth.profile=F,
    #                                                               plot.comparison=plot.comparison,
    #                                                               return.outcome=return.outcome) 
    
    
    
    #### Fineroot pool
    # top 60 cm
    fineroot_c_pool_stat <- adjust_p_variables_with_covariate(inDF=fineroot_c_pool, 
                                                         corDF.adj=corDF.adj, 
                                                         var.col=3,
                                                         poolORflux="pool",
                                                         with.depth.profile=F,
                                                         plot.comparison=plot.comparison,
                                                         return.outcome=return.outcome) 
    
    
    #### Understorey aboveground biomass 
    ### estimate % live and % dead
    understorey_c_pool_stat <- adjust_p_variables_with_covariate(inDF=understorey_c_pool, 
                                                                 corDF.adj=corDF.adj, 
                                                                 var.col=5,
                                                                 poolORflux="pool",
                                                                 with.depth.profile=F,
                                                                 plot.comparison=plot.comparison,
                                                                 return.outcome=return.outcome) 
    
    
    #### Soil C content
    #### return soil C by depths
    soil_c_pool_stat <- adjust_p_variables_with_covariate(inDF=soil_c_pool, 
                                                          corDF.adj=corDF.adj, 
                                                          var.col=4,
                                                          poolORflux="pool",
                                                          with.depth.profile=T,
                                                          plot.comparison=plot.comparison,
                                                          return.outcome=return.outcome) 
    
    
    #### Microbial C pool
    #### this pool has data only at 0-10cm depth - Cat's data
    microbial_c_pool_stat <- adjust_p_variables_with_covariate(inDF=microbial_c_pool, 
                                                               corDF.adj=corDF.adj, 
                                                               var.col=4,
                                                               poolORflux="pool",
                                                               with.depth.profile=T,
                                                               plot.comparison=plot.comparison,
                                                               return.outcome=return.outcome) 
    
    #### Soil mycorrhizal pool
    #### But we don't have a P concentration for it and 
    #### therefore it's not included in the P budget
    mycorrhizal_c_pool_stat <- adjust_p_variables_with_covariate(inDF=mycorrhizal_c_pool, 
                                                                 corDF.adj=corDF.adj, 
                                                                 var.col=4,
                                                                 poolORflux="pool",
                                                                 with.depth.profile=T,
                                                                 plot.comparison=plot.comparison,
                                                                 return.outcome=return.outcome) 
    
    
    #### Coarse root C pool 
    coarse_root_c_pool_stat <- adjust_p_variables_with_covariate(inDF=coarse_root_c_pool, 
                                                                 corDF.adj=corDF.adj, 
                                                                 var.col=3,
                                                                 poolORflux="pool",
                                                                 with.depth.profile=F,
                                                                 plot.comparison=plot.comparison,
                                                                 return.outcome=return.outcome) 
    
    
    #### Leaf litter pool - forest floor leaf litter pool
    leaflitter_c_pool_stat <- adjust_p_variables_with_covariate(inDF=leaflitter_c_pool, 
                                                                corDF.adj=corDF.adj, 
                                                                var.col=6,
                                                                poolORflux="pool",
                                                                with.depth.profile=F,
                                                                plot.comparison=plot.comparison,
                                                                return.outcome=return.outcome) 
    
    
    
    ############################## Fluxes ###############################
    
    #### Ltter production (leaf, twig, bark, seed)
    leaflitter_c_production_flux_stat <- adjust_p_variables_with_covariate(inDF=leaflitter_c_production_flux, 
                                                                           corDF.adj=corDF.adj, 
                                                                           var.col=3,
                                                                           poolORflux="flux",
                                                                           with.depth.profile=F,
                                                                           plot.comparison=plot.comparison,
                                                                           return.outcome=return.outcome) 
    
    
    twiglitter_c_production_flux_stat <- adjust_p_variables_with_covariate(inDF=twiglitter_c_production_flux, 
                                                                           corDF.adj=corDF.adj, 
                                                                           var.col=3,
                                                                           poolORflux="flux",
                                                                           with.depth.profile=F,
                                                                           plot.comparison=plot.comparison,
                                                                           return.outcome=return.outcome) 
    
    
    barklitter_c_production_flux_stat <- adjust_p_variables_with_covariate(inDF=barklitter_c_production_flux, 
                                                                           corDF.adj=corDF.adj, 
                                                                           var.col=3,
                                                                           poolORflux="flux",
                                                                           with.depth.profile=F,
                                                                           plot.comparison=plot.comparison,
                                                                           return.outcome=return.outcome) 
    
    
    seedlitter_c_production_flux_stat <- adjust_p_variables_with_covariate(inDF=seedlitter_c_production_flux, 
                                                                           corDF.adj=corDF.adj, 
                                                                           var.col=3,
                                                                           poolORflux="flux",
                                                                           with.depth.profile=F,
                                                                           plot.comparison=plot.comparison,
                                                                           return.outcome=return.outcome) 
    
    #### 2.3 Canopy C production
    ## assume it's the same as litterfall C production
    canopy_c_production_flux_stat <- adjust_p_variables_with_covariate(inDF=canopy_c_production_flux, 
                                                                       corDF.adj=corDF.adj, 
                                                                       var.col=3,
                                                                       poolORflux="flux",
                                                                       with.depth.profile=F,
                                                                       plot.comparison=plot.comparison,
                                                                       return.outcome=return.outcome) 
    
    #### 2.5 Wood C production
    wood_c_production_stat <- adjust_p_variables_with_covariate(inDF=wood_c_production, 
                                                                corDF.adj=corDF.adj, 
                                                                var.col=5,
                                                                poolORflux="flux",
                                                                with.depth.profile=F,
                                                                plot.comparison=plot.comparison,
                                                                return.outcome=return.outcome) 
    
    
    #### Fineroot production
    #### we can only estimate fineroot c production for top 30 cm,
    #### but the 30 - 60 cm fienroot biomass is tiny (3% of total), and therefore 
    #### we assume the production flux is small and negligible
    ####
    fineroot_c_production_flux_stat <- adjust_p_variables_with_covariate(inDF=fineroot_c_production_flux, 
                                                                         corDF.adj=corDF.adj, 
                                                                         var.col=5,
                                                                         poolORflux="flux",
                                                                         with.depth.profile=F,
                                                                         plot.comparison=plot.comparison,
                                                                         return.outcome=return.outcome) 
    
    
    
    #### Understorey production flux 
    #### - 1: Varsha's clipping
    #### - 2: Matthias's stereo camera
    understorey_c_flux_clipping_stat <- adjust_p_variables_with_covariate(inDF=understorey_c_flux_clipping, 
                                                                          corDF.adj=corDF.adj, 
                                                                          var.col=5,
                                                                          poolORflux="flux",
                                                                          with.depth.profile=F,
                                                                          plot.comparison=plot.comparison,
                                                                          return.outcome=return.outcome) 
    
    
    
    #### understorey litter flux
    ### basically understorey dead 
    understorey_litter_c_flux_stat <- adjust_p_variables_with_covariate(inDF=understorey_litter_c_flux, 
                                                                        corDF.adj=corDF.adj, 
                                                                        var.col=5,
                                                                        poolORflux="flux",
                                                                        with.depth.profile=F,
                                                                        plot.comparison=plot.comparison,
                                                                        return.outcome=return.outcome) 
    
    
    #### Frass production
    frass_c_production_flux_stat <- adjust_p_variables_with_covariate(inDF=frass_c_production_flux, 
                                                                      corDF.adj=corDF.adj, 
                                                                      var.col=5,
                                                                      poolORflux="flux",
                                                                      with.depth.profile=F,
                                                                      plot.comparison=plot.comparison,
                                                                      return.outcome=return.outcome) 
    
    
    #### Coarse root C production
    coarse_root_c_flux_stat <- adjust_p_variables_with_covariate(inDF=coarse_root_c_flux, 
                                                                 corDF.adj=corDF.adj, 
                                                                 var.col=5,
                                                                 poolORflux="flux",
                                                                 with.depth.profile=F,
                                                                 plot.comparison=plot.comparison,
                                                                 return.outcome=return.outcome) 
    
    
    
    
    ########################################################################################## 
    ########################################################################################## 
    #####
    ##### Step 4: Generating P pools and fluxes
    #####
    
    
    ############################## P Pools ###############################
    
    #### Soil P pool - top 60 cm
    soil_p_pool_stat <- adjust_p_variables_with_covariate(inDF=soil_p_pool, 
                                                          corDF.adj=corDF.adj, 
                                                          var.col=4,
                                                          poolORflux="pool",
                                                          with.depth.profile=T,
                                                          plot.comparison=plot.comparison,
                                                          return.outcome=return.outcome) 
    
    ### Soil inorganic pool - 60 cm
    soil_inorganic_p_pool_stat <- adjust_p_variables_with_covariate(inDF=soil_inorganic_p_pool, 
                                                                    corDF.adj=corDF.adj, 
                                                                    var.col=4,
                                                                    poolORflux="pool",
                                                                    ignore.date=T,
                                                                    with.depth.profile=T,
                                                                    plot.comparison=plot.comparison,
                                                                    return.outcome=return.outcome) 
    
    
    ### soil organic pool - 60 cm
    soil_organic_p_pool_stat <- adjust_p_variables_with_covariate(inDF=soil_organic_p_pool, 
                                                                  corDF.adj=corDF.adj, 
                                                                  var.col=4,
                                                                  poolORflux="pool",
                                                                  ignore.date=T,
                                                                  with.depth.profile=T,
                                                                  plot.comparison=plot.comparison,
                                                                  return.outcome=return.outcome) 
    
    #### Soil phosphate pool
    #### Top 60 cm
    #### Note:
    #### This is additional to the microbial PO4-P pool
    #### The microbial pool needs to have one step further 
    #### to break the cell when extracting.
    #### So this pool is more readily available to plants. 
    soil_phosphate_pool_stat <- adjust_p_variables_with_covariate(inDF=soil_phosphate_pool, 
                                                                  corDF.adj=corDF.adj, 
                                                                  var.col=4,
                                                                  poolORflux="pool",
                                                                  with.depth.profile=T,
                                                                  plot.comparison=plot.comparison,
                                                                  return.outcome=return.outcome) 
    
    
    #### Soil P pool of different bioavailability
    #### Top 10 cm only
    #soil_p_pool_hedley_stat <- adjust_p_variables_with_covariate(inDF=soil_p_pool_hedley, 
    #                                                             corDF.adj=corDF.adj, 
    #                                                             var.col=3,
    #                                                             with.depth.profile=F,
    #                                                             plot.comparison=plot.comparison,
    #                                                             return.outcome=return.outcome) 
    
    
    #### Microbial P pool 
    #### Top 60 cm
    microbial_p_pool_stat <- adjust_p_variables_with_covariate(inDF=microbial_p_pool, 
                                                               corDF.adj=corDF.adj, 
                                                               var.col=4,
                                                               poolORflux="pool",
                                                               with.depth.profile=T,
                                                               plot.comparison=plot.comparison,
                                                               return.outcome=return.outcome) 
    
    
    
    #### Canopy P pool - only for green leaves
    canopy_p_pool_stat <- adjust_p_variables_with_covariate(inDF=canopy_p_pool, 
                                                            corDF.adj=corDF.adj, 
                                                            var.col=3,
                                                            poolORflux="pool",
                                                            with.depth.profile=F,
                                                            plot.comparison=plot.comparison,
                                                            return.outcome=return.outcome) 
    
    
    ### Forest floor leaf litter pool
    leaflitter_p_pool_stat <- adjust_p_variables_with_covariate(inDF=leaflitter_p_pool, 
                                                                corDF.adj=corDF.adj, 
                                                                var.col=3,
                                                                poolORflux="pool",
                                                                with.depth.profile=F,
                                                                plot.comparison=plot.comparison,
                                                                return.outcome=return.outcome) 
    
    
    
    #### Wood P pool 
    wood_p_pool_stat <- adjust_p_variables_with_covariate(inDF=wood_p_pool, 
                                                          corDF.adj=corDF.adj, 
                                                          var.col=3,
                                                          poolORflux="pool",
                                                          with.depth.profile=F,
                                                          plot.comparison=plot.comparison,
                                                          return.outcome=return.outcome) 
    
    sapwood_p_pool_stat <- adjust_p_variables_with_covariate(inDF=sapwood_p_pool, 
                                                             corDF.adj=corDF.adj, 
                                                             var.col=3,
                                                             poolORflux="pool",
                                                             with.depth.profile=F,
                                                             plot.comparison=plot.comparison,
                                                             return.outcome=return.outcome) 
    
    
    heartwood_p_pool_stat <- adjust_p_variables_with_covariate(inDF=heartwood_p_pool, 
                                                               corDF.adj=corDF.adj, 
                                                               var.col=3,
                                                               poolORflux="pool",
                                                               with.depth.profile=F,
                                                               plot.comparison=plot.comparison,
                                                               return.outcome=return.outcome) 
    
    
    #### Standing dead p pool
    #standing_dead_p_pool_stat <- adjust_p_variables_with_covariate(inDF=standing_dead_p_pool, 
    #                                                               corDF.adj=corDF.adj, 
    #                                                               var.col=3,
    #                                                               with.depth.profile=F,
    #                                                               plot.comparison=plot.comparison,
    #                                                               return.outcome=return.outcome) 
    
    
    #### Fine root P biomass pool
    fineroot_p_pool_stat <- adjust_p_variables_with_covariate(inDF=fineroot_p_pool, 
                                                              corDF.adj=corDF.adj, 
                                                              var.col=3,
                                                              poolORflux="pool",
                                                              with.depth.profile=F,
                                                              plot.comparison=plot.comparison,
                                                              return.outcome=return.outcome) 
    
    
    #### Understorey P pool, assume both species contributed equally
    #### Also because p_conc and c_pool do not match in time,
    #### we are taking the average of p_conc and apply it to c_pool
    #### Here we can use Matthias's stereo camera estimate (2) or 
    #### Varsha's harvest data (1) to extrapolate for p pool
    #### It makes more sense to use harvest at this stage in time!
    #### Also, if use Varsha's harvest data, we can use either total or live part of biomass
    understorey_p_pool_stat <- adjust_p_variables_with_covariate(inDF=understorey_p_pool, 
                                                                 corDF.adj=corDF.adj, 
                                                                 var.col=3,
                                                                 poolORflux="pool",
                                                                 with.depth.profile=F,
                                                                 plot.comparison=plot.comparison,
                                                                 return.outcome=return.outcome) 
    
    
    
    ### 3.18 Coarse root P pool
    ### currently assuming sapwood P concentration
    coarse_root_p_pool_stat <- adjust_p_variables_with_covariate(inDF=coarse_root_p_pool, 
                                                                 corDF.adj=corDF.adj, 
                                                                 var.col=3,
                                                                 poolORflux="pool",
                                                                 with.depth.profile=F,
                                                                 plot.comparison=plot.comparison,
                                                                 return.outcome=return.outcome) 
    
    
    
    
    
    ############################## P fluxes ###############################
    
    #### Soil P mineralization flux
    #### Note:
    #### It is assumed that the mineralization data is for 60 cm depth.
    #### extrapolated based on data from top 10 cm. 
    #### This is the net mineralization flux (i.e. gross - immobilization)
    soil_p_mineralization_stat <- adjust_p_variables_with_covariate(inDF=soil_p_mineralization, 
                                                                    corDF.adj=corDF.adj, 
                                                                    var.col=4,
                                                                    poolORflux="flux",
                                                                    with.depth.profile=T,
                                                                    plot.comparison=plot.comparison,
                                                                    return.outcome=return.outcome) 
    
    #### Soil P leaching rate
    #### estimated based on deep depth (35 - 75 cm) lysimeter data
    #### and multiply by water flux of 20 mL m-2 d-1 (over-estimate)
    soil_p_leaching_stat <- adjust_p_variables_with_covariate(inDF=soil_p_leaching, 
                                                              corDF.adj=corDF.adj, 
                                                              var.col=5,
                                                              poolORflux="flux",
                                                              with.depth.profile=F,
                                                              plot.comparison=plot.comparison,
                                                              return.outcome=return.outcome) 
    
    
    #### Canopy production flux
    canopy_p_flux_stat <- adjust_p_variables_with_covariate(inDF=canopy_p_flux, 
                                                            corDF.adj=corDF.adj, 
                                                            var.col=5,
                                                            poolORflux="flux",
                                                            with.depth.profile=F,
                                                            plot.comparison=plot.comparison,
                                                            return.outcome=return.outcome) 
    
    
    
    #### Frass P production
    #### Used C fraction for frass to convert c production back to frass biomass
    #### We have more p conc than c flux so no need to gap fill. 
    #### This needs to be added to new canopy p production requirement and the
    #### total P budget. 
    frass_p_production_stat <- adjust_p_variables_with_covariate(inDF=frass_p_production, 
                                                                 corDF.adj=corDF.adj, 
                                                                 var.col=5,
                                                                 poolORflux="flux",
                                                                 with.depth.profile=F,
                                                                 plot.comparison=plot.comparison,
                                                                 return.outcome=return.outcome) 
    
    
    #### 3.8 Litter P production flux 
    #### Literfall biomass (not C) will be calculated within the function
    #### for data points where we have C but not P, we can create a separte script
    #### and gap-fill P concentration based on average values
    leaflitter_p_flux_stat <- adjust_p_variables_with_covariate(inDF=leaflitter_p_flux, 
                                                                corDF.adj=corDF.adj, 
                                                                var.col=5,
                                                                poolORflux="flux",
                                                                with.depth.profile=F,
                                                                plot.comparison=plot.comparison,
                                                                return.outcome=return.outcome) 
    
    
    #### Fine root P production flux
    fineroot_p_production_stat <- adjust_p_variables_with_covariate(inDF=fineroot_p_production, 
                                                                    corDF.adj=corDF.adj, 
                                                                    var.col=5,
                                                                    poolORflux="flux",
                                                                    with.depth.profile=F,
                                                                    plot.comparison=plot.comparison,
                                                                    return.outcome=return.outcome) 
    
    #### Fine root litter P production
    ### assuming P retranslocation coefficient for fine root is 50%
    ### and fine root c production flux is fine root c litter flux
    fineroot_litter_p_flux_stat <- adjust_p_variables_with_covariate(inDF=fineroot_litter_p_flux, 
                                                                     corDF.adj=corDF.adj, 
                                                                     var.col=5,
                                                                     poolORflux="flux",
                                                                     with.depth.profile=F,
                                                                     plot.comparison=plot.comparison,
                                                                     return.outcome=return.outcome) 
    
    
    #### Other litterfall
    twig_litter_p_flux_stat <- adjust_p_variables_with_covariate(inDF=twig_litter_p_flux, 
                                                                 corDF.adj=corDF.adj, 
                                                                 var.col=5,
                                                                 poolORflux="flux",
                                                                 with.depth.profile=F,
                                                                 plot.comparison=plot.comparison,
                                                                 return.outcome=return.outcome)   
    
    
    ## bark P concentration provided by Kristine
    bark_litter_p_flux_stat <- adjust_p_variables_with_covariate(inDF=bark_litter_p_flux, 
                                                                 corDF.adj=corDF.adj, 
                                                                 var.col=5,
                                                                 poolORflux="flux",
                                                                 with.depth.profile=F,
                                                                 plot.comparison=plot.comparison,
                                                                 return.outcome=return.outcome)   
    
    ## assume leaf p concentration
    seed_litter_p_flux_stat <- adjust_p_variables_with_covariate(inDF=seed_litter_p_flux, 
                                                                 corDF.adj=corDF.adj, 
                                                                 var.col=5,
                                                                 poolORflux="flux",
                                                                 with.depth.profile=F,
                                                                 plot.comparison=plot.comparison,
                                                                 return.outcome=return.outcome)   
    
    
    #### Wood p flux
    wood_p_flux_stat <- adjust_p_variables_with_covariate(inDF=wood_p_flux, 
                                                          corDF.adj=corDF.adj, 
                                                          var.col=5,
                                                          poolORflux="flux",
                                                          with.depth.profile=F,
                                                          plot.comparison=plot.comparison,
                                                          return.outcome=return.outcome) 
    
    
    ### Coarse root P flux
    coarse_root_p_flux_stat <- adjust_p_variables_with_covariate(inDF=coarse_root_p_flux, 
                                                                 corDF.adj=corDF.adj, 
                                                                 var.col=5,
                                                                 poolORflux="flux",
                                                                 with.depth.profile=F,
                                                                 plot.comparison=plot.comparison,
                                                                 return.outcome=return.outcome) 
    
    
    
    #### Understorey production flux
    #### Here we can use either stereo camera estimate of biomass (2) or
    #### Harvest biomass data (1) to calculate p flux
    #### Currently, we are using harvest estimate
    understorey_p_flux_stat <- adjust_p_variables_with_covariate(inDF=understorey_p_flux, 
                                                                 corDF.adj=corDF.adj, 
                                                                 var.col=5,
                                                                 poolORflux="flux",
                                                                 with.depth.profile=F,
                                                                 plot.comparison=plot.comparison,
                                                                 return.outcome=return.outcome) 
    
    
    understorey_litter_p_flux_stat <- adjust_p_variables_with_covariate(inDF=understorey_litter_p_flux, 
                                                                        corDF.adj=corDF.adj, 
                                                                        var.col=5,
                                                                        poolORflux="flux",
                                                                        with.depth.profile=F,
                                                                        plot.comparison=plot.comparison,
                                                                        return.outcome=return.outcome) 
    
    
    
    
    
    
    
    #############################################################
    ### now synthesize all variables together
    
    ### prepare storageDF
    name.list <- c("canopy_c_pool",
                   "wood_c_pool",
                   "standing_dead_c_pool",
                   "fineroot_c_pool",
                   "coarse_root_c_pool",
                   "understorey_c_pool",
                   "soil_c_pool",
                   "microbial_c_pool",
                   "mycorrhizal_c_pool",
                   "leaflitter_c_pool",
                   "leaflitter_c_production_flux",
                   "twiglitter_c_production_flux",
                   "barklitter_c_production_flux",
                   "seedlitter_c_production_flux",
                   "canopy_c_production_flux",
                   "wood_c_production",
                   "fineroot_c_production_flux",
                   "coarse_root_c_flux",
                   "understorey_c_flux_clipping",
                   "understorey_litter_c_flux",
                   "frass_c_production_flux",
                   "soil_p_pool",
                   "soil_inorganic_p_pool",
                   "soil_organic_p_pool",
                   "soil_phosphate_pool",
                   "soil_p_pool_hedley",
                   "microbial_p_pool",
                   "canopy_p_pool",
                   "leaflitter_p_pool",
                   "wood_p_pool",
                   "sapwood_p_pool",
                   "heartwood_p_pool",
                   "standing_dead_p_pool",
                   "fineroot_p_pool",
                   "understorey_p_pool",
                   "coarse_root_p_pool",
                   "soil_p_mineralization",
                   "soil_p_leaching",
                   "canopy_p_flux",
                   "frass_p_production",
                   "leaflitter_p_flux",
                   "fineroot_p_production",
                   "fineroot_litter_p_flux",
                   "twig_litter_p_flux",
                   "bark_litter_p_flux",
                   "seed_litter_p_flux",
                   "wood_p_flux",
                   "coarse_root_p_flux",
                   "understorey_p_flux",
                   "understorey_litter_p_flux")
    
    outDF <- data.frame("Variable" = name.list,
                        "Effect_size" = NA,
                        "CI_low" = NA,
                        "CI_high" = NA)
    
    
    ### assign values
    for (i in name.list) {
        tryCatch({
            tmpDF <- get(paste0(i, "_stat"))
            outDF$Effect_size[outDF$Variable==i]<-tmpDF$eff
            outDF$CI_low[outDF$Variable==i]<-tmpDF$conf[1]
            outDF$CI_high[outDF$Variable==i]<-tmpDF$conf[2]
        }, error=function(e){})
    }
    
    ### save output
    write.csv(outDF, paste0("plots_tables/covariate/stats_with_", covariate.name, ".csv"), 
              row.names=F)
    
}
