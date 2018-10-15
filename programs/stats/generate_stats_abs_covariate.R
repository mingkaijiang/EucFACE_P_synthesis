generate_stats_abs_covariate <- function(stat.model) {
    
    ### remove pre-treatment data period for individual variables (e.g. leaf, wood, soil, mic, myc)
    
    ######## Concentration
    ### Soil P conc
    s.soilp.conc <- make_soilp_conc_treatment_abs_effect_statistics(inDF=soil_p_concentration, 
                                                          var.col=3,
                                                          stat.model="no_interaction_with_covariate",
                                                          return.outcome="model")
    
    ### Soil Phosphate conc
    s.soil.phosphate.p.conc <- make_soilp_conc_treatment_abs_effect_statistics(inDF=soil_phosphate_concentration, 
                                                                    var.col=3,
                                                                    stat.model="no_interaction_with_covariate",
                                                                    return.outcome="model")
    
    ### Overstorey Leaf P conc
    s.leafp.conc <- make_leafp_conc_treatment_abs_effect_statistics(inDF=canopy_p_concentration, 
                                                          var.col=3,
                                                          stat.model="no_interaction_with_covariate",
                                                          return.outcome="model")
    
    ### Overstorey Leaf litter P conc
    s.leafp.lit.conc <- make_leafp_conc_treatment_abs_effect_statistics(inDF=leaflitter_p_concentration, 
                                                                    var.col=3,
                                                                    stat.model="no_interaction_with_covariate",
                                                                    return.outcome="model")
    
    
    ### To do list:
    ### 1. finish all conc, delta conc, fluxes, pools, delta pools stats
    ### 2. Create figures for stats summary
    ### 3. P budget figure (conc, fluxes and stocks)
    
    
    ### Wood P conc
    ### we do not have enough data to perform lmer for wood P concentration

    ### Fineroot P conc
    s.frp.conc <- make_frootp_conc_treatment_abs_effect_statistics(inDF=fineroot_p_concentration, 
                                                         var.col=3,
                                                         stat.model="no_interaction_with_covariate",
                                                         return.outcome="model")

    ### Understorey aboveground P conc
    s.uap.conc <- make_uap_conc_treatment_abs_effect_statistics(inDF=understorey_p_concentration, 
                                                      var.col=3,
                                                      stat.model="no_interaction_with_covariate",
                                                      return.outcome="model")
    
    ### Understorey aboveground litter P conc
    ### not possible to construct a model due to limited data

    ### Microbial P conc
    s.micp.conc <- make_micp_conc_treatment_abs_effect_statistics(inDF=microbial_p_concentration, 
                                                        var.col=3,
                                                        stat.model="no_interaction_with_covariate",
                                                        return.outcome="model")

    ### Mycorrhizal P conc
    #s.mycp.conc <- make_mycp_conc_treatment_abs_effect_statistics(inDF=mycorrhizal_p_concentration, 
    #                                                    var.col=3,
    #                                                    stat.model="no_interaction_with_covariate",
    #                                                    return.outcome="model")

    ### Leaf litter P conc
    s.litp.conc <- make_leafp_conc_treatment_abs_effect_statistics(inDF=canopy_p_concentration, 
                                                              var.col=3,
                                                              stat.model="no_interaction_with_covariate",
                                                              return.outcome="model")

    ### Frass P concentration
    s.frasp.conc <- make_frassp_conc_treatment_abs_effect_statistics(inDF=frass_p_concentration, 
                                                         var.col=3,
                                                         stat.model="no_interaction_with_covariate",
                                                         return.outcome="model")
 
    
    ######## P fluxes and stocks
    ### Soil P pool
    s.soilp.pool <- make_soilp_conc_treatment_abs_effect_statistics(inDF=soil_p_pool, 
                                                                    var.col=3,
                                                                    stat.model="no_interaction_with_covariate",
                                                                    return.outcome="model")
    
    ### Soil Phosphate pool
    s.soil.phosphate.p.pool <- make_soilp_conc_treatment_abs_effect_statistics(inDF=soil_phosphate_pool, 
                                                                               var.col=3,
                                                                               stat.model="no_interaction_with_covariate",
                                                                               return.outcome="model")
    
    ### Overstorey Leaf P pool
    s.leafp.pool <- make_leafp_conc_treatment_abs_effect_statistics(inDF=canopy_p_pool, 
                                                                    var.col=3,
                                                                    stat.model="no_interaction_with_covariate",
                                                                    return.outcome="model")
    
    ### Wood P pool
    s.woodp.pool <- make_woodp_pool_treatment_abs_effect_statistics(inDF=wood_p_pool, 
                                                                    var.col=3,
                                                                    stat.model="no_interaction_with_covariate",
                                                                    return.outcome="model")
    
    ### Fineroot P pool
    s.frp.pool <- make_frootp_conc_treatment_abs_effect_statistics(inDF=fineroot_p_pool, 
                                                                   var.col=3,
                                                                   stat.model="no_interaction_with_covariate",
                                                                   return.outcome="model")
    
    ### Understorey aboveground P pool
    s.uap.pool <- make_uap_conc_treatment_abs_effect_statistics(inDF=understorey_p_pool, 
                                                                var.col=3,
                                                                stat.model="no_interaction_with_covariate",
                                                                return.outcome="model")
    
    ### Microbial P pool
    s.micp.pool <- make_micp_conc_treatment_abs_effect_statistics(inDF=microbial_p_pool, 
                                                                  var.col=3,
                                                                  stat.model="no_interaction_with_covariate",
                                                                  return.outcome="model")
    
    ### Mycorrhizal P pool
    #s.mycp.pool <- make_mycp_conc_treatment_abs_effect_statistics(inDF=mycorrhizal_p_concentration, 
    #                                                    var.col=3,
    #                                                    stat.model="no_interaction_with_covariate",
    #                                                    return.outcome="model")
    
    ### coarse root P pool
    s.crootp.pool <- make_crootp_pool_treatment_abs_effect_statistics(inDF=coarse_root_p_pool_1, 
                                                                  var.col=3,
                                                                  stat.model="no_interaction_with_covariate",
                                                                  return.outcome="model")
    
    ### Leaf litter P flux
    s.leaf.litp.flux <- make_leaf_lit_p_flux_treatment_abs_effect_statistics(inDF=leaflitter_p_flux, 
                                                                   var.col=5,
                                                                   stat.model="no_interaction_with_covariate",
                                                                   return.outcome="model")
    
    ### twig litter flux
    s.other.litp.flux <- make_other_lit_p_flux_treatment_abs_effect_statistics(inDF=other_litter_p_flux, 
                                                                             var.col=5,
                                                                             stat.model="no_interaction_with_covariate",
                                                                             return.outcome="model")
    

    ### Frass P flux
    s.frasp.flux <- make_frassp_conc_treatment_abs_effect_statistics(inDF=frass_p_concentration, 
                                                                     var.col=3,
                                                                     stat.model="no_interaction_with_covariate",
                                                                     return.outcome="model")
    
    
    ### Wood production flux
    s.wood.prod <- make_wood_prod_treatment_abs_effect_statistics(inDF=wood_production_flux, 
                                               var.cond="flux", var.col=5,
                                               date.as.factor=T,
                                               stat.model="no_interaction_with_covariate",
                                               return.outcome="model") 
  
    ### Fineroot production flux
    s.froot.prod <- make_froot_prod_treatment_abs_effect_statistics(inDF=fineroot_production_flux, 
                                                                    var.cond="flux", var.col=5,
                                                                    date.as.factor=T,
                                                                    stat.model="no_interaction_with_covariate",
                                                                    return.outcome="model") 

    ### Coarseroot production
    s.croot.prod <- make_croot_prod_treatment_abs_effect_statistics(inDF=coarse_root_production_flux_1, 
                                                                    var.cond="flux", var.col=5,
                                                                    date.as.factor=T,
                                                                    stat.model="no_interaction_with_covariate",
                                                                    return.outcome="model")
   
    ### Understorey aboveground production
    s.und.prod <- make_und_prod_treatment_abs_effect_statistics(inDF=understorey_aboveground_production_flux, 
                                                                var.cond="flux", var.col=5,
                                                                date.as.factor=T,
                                                                stat.model="no_interaction_with_covariate",
                                                                return.outcome="model")
    
    ### Understory litter flux
    s.und.lit <- make_und_prod_treatment_abs_effect_statistics(inDF=understorey_aboveground_production_flux, 
                                                                var.cond="flux", var.col=6,
                                                                date.as.factor=T,
                                                                stat.model="no_interaction_with_covariate",
                                                                return.outcome="model")


    ### Delta Soil C
    s.delta.soilc <- make_delta_soilc_treatment_abs_effect_statistics(inDF=soil_c_pool, 
                                                          var.cond="pool", var.col=3,
                                                          date.as.factor=T,
                                                          stat.model="no_interaction_with_covariate",
                                                          return.outcome="model")
    
    ### Delta Leaf C
    s.delta.leafc <- make_delta_leafc_treatment_abs_effect_statistics(inDF=leaf_c_pool, 
                                                          var.cond="pool", var.col=3,
                                                          date.as.factor=T,
                                                          stat.model="no_interaction_with_covariate",
                                                          return.outcome="model")
    
    ### Delta Wood C pool
    s.delta.woodc <- make_delta_woodc_treatment_abs_effect_statistics(inDF=wood_c_pool, 
                                                          var.cond="pool", var.col=3,
                                                          date.as.factor=T,
                                                          stat.model="no_interaction_with_covariate",
                                                          return.outcome="model") 
    
    ### Delta Fineroot C pool
    s.delta.frc <- make_delta_frootc_treatment_abs_effect_statistics(inDF=fineroot_c_pool, 
                                                         var.cond="pool", var.col=3,
                                                         date.as.factor=T,
                                                         stat.model="no_interaction_with_covariate",
                                                         return.outcome="model")
    
    ### Delta Coarseroot C pool
    s.delta.crc <- make_delta_crootc_treatment_abs_effect_statistics(inDF=coarse_root_c_pool_1, 
                                                         var.cond="pool", var.col=3,
                                                         date.as.factor=T,
                                                         stat.model="no_interaction_with_covariate",
                                                         return.outcome="model")
    
    ### Delta Understorey aboveground C pool
    s.delta.uac <- make_delta_uac_treatment_abs_effect_statistics(inDF=understorey_aboveground_c_pool, 
                                                      var.cond="pool", var.col=5,
                                                      date.as.factor=T,
                                                      stat.model="no_interaction_with_covariate",
                                                      return.outcome="model")
    
    s.delta.uac2 <- make_delta_uac_treatment_abs_effect_statistics(inDF=understorey_aboveground_c_pool_2, 
                                                       var.cond="pool", var.col=3,
                                                       date.as.factor=T,
                                                       stat.model="no_interaction_with_covariate",
                                                       return.outcome="model")
    
    ### Delta Microbial C pool
    s.delta.micc <- make_delta_micc_treatment_abs_effect_statistics(inDF=microbial_c_pool, 
                                                        var.cond="pool", var.col=3,
                                                        date.as.factor=T,
                                                        stat.model="no_interaction_with_covariate",
                                                        return.outcome="model")
    
    ### Delta Mycorrhizal C pool
    s.delta.mycc <- make_delta_mycc_treatment_abs_effect_statistics(inDF=mycorrhizal_c_pool, 
                                                        var.cond="pool", var.col=3,
                                                        date.as.factor=T,
                                                        stat.model="no_interaction_with_covariate",
                                                        return.outcome="model")
    
    
    ### Delta Leaf litter C pool
    s.delta.litc <- make_delta_litc_treatment_abs_effect_statistics(inDF=leaflitter_pool, 
                                                        var.cond="pool", var.col=6,
                                                        date.as.factor=T,
                                                        stat.model="no_interaction_with_covariate"
                                                        ,return.outcome="model")
    
    ### Delta Insect pool
    s.delta.insc <- make_delta_insc_treatment_abs_effect_statistics(inDF=insect_pool, 
                                                        var.cond="pool", var.col=3,
                                                        date.as.factor=T,
                                                        stat.model="no_interaction_with_covariate",
                                                        return.outcome="model")
    
    #### Create a output table to store all stats
    var.list <- c("soil_c","leaf_c","wood_c","fineroot_c",
                  "coarseroot_c","understorey_c","understorey_c_2",
                  "microbial_c","mycorrhizal_c","litter_c","insect_c",
                  "frass_prod","herb_consump","leaf_prod","twig_prod",
                  "bark_prod","seed_prod","wood_prod","fineroot_prod",
                  "coarseroot_prod","understorey_prod","understorey_lit",
                  "delta_soil_c","delta_leaf_c","delta_wood_c","delta_fineroot_c",
                  "delta_coarseroot_c","delta_understorey_c","delta_understorey_c_2",
                  "delta_microbial_c","delta_mycorrhizal_c","delta_litter_c","delta_insect_c")
    out <- data.frame(var.list, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                      NA,NA,NA,NA,NA,NA)
    colnames(out) <- c("Variable", "interactive_state",
                       "Trt_F", "Date_F", "Trt_Date_F", 
                       "Trt_Df", "Date_Df","Trt_Date_Df",
                       "Trt_Df.res", "Date_Df.res","Trt_Date_Df.res",
                       "Trt_Pr", "Date_Pr", "Trt_Date_Pr",
                       "effect_size", "conf_low", "conf_high")
    
    #### Create a function to ease life
    assign_stats <- function(s.var) {
        temp <- c()
        
        if(s.var$int.state == "non-interactive") {
            #### Assign values to out
            temp <- c(s.var$int.state,
                      s.var$anova$F[1],s.var$anova$F[2],NA,
                      s.var$anova$Df[1],s.var$anova$Df[2],NA,
                      s.var$anova$Df.res[1],s.var$anova$Df.res[2],NA,
                      s.var$anova$`Pr(>F)`[1],s.var$anova$`Pr(>F)`[2],NA,
                      s.var$eff,s.var$conf[1],s.var$conf[2])
            
        } else {#if (s.var$int.state == "interactive") {
            #### Assign values to out
            temp <- c(s.var$int.state,
                      s.var$anova$F[1],s.var$anova$F[2],s.var$anova$F[3],
                      s.var$anova$Df[1],s.var$anova$Df[2],s.var$anova$Df[3],
                      s.var$anova$Df.res[1],s.var$anova$Df.res[2],s.var$anova$Df.res[3],
                      s.var$anova$`Pr(>F)`[1],s.var$anova$`Pr(>F)`[2],s.var$anova$`Pr(>F)`[3],
                      s.var$eff,s.var$conf[1],s.var$conf[2])
        }
        return(temp)
    }
    
    #### Assign value to out
    out[out$Variable=="soil_c",2:17] <- assign_stats(s.var=s.soilc)
    out[out$Variable=="leaf_c",2:17] <- assign_stats(s.var=s.leafc)
    out[out$Variable=="wood_c",2:17] <- assign_stats(s.var=s.woodc)
    out[out$Variable=="fineroot_c",2:17] <- assign_stats(s.var=s.frc)
    out[out$Variable=="coarseroot_c",2:17] <- assign_stats(s.var=s.crc)
    out[out$Variable=="understorey_c",2:17] <- assign_stats(s.var=s.uac)
    out[out$Variable=="understorey_c_2",2:17] <- assign_stats(s.var=s.uac2)
    out[out$Variable=="microbial_c",2:17] <- assign_stats(s.var=s.micc)
    out[out$Variable=="mycorrhizal_c",2:17] <- assign_stats(s.var=s.mycc)
    out[out$Variable=="litter_c",2:17] <- assign_stats(s.var=s.litc)
    out[out$Variable=="insect_c",2:17] <- assign_stats(s.var=s.insc)
    out[out$Variable=="frass_prod",2:17] <- assign_stats(s.var=s.fras)
    out[out$Variable=="herb_consump",2:17] <- assign_stats(s.var=s.hb.cons)
    out[out$Variable=="leaf_prod",2:17] <- assign_stats(s.var=s.lit.leaf)
    out[out$Variable=="twig_prod",2:17] <- assign_stats(s.var=s.lit.twig)
    out[out$Variable=="bark_prod",2:17] <- assign_stats(s.var=s.lit.bark)
    out[out$Variable=="seed_prod",2:17] <- assign_stats(s.var=s.lit.seed)
    out[out$Variable=="wood_prod",2:17] <- assign_stats(s.var=s.wood.prod)
    out[out$Variable=="fineroot_prod",2:17] <- assign_stats(s.var=s.froot.prod)
    out[out$Variable=="coarseroot_prod",2:17] <- assign_stats(s.var=s.croot.prod)
    out[out$Variable=="understorey_prod",2:17] <- assign_stats(s.var=s.und.prod)
    out[out$Variable=="understorey_lit",2:17] <- assign_stats(s.var=s.und.lit)
    out[out$Variable=="delta_soil_c",2:17] <- assign_stats(s.var=s.delta.soilc)
    out[out$Variable=="delta_leaf_c",2:17] <- assign_stats(s.var=s.delta.leafc)
    out[out$Variable=="delta_wood_c",2:17] <- assign_stats(s.var=s.delta.woodc)
    out[out$Variable=="delta_fineroot_c",2:17] <- assign_stats(s.var=s.delta.frc)
    out[out$Variable=="delta_coarseroot_c",2:17] <- assign_stats(s.var=s.delta.crc)
    out[out$Variable=="delta_understorey_c",2:17] <- assign_stats(s.var=s.delta.uac)
    out[out$Variable=="delta_understorey_c_2",2:17] <- assign_stats(s.var=s.delta.uac2)
    out[out$Variable=="delta_microbial_c",2:17] <- assign_stats(s.var=s.delta.micc)
    out[out$Variable=="delta_mycorrhizal_c",2:17] <- assign_stats(s.var=s.delta.mycc)
    out[out$Variable=="delta_litter_c",2:17] <- assign_stats(s.var=s.delta.litc)
    out[out$Variable=="delta_insect_c",2:17] <- assign_stats(s.var=s.delta.insc)

    stat.model <- "no_interaction_with_covariate"
    
    if (stat.model == "no_interaction_with_covariate") {
        write.csv(out, "plots_tables/treatment_statistics_abs_no_interaction_with_covariate.csv", row.names=F)
    } else if (stat.model == "interaction_with_covariate") {
        write.csv(out, "plots_tables/treatment_statistics_abs_interaction_with_covariate.csv", row.names=F)
    } else if (stat.model == "no_interaction_with_covariate_and_covariate") {
        write.csv(out, "plots_tables/treatment_statistics_abs_no_interaction_with_covariate_and_covariate.csv", row.names=F)
    } else {
        write.csv(out, "plots_tables/treatment_statistics_abs_paired_t_test.csv", row.names=F)
    }
    
}