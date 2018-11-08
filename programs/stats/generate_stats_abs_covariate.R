generate_stats_abs_covariate <- function() {
    
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
    s.crootp.pool <- make_crootp_pool_treatment_abs_effect_statistics(inDF=coarse_root_p_pool, 
                                                                  var.col=3,
                                                                  stat.model="no_interaction_with_covariate",
                                                                  return.outcome="model")
    
    ### Leaf litter P flux
    s.leaf.litp.flux <- make_leaf_lit_p_flux_treatment_abs_effect_statistics(inDF=leaflitter_p_flux, 
                                                                   var.col=5,
                                                                   stat.model="no_interaction_with_covariate",
                                                                   return.outcome="model")
    
    ### twig litter flux
    s.twig.litp.flux <- make_leaf_lit_p_flux_treatment_abs_effect_statistics(inDF=twig_litter_p_flux, 
                                                                             var.col=5,
                                                                             stat.model="no_interaction_with_covariate",
                                                                             return.outcome="model")
    
    ### bark litter flux
    s.bark.litp.flux <- make_leaf_lit_p_flux_treatment_abs_effect_statistics(inDF=bark_litter_p_flux, 
                                                                             var.col=5,
                                                                             stat.model="no_interaction_with_covariate",
                                                                             return.outcome="model")
    
    ### seed litter flux
    s.seed.litp.flux <- make_leaf_lit_p_flux_treatment_abs_effect_statistics(inDF=seed_litter_p_flux, 
                                                                             var.col=5,
                                                                             stat.model="no_interaction_with_covariate",
                                                                             return.outcome="model")
    

    ### Frass P flux
    s.frasp.flux <- make_frassp_conc_treatment_abs_effect_statistics(inDF=frass_p_production, 
                                                                     var.col=5,
                                                                     stat.model="no_interaction_with_covariate",
                                                                     return.outcome="model")
    
    ### Canopy P production flux
    s.canopy.p.flux <- make_canopy_p_flux_treatment_abs_effect_statistics(inDF=canopy_p_flux, 
                                                                     var.col=5,
                                                                     stat.model="no_interaction_with_covariate",
                                                                     return.outcome="model")
    
    ### Wood production flux
    s.wood.p.flux <- make_wood_p_flux_treatment_abs_effect_statistics(inDF=wood_p_flux, 
                                               var.col=5,
                                               stat.model="no_interaction_with_covariate",
                                               return.outcome="model") 
  
    ### Fineroot production flux
    s.froot.p.flux <- make_froot_p_flux_treatment_abs_effect_statistics(inDF=fineroot_p_production, 
                                                                    var.col=5,
                                                                    stat.model="no_interaction_with_covariate",
                                                                    return.outcome="model") 

    ### Coarseroot production
    s.croot.p.flux <- make_croot_p_flux_treatment_abs_effect_statistics(inDF=coarse_root_p_flux, 
                                                                    var.col=5,
                                                                    stat.model="no_interaction_with_covariate",
                                                                    return.outcome="model")
   
    ### Understorey aboveground production
    s.und.p.flux <- make_und_p_flux_treatment_abs_effect_statistics(inDF=understorey_p_flux, 
                                                                var.col=5,
                                                                stat.model="no_interaction_with_covariate",
                                                                return.outcome="model")
    
    ### Understory litter flux
    s.und.lit.p.flux <- make_und_lit_p_flux_treatment_abs_effect_statistics(inDF=understorey_litter_p_flux, 
                                                                 var.col=5,
                                                                stat.model="no_interaction_with_covariate",
                                                                return.outcome="model")

    ### p mineralization flux
    s.mineralization.p.flux <- make_mineralization_p_flux_treatment_abs_effect_statistics(inDF=soil_p_mineralization, 
                                                                                   var.col=3,
                                                                                   stat.model="no_interaction_with_covariate",
                                                                                   return.outcome="model")
    
    ### Delta Soil p
    s.delta.soilp <- make_delta_soilp_treatment_abs_effect_statistics(inDF=soil_p_pool, 
                                                         var.col=3,
                                                          stat.model="no_interaction_with_covariate",
                                                          return.outcome="model")
    
    ### Delta Leaf p
    s.delta.leafp <- make_delta_leafp_treatment_abs_effect_statistics(inDF=canopy_p_pool, 
                                                           var.col=3,
                                                          stat.model="no_interaction_with_covariate",
                                                          return.outcome="model")
    
    ### Delta Wood C pool
    s.delta.woodp <- make_delta_woodp_treatment_abs_effect_statistics(inDF=wood_p_pool, 
                                                          var.col=3,
                                                          stat.model="no_interaction_with_covariate",
                                                          return.outcome="model") 
    
    ### Delta Fineroot C pool
    s.delta.frp <- make_delta_frootp_treatment_abs_effect_statistics(inDF=fineroot_p_pool, 
                                                          var.col=3,
                                                          stat.model="no_interaction_with_covariate",
                                                          return.outcome="model")
    
    ### Delta Coarseroot C pool
    s.delta.crp <- make_delta_crootp_treatment_abs_effect_statistics(inDF=coarse_root_p_pool, 
                                                         var.col=3,
                                                         stat.model="no_interaction_with_covariate",
                                                         return.outcome="model")
    
    ### Delta Understorey aboveground C pool
    s.delta.uap <- make_delta_uap_treatment_abs_effect_statistics(inDF=understorey_p_pool, 
                                                       var.col=3,
                                                      stat.model="no_interaction_with_covariate",
                                                      return.outcome="model")
    
    ### Delta Microbial C pool
    s.delta.micp <- make_delta_micp_treatment_abs_effect_statistics(inDF=microbial_p_pool, 
                                                        var.col=3,
                                                        stat.model="no_interaction_with_covariate",
                                                        return.outcome="model")
    
    ### Delta Mycorrhizal C pool
    #s.delta.mycp <- make_delta_mycp_treatment_abs_effect_statistics(inDF=mycorrhizal_c_pool, 
    #                                                     var.col=3,
    #                                                    stat.model="no_interaction_with_covariate",
    #                                                    return.outcome="model")
    
    
    ### Delta Leaf litter C pool
    #s.delta.litp <- make_delta_litc_treatment_abs_effect_statistics(inDF=leaflitter_pool, 
    #                                                    var.col=6,
    #                                                    stat.model="no_interaction_with_covariate",
    #                                                    return.outcome="model")
    
 
    
    #### Create a output table to store all stats
    var.list <- c("soil_p_conc","soil_phosphate_p_conc","leaf_p_conc",
                  "wood_p_conc","fineroot_p_conc",
                  "understorey_p_conc","microbial_p_conc","litter_p_conc",
                  "frass_p_conc",
                  "soil_p_pool","leaf_p_pool","wood_p_pool","fineroot_p_pool",
                  "coarseroot_p_pool","understorey_p_pool",
                  "microbial_p_pool",
                  "frass_p_prod","leaf_p_prod","leaf_lit_p_prod","twig_lit_p_prod",
                  "bark_lit_p_prod","seed_lit_p_prod",
                  "wood_p_prod","fineroot_p_prod",
                  "coarseroot_p_prod","understorey_p_prod","understorey_lit_p_prod",
                  "soil_p_mineralization",
                  "delta_soil_p","delta_leaf_p","delta_wood_p","delta_fineroot_p",
                  "delta_coarseroot_p","delta_understorey_p",
                  "delta_microbial_p")
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
    
    out[out$Variable=="soil_p_conc",2:17] <- assign_stats(s.var=s.soilp.conc)
    out[out$Variable=="soil_phosphate_p_conc",2:17] <- assign_stats(s.var=s.soil.phosphate.p.conc)
    out[out$Variable=="leaf_p_conc",2:17] <- assign_stats(s.var=s.leafp.conc)
    #out[out$Variable=="wood_p_conc",2:17] <- assign_stats(s.var=s.woodp.conc)
    out[out$Variable=="fineroot_p_conc",2:17] <- assign_stats(s.var=s.frp.conc)
    out[out$Variable=="understorey_p_conc",2:17] <- assign_stats(s.var=s.uap.conc)
    out[out$Variable=="microbial_p_conc",2:17] <- assign_stats(s.var=s.micp.conc)
    out[out$Variable=="litter_p_conc",2:17] <- assign_stats(s.var=s.litp.conc)
    out[out$Variable=="frass_p_conc",2:17] <- assign_stats(s.var=s.frasp.conc)
    out[out$Variable=="soil_p_pool",2:17] <- assign_stats(s.var=s.soilp.pool)
    out[out$Variable=="leaf_p_pool",2:17] <- assign_stats(s.var=s.leafp.pool)
    out[out$Variable=="wood_p_pool",2:17] <- assign_stats(s.var=s.woodp.pool)
    out[out$Variable=="fineroot_p_pool",2:17] <- assign_stats(s.var=s.frp.pool)
    out[out$Variable=="coarseroot_p_pool",2:17] <- assign_stats(s.var=s.crootp.pool)
    out[out$Variable=="understorey_p_pool",2:17] <- assign_stats(s.var=s.uap.pool)
    out[out$Variable=="microbial_p_pool",2:17] <- assign_stats(s.var=s.micp.pool)
    out[out$Variable=="frass_p_prod",2:17] <- assign_stats(s.var=s.frasp.flux)
    out[out$Variable=="leaf_p_prod",2:17] <- assign_stats(s.var=s.canopy.p.flux)
    out[out$Variable=="twig_lit_p_prod",2:17] <- assign_stats(s.var=s.twig.litp.flux)
    out[out$Variable=="bark_lit_p_prod",2:17] <- assign_stats(s.var=s.bark.litp.flux)
    out[out$Variable=="seed_lit_p_prod",2:17] <- assign_stats(s.var=s.seed.litp.flux)
    out[out$Variable=="leaf_lit_p_prod",2:17] <- assign_stats(s.var=s.leaf.litp.flux)
    out[out$Variable=="wood_p_prod",2:17] <- assign_stats(s.var=s.wood.p.flux)
    out[out$Variable=="fineroot_p_prod",2:17] <- assign_stats(s.var=s.froot.p.flux)
    out[out$Variable=="coarseroot_p_prod",2:17] <- assign_stats(s.var=s.croot.p.flux)
    out[out$Variable=="understorey_p_prod",2:17] <- assign_stats(s.var=s.und.p.flux)
    out[out$Variable=="understorey_lit_p_prod",2:17] <- assign_stats(s.var=s.und.lit.p.flux)
    out[out$Variable=="soil_p_mineralization",2:17] <- assign_stats(s.var=s.mineralization.p.flux)
    
    out[out$Variable=="delta_soil_p",2:17] <- assign_stats(s.var=s.delta.soilp)
    out[out$Variable=="delta_leaf_p",2:17] <- assign_stats(s.var=s.delta.leafp)
    out[out$Variable=="delta_wood_p",2:17] <- assign_stats(s.var=s.delta.woodp)
    out[out$Variable=="delta_fineroot_p",2:17] <- assign_stats(s.var=s.delta.frp)
    out[out$Variable=="delta_coarseroot_p",2:17] <- assign_stats(s.var=s.delta.crp)
    out[out$Variable=="delta_understorey_p",2:17] <- assign_stats(s.var=s.delta.uap)
    out[out$Variable=="delta_microbial_p",2:17] <- assign_stats(s.var=s.delta.micp)

    stat.model <- "no_interaction_with_covariate"
    
    write.csv(out, "plots_tables/treatment_statistics_abs_no_interaction_with_covariate.csv", row.names=F)

    
}