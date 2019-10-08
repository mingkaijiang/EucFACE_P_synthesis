generate_stats_abs_covariate <- function() {
  
  ### remove pre-treatment data period for individual variables (e.g. leaf, wood, soil, mic, myc)
  
  ######## Concentration
  ### Soil P conc
  s.soilp.conc <- make_soilp_conc_treatment_abs_effect_statistics(inDF=soil_p_concentration, 
                                                                  var.col=3,
                                                                  return.outcome="model")
  
  ### Soil Phosphate conc
  s.soil.phosphate.p.conc <- make_soilp_conc_treatment_abs_effect_statistics(inDF=soil_phosphate_concentration, 
                                                                             var.col=3,
                                                                             return.outcome="model")
  
  ### hedley P concentrations
  s.soil.exhanagable.pi.conc <- make_hedley_soilp_conc_treatment_abs_effect_statistics(inDF=soil_hedley_p_concentration, 
                                                                                       var.col=3,
                                                                                       return.outcome="model")
  
  s.soil.exhanagable.po.conc <- make_hedley_soilp_conc_treatment_abs_effect_statistics(inDF=soil_hedley_p_concentration, 
                                                                                       var.col=4,
                                                                                       return.outcome="model")
  
  s.soil.mlabile.po.conc <- make_hedley_soilp_conc_treatment_abs_effect_statistics(inDF=soil_hedley_p_concentration, 
                                                                                   var.col=5,
                                                                                   return.outcome="model")
  
  s.soil.secondary.pi.conc <- make_hedley_soilp_conc_treatment_abs_effect_statistics(inDF=soil_hedley_p_concentration, 
                                                                                     var.col=6,
                                                                                     return.outcome="model")
  
  s.soil.primary.pi.conc <- make_hedley_soilp_conc_treatment_abs_effect_statistics(inDF=soil_hedley_p_concentration, 
                                                                                   var.col=7,
                                                                                   return.outcome="model")
  
  s.soil.occluded.p.conc <- make_hedley_soilp_conc_treatment_abs_effect_statistics(inDF=soil_hedley_p_concentration, 
                                                                                   var.col=8,
                                                                                   return.outcome="model")
  
  s.soil.aqua.p.conc <- make_hedley_soilp_conc_treatment_abs_effect_statistics(inDF=soil_hedley_p_concentration, 
                                                                               var.col=9,
                                                                               return.outcome="model")
  
  ### Overstorey Leaf P conc
  s.leafp.conc <- make_leafp_conc_treatment_abs_effect_statistics(inDF=canopy_p_concentration, 
                                                                  var.col=3,
                                                                  return.outcome="model")
  
  ### Overstorey Leaf litter P conc
  s.leafp.lit.conc <- make_leafp_conc_treatment_abs_effect_statistics(inDF=leaflitter_p_concentration, 
                                                                      var.col=3,
                                                                      return.outcome="model")
  
  ### Wood P conc
  ### we do not have enough data to perform lmer for wood P concentration
  
  ### Fineroot P conc
  s.frp.conc <- make_frootp_conc_treatment_abs_effect_statistics(inDF=fineroot_p_concentration, 
                                                                 var.col=3,
                                                                 return.outcome="model")
  
  ### Understorey aboveground P conc
  s.uap.conc <- make_uap_conc_treatment_abs_effect_statistics(inDF=understorey_p_concentration, 
                                                              var.col=3,
                                                              return.outcome="model")
  
  ### Understorey aboveground litter P conc
  ### not possible to construct a model due to limited data
  
  ### Microbial P conc
  s.micp.conc <- make_micp_conc_treatment_abs_effect_statistics(inDF=microbial_p_concentration, 
                                                                var.col=3,
                                                                return.outcome="model")
  
  
  ### Leaf litter P conc
  s.litp.conc <- make_leafp_conc_treatment_abs_effect_statistics(inDF=canopy_p_concentration, 
                                                                 var.col=3,
                                                                 return.outcome="model")
  
  ### Frass P concentration
  s.frasp.conc <- make_frassp_conc_treatment_abs_effect_statistics(inDF=frass_p_concentration, 
                                                                   var.col=3,
                                                                   return.outcome="model")
  
  
  ######## P fluxes and stocks
  ### Soil P pool
  s.soilp.pool <- make_soilp_conc_treatment_abs_effect_statistics(inDF=soil_p_pool, 
                                                                  var.col=3,
                                                                  return.outcome="model")
  
  ### Soil Phosphate pool
  s.soil.phosphate.p.pool <- make_soilp_conc_treatment_abs_effect_statistics(inDF=soil_phosphate_pool, 
                                                                             var.col=3,
                                                                             return.outcome="model")
  
  ### hedley P pools
  s.soil.exhanagable.pi.pool <- make_hedley_soilp_conc_treatment_abs_effect_statistics(inDF=soil_p_pool_hedley, 
                                                                                       var.col=3,
                                                                                       return.outcome="model")
  
  s.soil.exhanagable.po.pool <- make_hedley_soilp_conc_treatment_abs_effect_statistics(inDF=soil_p_pool_hedley, 
                                                                                       var.col=4,
                                                                                       return.outcome="model")
  
  s.soil.mlabile.po.pool <- make_hedley_soilp_conc_treatment_abs_effect_statistics(inDF=soil_p_pool_hedley, 
                                                                                   var.col=5,
                                                                                   return.outcome="model")
  
  s.soil.secondary.pi.pool <- make_hedley_soilp_conc_treatment_abs_effect_statistics(inDF=soil_p_pool_hedley, 
                                                                                     var.col=6,
                                                                                     return.outcome="model")
  
  s.soil.primary.pi.pool <- make_hedley_soilp_conc_treatment_abs_effect_statistics(inDF=soil_p_pool_hedley, 
                                                                                   var.col=7,
                                                                                   return.outcome="model")
  
  s.soil.occluded.p.pool <- make_hedley_soilp_conc_treatment_abs_effect_statistics(inDF=soil_p_pool_hedley, 
                                                                                   var.col=8,
                                                                                   return.outcome="model")
  
  s.soil.aqua.p.pool <- make_hedley_soilp_conc_treatment_abs_effect_statistics(inDF=soil_p_pool_hedley, 
                                                                               var.col=9,
                                                                               return.outcome="model")
  
  ### Overstorey Leaf P pool
  s.leafp.pool <- make_leafp_conc_treatment_abs_effect_statistics(inDF=canopy_p_pool, 
                                                                  var.col=3,
                                                                  return.outcome="model")
  
  ### Wood P pool
  s.woodp.pool <- make_woodp_pool_treatment_abs_effect_statistics(inDF=wood_p_pool, 
                                                                  var.col=3,
                                                                  return.outcome="model")
  
  ### Fineroot P pool
  s.frp.pool <- make_frootp_conc_treatment_abs_effect_statistics(inDF=fineroot_p_pool, 
                                                                 var.col=3,
                                                                 return.outcome="model")
  
  ### Understorey aboveground P pool
  s.uap.pool <- make_uap_conc_treatment_abs_effect_statistics(inDF=understorey_p_pool, 
                                                              var.col=3,
                                                              return.outcome="model")
  
  ### Microbial P pool
  s.micp.pool <- make_micp_conc_treatment_abs_effect_statistics(inDF=microbial_p_pool, 
                                                                var.col=3,
                                                                return.outcome="model")
  
  
  ### coarse root P pool
  s.crootp.pool <- make_crootp_pool_treatment_abs_effect_statistics(inDF=coarse_root_p_pool, 
                                                                    var.col=3,
                                                                    return.outcome="model")
  
  ### Leaf litter P flux
  s.leaf.litp.flux <- make_leaf_lit_p_flux_treatment_abs_effect_statistics(inDF=leaflitter_p_flux, 
                                                                           var.col=5,
                                                                           return.outcome="model")
  
  ### twig litter flux
  s.twig.litp.flux <- make_leaf_lit_p_flux_treatment_abs_effect_statistics(inDF=twig_litter_p_flux, 
                                                                           var.col=5,
                                                                           return.outcome="model")
  
  ### bark litter flux
  s.bark.litp.flux <- make_leaf_lit_p_flux_treatment_abs_effect_statistics(inDF=bark_litter_p_flux, 
                                                                           var.col=5,
                                                                           return.outcome="model")
  
  ### seed litter flux
  s.seed.litp.flux <- make_leaf_lit_p_flux_treatment_abs_effect_statistics(inDF=seed_litter_p_flux, 
                                                                           var.col=5,
                                                                           return.outcome="model")
  
  
  ### Frass P flux
  s.frasp.flux <- make_frassp_conc_treatment_abs_effect_statistics(inDF=frass_p_production, 
                                                                   var.col=5,
                                                                   return.outcome="model")
  
  ### Canopy P production flux
  s.canopy.p.flux <- make_canopy_p_flux_treatment_abs_effect_statistics(inDF=canopy_p_flux, 
                                                                        var.col=5,
                                                                        return.outcome="model")
  
  ### Wood production flux
  s.wood.p.flux <- make_wood_p_flux_treatment_abs_effect_statistics(inDF=wood_p_flux, 
                                                                    var.col=5,
                                                                    return.outcome="model") 
  
  ### Fineroot production flux
  s.froot.p.flux <- make_froot_p_flux_treatment_abs_effect_statistics(inDF=fineroot_p_production, 
                                                                      var.col=5,
                                                                      return.outcome="model") 
  
  ### Coarseroot production
  s.croot.p.flux <- make_croot_p_flux_treatment_abs_effect_statistics(inDF=coarse_root_p_flux, 
                                                                      var.col=5,
                                                                      return.outcome="model")
  
  ### Understorey aboveground production
  s.und.p.flux <- make_und_p_flux_treatment_abs_effect_statistics(inDF=understorey_p_flux, 
                                                                  var.col=5,
                                                                  return.outcome="model")
  
  ### Understory litter flux
  s.und.lit.p.flux <- make_und_lit_p_flux_treatment_abs_effect_statistics(inDF=understorey_litter_p_flux, 
                                                                          var.col=5,
                                                                          return.outcome="model")
  
  ### p mineralization flux
  s.mineralization.p.flux <- make_mineralization_p_flux_treatment_abs_effect_statistics(inDF=soil_p_mineralization, 
                                                                                        var.col=3,
                                                                                        return.outcome="model")
  
  ### p leaching flux
  s.leaching.p.flux <- make_p_leaching_flux_treatment_abs_effect_statistics(inDF=soil_p_leaching, 
                                                                            var.col=5,
                                                                            return.outcome="model")
  

  
  
  #### Create a output table to store all stats
  var.list <- c("soil_p_conc","soil_phosphate_p_conc",
                "exhanagable_pi_conc", "exhanagable_po_conc",
                "moderately_labile_po_conc", "secondary_pi_conc",
                "primary_pi_conc", "occluded_p_conc",
                "aqua_regia_p_conc",
                "leaf_p_conc",
                "wood_p_conc","fineroot_p_conc",
                "understorey_p_conc","microbial_p_conc","litter_p_conc",
                "frass_p_conc",
                "soil_p_pool",
                "exhanagable_pi_pool", "exhanagable_po_pool",
                "moderately_labile_po_pool", "secondary_pi_pool",
                "primary_pi_pool", "occluded_p_pool",
                "aqua_regia_p_pool",
                "leaf_p_pool","wood_p_pool","fineroot_p_pool",
                "coarseroot_p_pool","understorey_p_pool",
                "microbial_p_pool",
                "frass_p_prod","leaf_p_prod","leaf_lit_p_prod","twig_lit_p_prod",
                "bark_lit_p_prod","seed_lit_p_prod",
                "wood_p_prod","fineroot_p_prod",
                "coarseroot_p_prod","understorey_p_prod","understorey_lit_p_prod",
                "soil_p_mineralization", "soil_p_leaching")
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
    
    #### Assign values to out
    temp <- c(s.var$int.state,
              s.var$anova$F[1],s.var$anova$F[2],NA,
              s.var$anova$Df[1],s.var$anova$Df[2],NA,
              s.var$anova$Df.res[1],s.var$anova$Df.res[2],NA,
              s.var$anova$`Pr(>F)`[1],s.var$anova$`Pr(>F)`[2],NA,
              s.var$eff,s.var$conf[1],s.var$conf[2])
    
    
    return(temp)
  }
  
  out[out$Variable=="soil_p_conc",2:17] <- assign_stats(s.var=s.soilp.conc)
  out[out$Variable=="exhanagable_pi_conc",2:17] <- assign_stats(s.var=s.soil.exhanagable.pi.conc)
  out[out$Variable=="exhanagable_po_conc",2:17] <- assign_stats(s.var=s.soil.exhanagable.po.conc)
  out[out$Variable=="moderately_labile_po_conc",2:17] <- assign_stats(s.var=s.soil.mlabile.po.conc)
  out[out$Variable=="secondary_pi_conc",2:17] <- assign_stats(s.var=s.soil.secondary.pi.conc)
  out[out$Variable=="primary_pi_conc",2:17] <- assign_stats(s.var=s.soil.primary.pi.conc)
  out[out$Variable=="occluded_p_conc",2:17] <- assign_stats(s.var=s.soil.occluded.p.conc)
  out[out$Variable=="aqua_regia_p_conc",2:17] <- assign_stats(s.var=s.soil.aqua.p.conc)
  
  out[out$Variable=="leaf_p_conc",2:17] <- assign_stats(s.var=s.leafp.conc)
  #out[out$Variable=="wood_p_conc",2:17] <- assign_stats(s.var=s.woodp.conc)
  out[out$Variable=="fineroot_p_conc",2:17] <- assign_stats(s.var=s.frp.conc)
  out[out$Variable=="understorey_p_conc",2:17] <- assign_stats(s.var=s.uap.conc)
  out[out$Variable=="microbial_p_conc",2:17] <- assign_stats(s.var=s.micp.conc)
  out[out$Variable=="litter_p_conc",2:17] <- assign_stats(s.var=s.litp.conc)
  out[out$Variable=="frass_p_conc",2:17] <- assign_stats(s.var=s.frasp.conc)
  
  
  out[out$Variable=="soil_p_pool",2:17] <- assign_stats(s.var=s.soilp.pool)
  out[out$Variable=="exhanagable_pi_pool",2:17] <- assign_stats(s.var=s.soil.exhanagable.pi.pool)
  out[out$Variable=="exhanagable_po_pool",2:17] <- assign_stats(s.var=s.soil.exhanagable.po.pool)
  out[out$Variable=="moderately_labile_po_pool",2:17] <- assign_stats(s.var=s.soil.mlabile.po.pool)
  out[out$Variable=="secondary_pi_pool",2:17] <- assign_stats(s.var=s.soil.secondary.pi.pool)
  out[out$Variable=="primary_pi_pool",2:17] <- assign_stats(s.var=s.soil.primary.pi.pool)
  out[out$Variable=="occluded_p_pool",2:17] <- assign_stats(s.var=s.soil.occluded.p.pool)
  out[out$Variable=="aqua_regia_p_pool",2:17] <- assign_stats(s.var=s.soil.aqua.p.pool)
  
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
  out[out$Variable=="soil_p_leaching",2:17] <- assign_stats(s.var=s.leaching.p.flux)
  
  
  write.csv(out, "plots_tables/treatment_statistics_normalized.csv", row.names=F)
  
  
}