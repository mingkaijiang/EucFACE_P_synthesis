make_cp_ratios_sensitivity_test <- function(norm, c_pool, p_pool, 
                                            c_flux, p_flux) {
    ### Compute CP ratio for major pools
    
    out <- data.frame(c(1:6), NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(out) <- c("Ring", 
                       "canopy", 
                       "leaflitter", 
                       "wood",
                       "sapwood", 
                       "heartwood",
                       "fineroot", 
                       "understorey", 
                       "understorey_litter", 
                       "frass", 
                       "soil", 
                       "microbe")
    
    
    ### Compute CP ratio for major pools
    out$canopy <- as.numeric(c_pool[c_pool$terms == "Canopy C Pool", 2:7]*1.1/p_pool[p_pool$terms == "Canopy P Pool",
                                                                                 2:7])
    
    out$leaflitter <- as.numeric(c_pool[c_pool$terms == "Leaflitter C Pool", 2:7]*1.1/p_pool[p_pool$terms == "Forestfloor Leaf Litter P Pool",
                                                                                         2:7])
    
    out$understorey <- as.numeric(c_pool[c_pool$terms == "Understorey C Pool", 2:7]*1.1/p_pool[p_pool$terms == "Understorey P Pool",
                                                                                           2:7])
    
    out$understorey_litter <- as.numeric(c_flux[c_flux$terms == "Understorey Litter C flux", 2:7]*1.1/p_flux[p_flux$terms == "Understorey Litter P flux",
                                                                                           2:7])
    
    out$wood <- as.numeric(c_pool[c_pool$terms == "Wood C Pool", 2:7]*1.1/p_pool[p_pool$terms == "Total Wood P Pool",
                                                                             2:7])
    
    out$sapwood <- as.numeric(c_pool[c_pool$terms == "Sapwood C Pool", 2:7]*1.1/p_pool[p_pool$terms == "Sapwood P Pool",
                                                                             2:7])
    
    out$heartwood <- as.numeric(c_pool[c_pool$terms == "Heartwood C Pool", 2:7]*1.1/p_pool[p_pool$terms == "Heartwood P Pool",
                                                                             2:7])
    
    out$fineroot <- as.numeric(c_pool[c_pool$terms == "Fine Root C Pool", 2:7]*1.1/p_pool[p_pool$terms == "Fine Root P Pool",
                                                                                      2:7])
    
    out$frass <- as.numeric(c_flux[c_flux$terms == "Frass C flux", 2:7]*1.1/p_flux[p_flux$terms == "Frass P flux",
                                                                               2:7])
    
    
    
    out$soil <- as.numeric(c_pool[c_pool$terms == "Soil C Pool", 2:7]/(p_pool[p_pool$terms == "Soil P Pool 0-10cm",2:7]+
                                                                           p_pool[p_pool$terms == "Soil P Pool 10-30cm",2:7]+
                                                                           p_pool[p_pool$terms == "Soil P Pool 30-60cm",2:7]))
    
    out$microbe <- as.numeric(c_pool[c_pool$terms == "Microbial C Pool", 2:7]/(p_pool[p_pool$terms == "Microbial P Pool 0-10cm",2:7]+
                                                                                   p_pool[p_pool$terms == "Microbial P Pool 10-30cm",2:7]+
                                                                                   p_pool[p_pool$terms == "Microbial P Pool 30-60cm",2:7]))
    
    write.csv(out, paste0("plots_tables/summary_tables/", norm, "/summary_cp_ratios_sensitivity.csv"),
              row.names=F)
    
    
    return(out)
    
}
