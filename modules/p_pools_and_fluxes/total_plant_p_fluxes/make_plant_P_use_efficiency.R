make_plant_P_use_efficiency <- function(c_flux, p_flux) {
    
    
    ### NPP df
    nppDF <- c_flux[c_flux$terms%in%c("Canopy C flux", "Wood C flux", "Coarse Root C flux",
                                      "Fine Root C flux", "Understorey C flux", "Frass C flux",
                                      "Twig C flux", "Bark C flux", "Seed C flux"),]
    
    tot <- colSums(nppDF[,2:7])
    
    out <- data.frame(p_flux$Ring, p_flux$Trt, tot, p_flux$Total_plant_uptake_P_flux)
    colnames(out) <- c("Ring", "Trt", "NPP", "Total_plant_uptake_P_flux")
    
    ### PUE
    out$PUE <- out$NPP/out$Total_plant_uptake_P_flux   
    
    return(out)
}