make_plant_P_use_efficiency <- function(c_flux, p_flux) {
    
    
    ### NPP df
    nppDF <- c_flux[c_flux$terms%in%c("Canopy C flux", 
                                      "Wood C flux", 
                                      "Coarse Root C flux",
                                      "Fine Root C flux",
                                      "Understorey C flux", 
                                      "Frass C flux",
                                      "Twig C flux", 
                                      "Bark C flux", 
                                      "Seed C flux"),]
    
    tot <- as.numeric(colSums(nppDF[,2:7]))
    
    tmp1 <- as.numeric(p_flux[p_flux$terms=="Total vegetation uptake P flux",2:7])
    
    
    out <- data.frame("Ring" = c(1:6), "Trt"=c("ele","amb","amb","ele","ele","amb"),
                      "NPP" = tot, "Total_plant_uptake_P_flux" = tmp1)

    ### PUE
    out$PUE <- out$NPP/out$Total_plant_uptake_P_flux   
    
    return(out)
}