make_plant_P_use_efficiency <- function(norm, c_flux, p_flux) {
    
    ### need to use normalized NPP, not the unnormalized results
    
    
    ### NPP df
    #nppDF <- c_flux[c_flux$terms%in%c("Canopy C flux", 
    #                                  "Wood C flux", 
    #                                  "Coarse Root C flux",
    #                                  "Fine Root C flux",
    #                                  "Understorey C flux", 
    #                                  "Frass C flux",
    #                                  "Twig C flux", 
    #                                  "Bark C flux", 
    #                                  "Seed C flux"),]
    
    
    nppDF <- data.frame("terms"=c("Canopy C flux", 
                                  "Wood C flux", 
                                  "Coarse Root C flux",
                                  "Fine Root C flux",
                                  "Understorey C flux", 
                                  "Frass C flux",
                                  "Other C flux"),
                        "R1"=c(182.39,37.46,4.64,152.1,166.55,12.43,128.13),
                        "R2"=c(192.17,40.72,4.64,131.27,150.76,12.42,105.36),
                        "R3"=c(191.79,58.8,7.41,136.94,110.94,9.04,102.7),
                        "R4"=c(182.20,39.00,5.28,147.16,195.93,8.46,107.6),
                        "R5"=c(182.43,30.81,4.94,151.71,187.33,13.32,119.14),
                        "R6"=c(192.14,30.57,3.4,127.74,137.08,9.91,114.38))
    
    tot <- as.numeric(colSums(nppDF[,2:7]))
    
    tmp1 <- as.numeric(p_flux[p_flux$terms=="Total vegetation uptake P flux",2:7])
    
    
    out <- data.frame("Ring" = c(1:6), "Trt"=c("ele","amb","amb","ele","ele","amb"),
                      "NPP" = tot, "Total_plant_uptake_P_flux" = tmp1)

    ### PUE
    out$PUE <- out$NPP/out$Total_plant_uptake_P_flux   
    
    return(out)
}