make_total_plant_p_fluxes <- function(sumDF, wood_retrans_coef) {
    
    #### Calculate total plant P requirement flux
    myDF1 <- sumDF[sumDF$terms%in%c("Canopy P flux", "Wood P flux", "Fine Root P flux",
                                    "Coarse Root P flux", "Twig litter P flux", "Bark litter P flux", 
                                    "Seed litter P flux", "Frass P flux", "Understorey P flux"),]
    
    tot1 <- colSums(myDF1[,2:7])
    
    out <- data.frame(c(1:6), tot1, c("eCO2", "aCO2", "aCO2", "eCO2", "eCO2", "aCO2"))
    colnames(out) <- c("Ring", "Total_plant_P_requirement_flux", "Trt")
    
    
    ### calculate wood and coarseroot "litter" fluxes: that is, the flux that turned into heartwood
    coef <- mean(wood_retrans_coef$retrans_coef)
    tmpDF <- sumDF[sumDF$terms%in%c("Wood P flux", "Coarse Root P flux"), 2:7] * (1-coef)
    
    
    
    #### Calculate total plant litter P flux
    ### litter fluxes, we do not have wood and coarseroot
    myDF2 <- sumDF[sumDF$terms%in%c("Leaflitter P flux", "Fineroot Litter P flux", "Understorey Litter P flux"),2:7]
    myDF3 <- rbind(tmpDF, myDF2)
    
    tot2 <- colSums(myDF3)
    
    out$Total_plant_litter_P_flux <- tot2
    
    
    #### Calculate total plant retranslocation P flux
    out$Total_plant_retranslocation_P_flux <- with(out, (Total_plant_P_requirement_flux - Total_plant_litter_P_flux))
    
    
    #### Calculate total plant uptake P flux
    out$Total_plant_uptake_P_flux <- with(out, Total_plant_P_requirement_flux - Total_plant_retranslocation_P_flux)
    
    #### Calculate total uptake over requirement ratio
    out$Total_uptake_over_requirement_ratio <- round(with(out, Total_plant_uptake_P_flux / Total_plant_P_requirement_flux),2)
    
    #### Calculate total retranslocation over requirement ratio
    out$Total_retranslocation_over_requirement_ratio <- with(out, 1 - Total_uptake_over_requirement_ratio)
    
    return(out)
    
}