
#### To make EucFACE C summary table by CO2 treatment
#### Ignore time but produce time coverage information
#### This is for fluxes

make_c_flux_summary_table_by_treatment_bootstrap <- function() {
    
    ### Define production variable names
    terms <- c("Wood C flux", "Canopy C flux", "Fine Root C flux",
               "Coarse Root C flux","Leaflitter C flux", "Twiglitter C flux",
               "Barklitter C flux","Seedlitter C flux",
               "Fineroot Litter C flux",
               "Frass C flux","Understorey C flux")
    
    treatDF <- data.frame(terms)
    treatDF$R1 <- rep(NA, length(treatDF$terms))
    treatDF$R2 <- rep(NA, length(treatDF$terms))
    treatDF$R3 <- rep(NA, length(treatDF$terms))
    treatDF$R4 <- rep(NA, length(treatDF$terms))
    treatDF$R5 <- rep(NA, length(treatDF$terms))
    treatDF$R6 <- rep(NA, length(treatDF$terms))
    
    treatDF$aCO2 <- rep(NA, length(treatDF$terms))
    treatDF$eCO2 <- rep(NA, length(treatDF$terms))
    treatDF$aCO2_sd <- rep(NA, length(treatDF$terms))
    treatDF$eCO2_sd <- rep(NA, length(treatDF$terms))
    
    ### Canopy C flux
    out <- summaryBy(predicted~Ring,data=leaflitter_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Canopy C flux", 2:7] <- out$predicted
    
    
    
    ### Wood C 
    out <- summaryBy(predicted~Ring,data=wood_production_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Wood C flux", 2:7] <- out$predicted
    
    ### Fine root C flux
    out <- summaryBy(predicted~Ring,data=fineroot_production_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Fine Root C flux", 2:7] <- out$predicted
    
    ### Coarse root C flux
    out <- summaryBy(predicted~Ring,data=coarse_root_production_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Coarse Root C flux", 2:7] <- out$predicted
    
    ### Understorey C flux
    out <- summaryBy(predicted~Ring,data=understorey_aboveground_production_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Understorey C flux", 2:7] <- out$predicted
    
    ### Frass production flux
    out <- summaryBy(predicted~Ring,data=frass_c_production_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Frass C flux", 2:7] <- out$predicted
    
    ### Leaf litter flux
    out <- summaryBy(predicted~Ring,data=leaflitter_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Leaflitter C flux", 2:7] <- out$predicted
    
    ### Twig litter flux
    out <- summaryBy(predicted~Ring,data=twiglitter_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Twiglitter C flux", 2:7] <- out$predicted
    
    ### Bark litter flux
    out <- summaryBy(predicted~Ring,data=barklitter_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Barklitter C flux", 2:7] <- out$predicted
    
    ### Seed litter flux
    out <- summaryBy(predicted~Ring,data=seedlitter_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Seedlitter C flux", 2:7] <- out$predicted
    
    ### Fine Root litter flux
    # assume it's the same as fine root production flux
    out <- summaryBy(predicted~Ring,data=fineroot_production_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Fineroot Liter C flux", 2:7] <- out$predicted
    
    ### calculate treatment averages
    treatDF$aCO2 <- round(rowMeans(subset(treatDF, select=c(R2, R3, R6)), na.rm=T), 5)
    treatDF$eCO2 <- round(rowMeans(subset(treatDF, select=c(R1, R4, R5)), na.rm=T), 5)

    
    ##### output tables
    return(treatDF)
      
}

