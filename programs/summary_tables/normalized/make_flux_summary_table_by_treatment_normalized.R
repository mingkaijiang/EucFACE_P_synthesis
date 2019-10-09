
#### To make EucFACE P summary table by CO2 treatment
#### Ignore time but produce time coverage information
#### This is for fluxes

make_flux_summary_table_by_treatment_normalized <- function() {
    
    ### Define production variable names
    terms <- c("Wood P flux", "Canopy P flux", "Fine Root P flux",
               "Coarse Root P flux","Leaflitter P flux", "Fineroot Litter P flux",
               "Twig litter P flux", "Bark litter P flux","Seed litter P flux", "Frass P flux",
               "Understorey P flux", "Understorey Litter P flux", "Mineralization P flux")
    
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
    
    ### Canopy P flux
    out <- summaryBy(predicted~Ring,data=canopy_p_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Canopy P flux", 2:7] <- out$predicted
    
    ### Wood P 
    out <- summaryBy(predicted~Ring,data=wood_p_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Wood P flux", 2:7] <- out$predicted
    
    ### Fine root P flux
    out <- summaryBy(predicted~Ring,data=fineroot_p_production_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Fine Root P flux", 2:7] <- out$predicted
    
    ### Coarse root P flux
    out <- summaryBy(predicted~Ring,data=coarse_root_p_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Coarse Root P flux", 2:7] <- out$predicted
    
    ### Understorey P flux
    out <- summaryBy(predicted~Ring,data=understorey_p_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Understorey P flux", 2:7] <- out$predicted
    
    ### Understorey Litter P flux
    out <- summaryBy(predicted~Ring,data=understorey_litter_p_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Understorey Litter P flux", 2:7] <- out$predicted

    ### Mineralization flux
    out <- summaryBy(predicted~Ring,data=soil_p_mineralization_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Mineralization P flux", 2:7] <- out$predicted
    
    ### Frass production flux
    out <- summaryBy(predicted~Ring,data=frass_p_production_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Frass P flux", 2:7] <- out$predicted
    
    ### Leaf litter flux
    out <- summaryBy(predicted~Ring,data=leaflitter_p_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Leaflitter P flux", 2:7] <- out$predicted
    
    ### Fine Root litter flux

    
    ### twig litter flux
    out <- summaryBy(predicted~Ring,data=twig_litter_p_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Twig litter P flux", 2:7] <- out$predicted
    
    ### bark litter flux
    out <- summaryBy(predicted~Ring,data=bark_litter_p_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Bark litter P flux", 2:7] <- out$predicted
    
    ### seed litter flux
    out <- summaryBy(predicted~Ring,data=seed_litter_p_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Seed litter P flux", 2:7] <- out$predicted
    
    ### calculate treatment averages
    treatDF$aCO2 <- round(rowMeans(subset(treatDF, select=c(R2, R3, R6)), na.rm=T), 5)
    treatDF$eCO2 <- round(rowMeans(subset(treatDF, select=c(R1, R4, R5)), na.rm=T), 5)
    
    treatDF$aCO2_sd <- rowSds(as.matrix(subset(treatDF, select=c(R2, R3, R6))), na.rm=T)
    treatDF$eCO2_sd <- rowSds(as.matrix(subset(treatDF, select=c(R1, R4, R5))), na.rm=T)
    
    
    
    ##### output tables
    return(treatDF)
      
}

