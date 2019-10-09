
#### To make EucFACE P summary table by CO2 treatment
#### Ignore time but produce time coverage information
#### This is for pools

make_pool_summary_table_by_treatment_normalized <- function() {
    
    ### Define pool variable names
    terms <- c("Wood P Pool", "Canopy P Pool", "Fine Root P Pool",
               "Coarse Root P Pool", "Understorey P Pool", 
               "Microbial P Pool", "Soil Phosphate P Pool",
               "Soil P Pool", "Mycorrhizal P Pool",
               "Exhanagable Pi Pool", "Exhanagable Po Pool",
               "Moderately labile Po Pool", "Secondary Fe bound Pi Pool", "Primary Ca bound Pi Pool",
               "Occluded P Pool")
    
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
    
    ### Canopy P 
    out <- summaryBy(predicted~Ring,data=canopy_p_pool_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Canopy P Pool", 2:7] <- out$predicted


    
    ### Wood P 
    out <- summaryBy(predicted~Ring,data=wood_p_pool_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Wood P Pool", 2:7] <- out$predicted
    
    
    ### Fine root P pool
    out <- summaryBy(predicted~Ring,data=fineroot_p_pool_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Fine Root P Pool", 2:7] <- out$predicted
    
    ### Coarse root P pool
    out <- summaryBy(predicted~Ring,data=coarse_root_p_pool_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Coarse Root P Pool", 2:7] <- out$predicted
    
    ### Understorey P pool
    out <- summaryBy(predicted~Ring,data=understorey_p_pool_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Understorey P Pool", 2:7] <- out$predicted
    
    ### Understorey Litter P pool
    
    ### Microbial P pool
    out <- summaryBy(predicted~Ring,data=microbial_p_pool_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Microbial P Pool", 2:7] <- out$predicted
    
    ### Soil Phosphate P pool
    out <- summaryBy(predicted~Ring,data=soil_phosphate_pool_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Soil Phosphate P Pool", 2:7] <- out$predicted
    
    ### Soil P pool
    out <- summaryBy(predicted~Ring,data=soil_p_pool_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Soil P Pool", 2:7] <- out$predicted
    

    ### Mycorrhizal P pool
    
    

    ### Exhanagable Pi Pool
    out <- summaryBy(predicted~Ring,data=soil_exhanagable_pi_pool_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Exhanagable Pi Pool", 2:7] <- out$predicted
    
    ### Exhanagable Po Pool
    out <- summaryBy(predicted~Ring,data=soil_exhanagable_po_pool_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Exhanagable Po Pool", 2:7] <- out$predicted
    
    
    ### Moderately labile Po Pool
    out <- summaryBy(predicted~Ring,data=soil_mlabile_po_pool_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Moderately labile Po Pool", 2:7] <- out$predicted
    
    ### Secondary Fe bound Pi Pool
    out <- summaryBy(predicted~Ring,data=soil_secondary_pi_pool_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Secondary Fe bound Pi Pool", 2:7] <- out$predicted
    
    ### Primary Ca bound Pi Pool
    out <- summaryBy(predicted~Ring,data=soil_primary_pi_pool_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Primary Ca bound Pi Pool", 2:7] <- out$predicted
    
    
    ### Occluded P Pool
    out <- summaryBy(predicted~Ring,data=soil_occluded_p_pool_pred,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Occluded P Pool", 2:7] <- out$predicted
    
    ### calculate treatment averages
    treatDF$aCO2 <- round(rowMeans(subset(treatDF, select=c(R2, R3, R6)), na.rm=T), 5)
    treatDF$eCO2 <- round(rowMeans(subset(treatDF, select=c(R1, R4, R5)), na.rm=T), 5)
    
    treatDF$aCO2_sd <- rowSds(as.matrix(subset(treatDF, select=c(R2, R3, R6))), na.rm=T)
    treatDF$eCO2_sd <- rowSds(as.matrix(subset(treatDF, select=c(R1, R4, R5))), na.rm=T)
    
    
    
    ##### output tables
    return(treatDF)
      
}

