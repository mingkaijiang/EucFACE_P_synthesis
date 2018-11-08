
#### To make EucFACE P summary table by CO2 treatment
#### Ignore time but produce time coverage information
#### This is for pools

make_pool_summary_table_by_treatment_bootstrap <- function() {
    
    ### Define pool variable names
    terms <- c("Wood P Pool", "Canopy P Pool", "Fine Root P Pool",
               "Coarse Root P Pool", "Understorey P Pool", 
               "Microbial P Pool", "Soil Phosphate P Pool",
               "Soil P Pool", "Mycorrhizal P Pool")
    
    treatDF <- data.frame(terms)
    
    treatDF$aCO2 <- rep(NA, length(treatDF$terms))
    treatDF$eCO2 <- rep(NA, length(treatDF$terms))
    treatDF$aCO2_sd <- rep(NA, length(treatDF$terms))
    treatDF$eCO2_sd <- rep(NA, length(treatDF$terms))
    
    ### Canopy P 
    out1 <- summaryBy(predicted~Trt,data=canopy_p_pool_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=canopy_p_pool_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Canopy P Pool"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Canopy P Pool"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Canopy P Pool"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Canopy P Pool"] <- out2$predicted[out2$Trt=="ele"]

    
    ### Wood P 
    out1 <- summaryBy(predicted~Trt,data=wood_p_pool_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=wood_p_pool_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Wood P Pool"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Wood P Pool"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Wood P Pool"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Wood P Pool"] <- out2$predicted[out2$Trt=="ele"]
    
    
    ### Fine root P pool
    out1 <- summaryBy(predicted~Trt,data=fineroot_p_pool_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=fineroot_p_pool_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Fine Root P Pool"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Fine Root P Pool"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Fine Root P Pool"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Fine Root P Pool"] <- out2$predicted[out2$Trt=="ele"]
    
    ### Coarse root P pool
    out1 <- summaryBy(predicted~Trt,data=coarse_root_p_pool_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=coarse_root_p_pool_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Coarse Root P Pool"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Coarse Root P Pool"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Coarse Root P Pool"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Coarse Root P Pool"] <- out2$predicted[out2$Trt=="ele"]
    
    ### Understorey P pool
    out1 <- summaryBy(predicted~Trt,data=understorey_p_pool_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=understorey_p_pool_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Understorey P Pool"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Understorey P Pool"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Understorey P Pool"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Understorey P Pool"] <- out2$predicted[out2$Trt=="ele"]
    
    ### Understorey Litter P pool

    
    ### Microbial P pool
    out1 <- summaryBy(predicted~Trt,data=microbial_p_pool_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=microbial_p_pool_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Microbial P Pool"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Microbial P Pool"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Microbial P Pool"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Microbial P Pool"] <- out2$predicted[out2$Trt=="ele"]
    
    ### Soil Phosphate P pool
    out1 <- summaryBy(predicted~Trt,data=soil_phosphate_pool_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=soil_phosphate_pool_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Soil Phosphate P Pool"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Soil Phosphate P Pool"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Soil Phosphate P Pool"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Soil Phosphate P Pool"] <- out2$predicted[out2$Trt=="ele"]
    
    ### Soil P pool
    out1 <- summaryBy(predicted~Trt,data=soil_p_pool_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=soil_p_pool_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Soil P Pool"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Soil P Pool"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Soil P Pool"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Soil P Pool"] <- out2$predicted[out2$Trt=="ele"]
    

    ### Mycorrhizal P pool


    
    ##### output tables
    return(treatDF)
      
}

