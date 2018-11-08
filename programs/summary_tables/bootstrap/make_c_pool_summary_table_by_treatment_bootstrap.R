
#### To make EucFACE C summary table by CO2 treatment
#### Ignore time but produce time coverage information
#### This is for pools

make_c_pool_summary_table_by_treatment_bootstrap <- function() {
    
    ### Define pool variable names
    terms <- c("Wood C Pool", "Canopy C Pool", "Fine Root C Pool",
               "Coarse Root C Pool", "Understorey C Pool", 
               "Microbial C Pool", 
               "Soil C Pool", "Mycorrhizal C Pool")
    
    treatDF <- data.frame(terms)
    
    treatDF$aCO2 <- rep(NA, length(treatDF$terms))
    treatDF$eCO2 <- rep(NA, length(treatDF$terms))
    treatDF$aCO2_sd <- rep(NA, length(treatDF$terms))
    treatDF$eCO2_sd <- rep(NA, length(treatDF$terms))
    
    ### Canopy C 
    out1 <- summaryBy(predicted~Trt,data=canopy_biomass_pool_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=canopy_biomass_pool_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Canopy C Pool"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Canopy C Pool"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Canopy C Pool"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Canopy C Pool"] <- out2$predicted[out2$Trt=="ele"]

    
    ### Wood C 
    out1 <- summaryBy(predicted~Trt,data=wood_c_pool_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=wood_c_pool_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Wood C Pool"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Wood C Pool"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Wood C Pool"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Wood C Pool"] <- out2$predicted[out2$Trt=="ele"]
    
    
    ### Fine root C pool
    out1 <- summaryBy(predicted~Trt,data=fineroot_c_pool_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=fineroot_c_pool_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Fine Root C Pool"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Fine Root C Pool"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Fine Root C Pool"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Fine Root C Pool"] <- out2$predicted[out2$Trt=="ele"]
    
    ### Coarse root C pool
    out1 <- summaryBy(predicted~Trt,data=coarse_root_c_pool_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=coarse_root_c_pool_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Coarse Root C Pool"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Coarse Root C Pool"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Coarse Root C Pool"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Coarse Root C Pool"] <- out2$predicted[out2$Trt=="ele"]
    
    ### Understorey C pool
    out1 <- summaryBy(predicted~Trt,data=understorey_aboveground_c_pool_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=understorey_aboveground_c_pool_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Understorey C Pool"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Understorey C Pool"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Understorey C Pool"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Understorey C Pool"] <- out2$predicted[out2$Trt=="ele"]

    
    ### Microbial C pool
    out1 <- summaryBy(predicted~Trt,data=microbial_c_pool_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=microbial_c_pool_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Microbial C Pool"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Microbial C Pool"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Microbial C Pool"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Microbial C Pool"] <- out2$predicted[out2$Trt=="ele"]
    
    
    ### Soil C pool
    out1 <- summaryBy(predicted~Trt,data=soil_c_pool_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=soil_c_pool_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Soil C Pool"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Soil C Pool"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Soil C Pool"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Soil C Pool"] <- out2$predicted[out2$Trt=="ele"]
    

    ### Mycorrhizal C pool
    out1 <- summaryBy(predicted~Trt,data=mycorrhizal_c_pool_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=mycorrhizal_c_pool_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Mycorrhizal C Pool"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Mycorrhizal C Pool"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Mycorrhizal C Pool"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Mycorrhizal C Pool"] <- out2$predicted[out2$Trt=="ele"]
    
    

    ##### output tables
    return(treatDF)
      
}

