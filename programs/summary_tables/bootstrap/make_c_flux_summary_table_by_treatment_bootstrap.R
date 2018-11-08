
#### To make EucFACE C summary table by CO2 treatment
#### Ignore time but produce time coverage information
#### This is for fluxes

make_c_flux_summary_table_by_treatment_bootstrap <- function() {
    
    ### convert daily flux in mg C m2 d-1 to g C m-2 yr-1
    conv <- 365 / 1000
    
    ### Define production variable names
    terms <- c("Wood C flux", "Canopy C flux", "Fine Root C flux",
               "Coarse Root C flux","Leaflitter C flux", "Twiglitter C flux",
               "Barklitter C flux","Seedlitter C flux",
               "Fineroot Litter C flux",
               "Frass C flux","Understorey C flux")
    
    treatDF <- data.frame(terms)
    treatDF$aCO2 <- rep(NA, length(treatDF$terms))
    treatDF$eCO2 <- rep(NA, length(treatDF$terms))
    treatDF$aCO2_sd <- rep(NA, length(treatDF$terms))
    treatDF$eCO2_sd <- rep(NA, length(treatDF$terms))
    
    ### Canopy C flux
    out1 <- summaryBy(predicted~Trt,data=leaflitter_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=leaflitter_flux_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Canopy C flux"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Canopy C flux"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Canopy C flux"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Canopy C flux"] <- out2$predicted[out2$Trt=="ele"]
    
    
    ### Wood C 
    out1 <- summaryBy(predicted~Trt,data=wood_production_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=wood_production_flux_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Wood C flux"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Wood C flux"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Wood C flux"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Wood C flux"] <- out2$predicted[out2$Trt=="ele"]
    
    ### Fine root C flux
    out1 <- summaryBy(predicted~Trt,data=fineroot_production_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=fineroot_production_flux_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Fine Root C flux"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Fine Root C flux"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Fine Root C flux"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Fine Root C flux"] <- out2$predicted[out2$Trt=="ele"]
    
    ### Coarse root C flux
    out1 <- summaryBy(predicted~Trt,data=coarse_root_production_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=coarse_root_production_flux_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Coarse Root C flux"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Coarse Root C flux"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Coarse Root C flux"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Coarse Root C flux"] <- out2$predicted[out2$Trt=="ele"]
    
    ### Understorey C flux
    out1 <- summaryBy(predicted~Trt,data=understorey_aboveground_production_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=understorey_aboveground_production_flux_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Understorey C flux"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Understorey C flux"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Understorey C flux"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Understorey C flux"] <- out2$predicted[out2$Trt=="ele"]
    
    ### Frass production flux
    out1 <- summaryBy(predicted~Trt,data=frass_c_production_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=frass_c_production_flux_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Frass C flux"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Frass C flux"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Frass C flux"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Frass C flux"] <- out2$predicted[out2$Trt=="ele"]
    
    ### Leaf litter flux
    out1 <- summaryBy(predicted~Trt,data=leaflitter_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=leaflitter_flux_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Leaflitter C flux"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Leaflitter C flux"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Leaflitter C flux"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Leaflitter C flux"] <- out2$predicted[out2$Trt=="ele"]
    
    ### Twig litter flux
    out1 <- summaryBy(predicted~Trt,data=twiglitter_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=twiglitter_flux_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Twiglitter C flux"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Twiglitter C flux"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Twiglitter C flux"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Twiglitter C flux"] <- out2$predicted[out2$Trt=="ele"]
    
    ### Bark litter flux
    out1 <- summaryBy(predicted~Trt,data=barklitter_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=barklitter_flux_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Barklitter C flux"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Barklitter C flux"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Barklitter C flux"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Barklitter C flux"] <- out2$predicted[out2$Trt=="ele"]
    
    ### Seed litter flux
    out1 <- summaryBy(predicted~Trt,data=seedlitter_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=seedlitter_flux_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Seedlitter C flux"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Seedlitter C flux"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Seedlitter C flux"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Seedlitter C flux"] <- out2$predicted[out2$Trt=="ele"]
    
    ### Fine Root litter flux
    # assume it's the same as fine root production flux
    out1 <- summaryBy(predicted~Trt,data=fineroot_production_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=fineroot_production_flux_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Fineroot Litter C flux"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Fineroot Litter C flux"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Fineroot Litter C flux"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Fineroot Litter C flux"] <- out2$predicted[out2$Trt=="ele"]
    

    
    ##### output tables
    return(treatDF)
      
}

