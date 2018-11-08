
#### To make EucFACE P summary table by CO2 treatment
#### Ignore time but produce time coverage information
#### This is for fluxes

make_flux_summary_table_by_treatment_bootstrap <- function() {
    
    ### Define production variable names
    terms <- c("Wood P flux", "Canopy P flux", "Fine Root P flux",
               "Coarse Root P flux","Leaflitter P flux", "Fineroot Litter P flux",
               "Twig litter P flux", "Bark litter P flux","Seed litter P flux", "Frass P flux",
               "Understorey P flux", "Understorey Litter P flux", "Mineralization P flux")
    
    treatDF <- data.frame(terms)
    
    treatDF$aCO2 <- rep(NA, length(treatDF$terms))
    treatDF$eCO2 <- rep(NA, length(treatDF$terms))
    treatDF$aCO2_sd <- rep(NA, length(treatDF$terms))
    treatDF$eCO2_sd <- rep(NA, length(treatDF$terms))
    
    ### Canopy P flux
    out1 <- summaryBy(predicted~Trt,data=canopy_p_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=canopy_p_flux_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Canopy P flux"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Canopy P flux"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Canopy P flux"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Canopy P flux"] <- out2$predicted[out2$Trt=="ele"]

    
    ### Wood P 
    out1 <- summaryBy(predicted~Trt,data=wood_p_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=wood_p_flux_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Wood P flux"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Wood P flux"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Wood P flux"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Wood P flux"] <- out2$predicted[out2$Trt=="ele"]
    
    ### Fine root P flux
    out1 <- summaryBy(predicted~Trt,data=fineroot_p_production_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=fineroot_p_production_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Fine Root P flux"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Fine Root P flux"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Fine Root P flux"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Fine Root P flux"] <- out2$predicted[out2$Trt=="ele"]
    
    ### Coarse root P flux
    out1 <- summaryBy(predicted~Trt,data=coarse_root_p_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=coarse_root_p_flux_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Coarse Root P flux"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Coarse Root P flux"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Coarse Root P flux"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Coarse Root P flux"] <- out2$predicted[out2$Trt=="ele"]
    
    ### Understorey P flux
    out1 <- summaryBy(predicted~Trt,data=understorey_p_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=understorey_p_flux_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Understorey P flux"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Understorey P flux"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Understorey P flux"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Understorey P flux"] <- out2$predicted[out2$Trt=="ele"]
    
    ### Understorey Litter P flux
    out1 <- summaryBy(predicted~Trt,data=understorey_litter_p_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=understorey_litter_p_flux_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Understorey Litter P flux"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Understorey Litter P flux"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Understorey Litter P flux"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Understorey Litter P flux"] <- out2$predicted[out2$Trt=="ele"]

    ### Mineralization flux
    out1 <- summaryBy(predicted~Trt,data=soil_p_mineralization_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=soil_p_mineralization_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Mineralization P flux"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Mineralization P flux"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Mineralization P flux"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Mineralization P flux"] <- out2$predicted[out2$Trt=="ele"]
    
    ### Frass production flux
    out1 <- summaryBy(predicted~Trt,data=frass_p_production_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=frass_p_production_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Frass P flux"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Frass P flux"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Frass P flux"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Frass P flux"] <- out2$predicted[out2$Trt=="ele"]
    
    ### Leaf litter flux
    out1 <- summaryBy(predicted~Trt,data=leaflitter_p_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=leaflitter_p_flux_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Leaflitter P flux"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Leaflitter P flux"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Leaflitter P flux"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Leaflitter P flux"] <- out2$predicted[out2$Trt=="ele"]
    
    ### Fine Root litter flux

    
    ### twig litter flux
    out1 <- summaryBy(predicted~Trt,data=twig_litter_p_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=twig_litter_p_flux_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Twig Litter P flux"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Twig Litter P flux"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Twig Litter P flux"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Twig Litter P flux"] <- out2$predicted[out2$Trt=="ele"]
    
    ### bark litter flux
    out1 <- summaryBy(predicted~Trt,data=bark_litter_p_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=bark_litter_p_flux_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Bark Litter P flux"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Bark Litter P flux"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Bark Litter P flux"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Bark Litter P flux"] <- out2$predicted[out2$Trt=="ele"]
    
    ### seed litter flux
    out1 <- summaryBy(predicted~Trt,data=seed_litter_p_flux_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=seed_litter_p_flux_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$terms == "Seed Litter P flux"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$terms == "Seed Litter P flux"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$terms == "Seed Litter P flux"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$terms == "Seed Litter P flux"] <- out2$predicted[out2$Trt=="ele"]
    
    
    ##### output tables
    return(treatDF)
      
}

