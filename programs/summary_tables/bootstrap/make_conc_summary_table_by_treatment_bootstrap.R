
#### To make EucFACE P summary table by CO2 treatment
#### Ignore time but produce time coverage information

make_conc_summary_table_by_treatment_bootstrap <- function() {
    
    ### Define concentration variable names
    conc.terms <- c("Wood P Conc", "Canopy P Conc", "Fine Root P Conc", "Coarse Root P Conc",
                    "Leaflitter P Conc","Understorey P Conc", "Understorey Litter P Conc", "Frass P Conc",
                    "Microbial P Conc", "Soil P Conc", "Soil Phosphate P Conc",
                    "Mycorrhizal P Conc")
    
    treatDF <- data.frame(conc.terms)

    treatDF$aCO2 <- rep(NA, length(treatDF$conc.terms))
    treatDF$eCO2 <- rep(NA, length(treatDF$conc.terms))
    treatDF$aCO2_sd <- rep(NA, length(treatDF$conc.terms))
    treatDF$eCO2_sd <- rep(NA, length(treatDF$conc.terms))
    
    ### Canopy P concentration
    out1 <- summaryBy(predicted~Trt,data=canopy_p_concentration_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=canopy_p_concentration_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$conc.terms == "Canopy P Conc"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$conc.terms == "Canopy P Conc"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$conc.terms == "Canopy P Conc"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$conc.terms == "Canopy P Conc"] <- out2$predicted[out2$Trt=="ele"]

    
    ### Wood P concentration

    
    ### Coarse root P concentration
    
    
    ### Fine root P concentration
    out1 <- summaryBy(predicted~Trt,data=fineroot_p_concentration_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=fineroot_p_concentration_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$conc.terms == "Fine Root P Conc"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$conc.terms == "Fine Root P Conc"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$conc.terms == "Fine Root P Conc"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$conc.terms == "Fine Root P Conc"] <- out2$predicted[out2$Trt=="ele"]
    
    
    
    ### Leaf litter P concentration
    out1 <- summaryBy(predicted~Trt,data=leaflitter_p_concentration_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=leaflitter_p_concentration_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$conc.terms == "Leaflitter P Conc"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$conc.terms == "Leaflitter P Conc"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$conc.terms == "Leaflitter P Conc"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$conc.terms == "Leaflitter P Conc"] <- out2$predicted[out2$Trt=="ele"]

    
    ### Understorey P concentration
    out1 <- summaryBy(predicted~Trt,data=understorey_p_concentration_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=understorey_p_concentration_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$conc.terms == "Understorey P Conc"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$conc.terms == "Understorey P Conc"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$conc.terms == "Understorey P Conc"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$conc.terms == "Understorey P Conc"] <- out2$predicted[out2$Trt=="ele"]
    
    ### Understorey Litter P concentration


    ### Frass P concentration
    out1 <- summaryBy(predicted~Trt,data=frass_p_concentration_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=frass_p_concentration_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$conc.terms == "Frass P Conc"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$conc.terms == "Frass P Conc"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$conc.terms == "Frass P Conc"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$conc.terms == "Frass P Conc"] <- out2$predicted[out2$Trt=="ele"]
    
    ### Microbial P concentration
    out1 <- summaryBy(predicted~Trt,data=microbial_p_concentration_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=microbial_p_concentration_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$conc.terms == "Microbial P Conc"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$conc.terms == "Microbial P Conc"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$conc.terms == "Microbial P Conc"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$conc.terms == "Microbial P Conc"] <- out2$predicted[out2$Trt=="ele"]
    
    ### Soil P concentration
    out1 <- summaryBy(predicted~Trt,data=soil_p_concentration_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=soil_p_concentration_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$conc.terms == "Soil P Conc"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$conc.terms == "Soil P Conc"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$conc.terms == "Soil P Conc"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$conc.terms == "Soil P Conc"] <- out2$predicted[out2$Trt=="ele"]
    
    ### Soil Phosphate P concentration
    out1 <- summaryBy(predicted~Trt,data=soil_phosphate_concentration_pred,FUN=mean,keep.names=T,na.rm=T)
    out2 <- summaryBy(predicted~Trt,data=soil_phosphate_concentration_pred,FUN=sd,keep.names=T,na.rm=T)
    
    treatDF$aCO2[treatDF$conc.terms == "Soil Phosphase P Conc"] <- out1$predicted[out1$Trt=="amb"]
    treatDF$eCO2[treatDF$conc.terms == "Soil Phosphase P Conc"] <- out1$predicted[out1$Trt=="ele"]
    treatDF$aCO2_sd[treatDF$conc.terms == "Soil Phosphase P Conc"] <- out2$predicted[out2$Trt=="amb"]
    treatDF$eCO2_sd[treatDF$conc.terms == "Soil Phosphase P Conc"] <- out2$predicted[out2$Trt=="ele"]
    
    ### Mycorrhizal P concentration

    
    ##### output tables
    return(treatDF)
      
}

