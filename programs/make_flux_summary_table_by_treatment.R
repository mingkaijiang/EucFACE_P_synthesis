
#### To make EucFACE P summary table by CO2 treatment
#### Ignore time but produce time coverage information
#### This is for fluxes

make_flux_summary_table_by_treatment <- function() {
    
    ### Define production variable names
    terms <- c("Wood P flux", "Canopy P flux", "Fine Root P flux",
               "Leaflitter P flux", "Other litter P flux", "Frass P flux",
               "Understorey P flux", "Mineralization P flux")
    
    treatDF <- data.frame(terms)
    treatDF$R1 <- rep(NA, length(treatDF$terms))
    treatDF$R2 <- rep(NA, length(treatDF$terms))
    treatDF$R3 <- rep(NA, length(treatDF$terms))
    treatDF$R4 <- rep(NA, length(treatDF$terms))
    treatDF$R5 <- rep(NA, length(treatDF$terms))
    treatDF$R6 <- rep(NA, length(treatDF$terms))
    
    treatDF$aCO2 <- rep(NA, length(treatDF$terms))
    treatDF$eCO2 <- rep(NA, length(treatDF$terms))
    treatDF$year_start <- rep(NA, length(treatDF$terms))
    treatDF$year_end <- rep(NA, length(treatDF$terms))
    treatDF$timepoint <- rep(NA, length(treatDF$terms))
    treatDF$notes <- rep(NA, length(treatDF$terms))
    
    ### Canopy P flux
    # out <- summaryBy(leaf_p_production~Ring,data=canopy_p_production,FUN=mean,keep.names=T,na.rm=T)
    # treatDF[treatDF$terms == "Canopy P flux", 2:7] <- out$leaf_p_production
    # treatDF$year_start[treatDF$terms == "Canopy P flux"] <- min(year(canopy_p_production$Date))    
    # treatDF$year_end[treatDF$terms == "Canopy P flux"] <- max(year(canopy_p_production$Date))    
    # treatDF$timepoint[treatDF$terms == "Canopy P flux"] <- length(unique(canopy_p_production$Date))  
    treatDF$notes[treatDF$terms == "Canopy P flux"] <- "need to consider leaf turnover"

    
    ### Wood P 
    out <- summaryBy(wood_p_flux~Ring,data=wood_p_flux,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Wood P flux", 2:7] <- out$wood_p_flux
    treatDF$year_start[treatDF$terms == "Wood P flux"] <- min(year(wood_p_flux$Date))    
    treatDF$year_end[treatDF$terms == "Wood P flux"] <- max(year(wood_p_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Wood P flux"] <- length(unique(wood_p_flux$Date)) 
    treatDF$notes[treatDF$terms == "Wood P flux"] <- "Based on single time point measurement"
    
    
    ### Fine root P flux
    out <- summaryBy(fineroot_p_flux_mg_m2_d~Ring,data=fineroot_p_production,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Fine Root P flux", 2:7] <- out$fineroot_p_flux_mg_m2_d
    treatDF$year_start[treatDF$terms == "Fine Root P flux"] <- min(year(fineroot_p_production$Date))    
    treatDF$year_end[treatDF$terms == "Fine Root P flux"] <- max(year(fineroot_p_production$Date))    
    treatDF$timepoint[treatDF$terms == "Fine Root P flux"] <- length(unique(fineroot_p_production$Date))  
    treatDF$notes[treatDF$terms == "Fine Root P flux"] <- "Top 30 cm"
    
    
    ### Understorey P flux
    out <- summaryBy(understorey_p_flux~Ring,data=understorey_p_flux,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Understorey P flux", 2:7] <- out$understorey_p_flux
    treatDF$year_start[treatDF$terms == "Understorey P flux"] <- min(year(understorey_p_flux$Date))    
    treatDF$year_end[treatDF$terms == "Understorey P flux"] <- max(year(understorey_p_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Understorey P flux"] <- length(unique(understorey_p_flux$Date))  
    treatDF$notes[treatDF$terms == "Understorey P flux"] <- "Used Varsha's harvest data"
    

    ### Mineralization flux
    out <- summaryBy(p_mineralization_mg_m2_d~Ring,data=soil_p_mineralization,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Mineralization P flux", 2:7] <- out$p_mineralization_mg_m2_d
    treatDF$year_start[treatDF$terms == "Mineralization P flux"] <- min(year(soil_p_mineralization$date))    
    treatDF$year_end[treatDF$terms == "Mineralization P flux"] <- max(year(soil_p_mineralization$date))    
    treatDF$timepoint[treatDF$terms == "Mineralization P flux"] <- length(unique(soil_p_mineralization$date))  
    treatDF$notes[treatDF$terms == "Mineralization P flux"] <- "positive is mineralization, negative is immobilization"
    
    ### Frass production flux
    out <- summaryBy(frass_p_flux_mg_m2_d~Ring,data=frass_p_production,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Frass P flux", 2:7] <- out$frass_p_flux_mg_m2_d
    treatDF$year_start[treatDF$terms == "Frass P flux"] <- min(year(frass_p_production$Date))    
    treatDF$year_end[treatDF$terms == "Frass P flux"] <- max(year(frass_p_production$Date))    
    treatDF$timepoint[treatDF$terms == "Frass P flux"] <- length(unique(frass_p_production$Date))  
    treatDF$notes[treatDF$terms == "Frass P flux"] <- "NA"
    
    ### Leaf litter flux
    out <- summaryBy(leaflitter_p_flux_mg_m2_d~Ring,data=leaflitter_p_flux,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Leaflitter P flux", 2:7] <- out$leaflitter_p_flux_mg_m2_d
    treatDF$year_start[treatDF$terms == "Leaflitter P flux"] <- min(year(leaflitter_p_flux$Date))    
    treatDF$year_end[treatDF$terms == "Leaflitter P flux"] <- max(year(leaflitter_p_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Leaflitter P flux"] <- length(unique(leaflitter_p_flux$Date))  
    treatDF$notes[treatDF$terms == "Leaflitter P flux"] <- "Only leaves, exclude twig, barks and seeds"
    
    
    ### Other aboveground litter flux
    out <- summaryBy(other_litter_p_flux_mg_m2_d~Ring,data=other_litter_p_flux,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Other litter P flux", 2:7] <- out$other_litter_p_flux_mg_m2_d
    treatDF$year_start[treatDF$terms == "Other litter P flux"] <- min(year(other_litter_p_flux$Date))    
    treatDF$year_end[treatDF$terms == "Other litter P flux"] <- max(year(other_litter_p_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Other litter P flux"] <- length(unique(other_litter_p_flux$Date))  
    treatDF$notes[treatDF$terms == "Other litter P flux"] <- "Assume wood P concentration applies to twig, bark and seed"
    
    
    ### calculate treatment averages
    treatDF$aCO2 <- round(rowMeans(subset(treatDF, select=c(R2, R3, R6)), na.rm=T), 5)
    treatDF$eCO2 <- round(rowMeans(subset(treatDF, select=c(R1, R4, R5)), na.rm=T), 5)
    
    
    ##### output tables
    return(treatDF)
      
}

