
#### To make EucFACE P summary table by CO2 treatment
#### Ignore time but produce time coverage information
#### This is for fluxes

make_flux_summary_table_by_treatment <- function() {
    
    ### convert daily flux in mg P m2 d-1 to g P m-2 yr-1
    conv <- 365 / 1000
    
    ### Define production variable names
    terms <- c("Wood P flux", "Canopy P flux", "Fine Root P flux",
               "Coarse Root P flux",
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
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Canopy P flux", i+1] <- with(canopy_p_flux[canopy_p_flux$Ring ==i,],
                                                              sum(canopy_p_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Canopy P flux"] <- min(year(canopy_p_flux$Date))    
    treatDF$year_end[treatDF$terms == "Canopy P flux"] <- max(year(canopy_p_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Canopy P flux"] <- length(unique(canopy_p_flux$Date))  
    treatDF$notes[treatDF$terms == "Canopy P flux"] <- "need to consider leaf turnover"

    
    ### Wood P 
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Wood P flux", i+1] <- with(wood_p_flux[wood_p_flux$Ring ==i,],
                                                             sum(wood_p_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Wood P flux"] <- min(year(wood_p_flux$Date))    
    treatDF$year_end[treatDF$terms == "Wood P flux"] <- max(year(wood_p_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Wood P flux"] <- length(unique(wood_p_flux$Date)) 
    treatDF$notes[treatDF$terms == "Wood P flux"] <- "Based on single time point measurement"
    
    ### Fine root P flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Fine Root P flux", i+1] <- with(fineroot_p_production[fineroot_p_production$Ring ==i,],
                                                             sum(fineroot_p_flux_mg_m2_d*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Fine Root P flux"] <- min(year(fineroot_p_production$Date))    
    treatDF$year_end[treatDF$terms == "Fine Root P flux"] <- max(year(fineroot_p_production$Date))    
    treatDF$timepoint[treatDF$terms == "Fine Root P flux"] <- length(unique(fineroot_p_production$Date))  
    treatDF$notes[treatDF$terms == "Fine Root P flux"] <- "Top 30 cm"
    
    ### Coarse root P flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Coarse Root P flux", i+1] <- with(coarse_root_p_flux_1[coarse_root_p_flux_1$Ring ==i,],
                                                                  sum(coarse_root_p_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Coarse Root P flux"] <- min(year(coarse_root_p_flux_1$Date))    
    treatDF$year_end[treatDF$terms == "Coarse Root P flux"] <- max(year(coarse_root_p_flux_1$Date))    
    treatDF$timepoint[treatDF$terms == "Coarse Root P flux"] <- length(unique(coarse_root_p_flux_1$Date))  
    treatDF$notes[treatDF$terms == "Coarse Root P flux"] <- "Allometric rlt with DBH"
    
    ### Understorey P flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Understorey P flux", i+1] <- with(understorey_p_flux[understorey_p_flux$Ring ==i,],
                                                                    sum(understorey_p_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Understorey P flux"] <- min(year(understorey_p_flux$Date))    
    treatDF$year_end[treatDF$terms == "Understorey P flux"] <- max(year(understorey_p_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Understorey P flux"] <- length(unique(understorey_p_flux$Date))  
    treatDF$notes[treatDF$terms == "Understorey P flux"] <- "Used Varsha's harvest data"
    

    ### Mineralization flux
    out <- summaryBy(p_mineralization_mg_m2_d~Ring,data=soil_p_mineralization,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Mineralization P flux", 2:7] <- out$p_mineralization_mg_m2_d * conv
    treatDF$year_start[treatDF$terms == "Mineralization P flux"] <- min(year(soil_p_mineralization$date))    
    treatDF$year_end[treatDF$terms == "Mineralization P flux"] <- max(year(soil_p_mineralization$date))    
    treatDF$timepoint[treatDF$terms == "Mineralization P flux"] <- length(unique(soil_p_mineralization$date))  
    treatDF$notes[treatDF$terms == "Mineralization P flux"] <- "positive is mineralization, negative is immobilization"
    
    ### Frass production flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Frass P flux", i+1] <- with(frass_p_production[frass_p_production$Ring ==i,],
                                                                    sum(frass_p_flux_mg_m2_d*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Frass P flux"] <- min(year(frass_p_production$Date))    
    treatDF$year_end[treatDF$terms == "Frass P flux"] <- max(year(frass_p_production$Date))    
    treatDF$timepoint[treatDF$terms == "Frass P flux"] <- length(unique(frass_p_production$Date))  
    treatDF$notes[treatDF$terms == "Frass P flux"] <- "NA"
    
    ### Leaf litter flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Leaflitter P flux", i+1] <- with(leaflitter_p_flux[leaflitter_p_flux$Ring ==i,],
                                                              sum(leaflitter_p_flux_mg_m2_d*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Leaflitter P flux"] <- min(year(leaflitter_p_flux$Date))    
    treatDF$year_end[treatDF$terms == "Leaflitter P flux"] <- max(year(leaflitter_p_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Leaflitter P flux"] <- length(unique(leaflitter_p_flux$Date))  
    treatDF$notes[treatDF$terms == "Leaflitter P flux"] <- "Only leaves, exclude twig, barks and seeds"
    
    
    ### Other aboveground litter flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Other litter P flux", i+1] <- with(other_litter_p_flux[other_litter_p_flux$Ring ==i,],
                                                                   sum(other_litter_p_flux_mg_m2_d*Days)/sum(Days)) * conv
    }
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

