
#### To make EucFACE P summary table by CO2 treatment
#### Ignore time but produce time coverage information

make_conc_summary_table_by_treatment <- function() {
    
    ### Define concentration variable names
    conc.terms <- c("Wood P Conc", "Canopy P Conc", "Fine Root P Conc",
                    "Leaflitter P Conc","Understorey P Conc", "Frass P Conc",
                    "Microbial P Conc", "Soil P Conc", "Soil Phosphate P Conc",
                    "Mycorrhizal P Conc")
    
    treatDF <- data.frame(conc.terms)
    treatDF$R1 <- rep(NA, length(treatDF$conc.terms))
    treatDF$R2 <- rep(NA, length(treatDF$conc.terms))
    treatDF$R3 <- rep(NA, length(treatDF$conc.terms))
    treatDF$R4 <- rep(NA, length(treatDF$conc.terms))
    treatDF$R5 <- rep(NA, length(treatDF$conc.terms))
    treatDF$R6 <- rep(NA, length(treatDF$conc.terms))
    
    treatDF$aCO2 <- rep(NA, length(treatDF$conc.terms))
    treatDF$eCO2 <- rep(NA, length(treatDF$conc.terms))
    treatDF$year_start <- rep(NA, length(treatDF$conc.terms))
    treatDF$year_end <- rep(NA, length(treatDF$conc.terms))
    treatDF$timepoint <- rep(NA, length(treatDF$conc.terms))
    treatDF$notes <- rep(NA, length(treatDF$conc.terms))
    
    ### Canopy P concentration
    out <- summaryBy(PercP~Ring,data=canopy_p_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Canopy P Conc", 2:7] <- out$PercP
    treatDF$year_start[treatDF$conc.terms == "Canopy P Conc"] <- min(year(canopy_p_concentration$Date))    
    treatDF$year_end[treatDF$conc.terms == "Canopy P Conc"] <- max(year(canopy_p_concentration$Date))    
    treatDF$timepoint[treatDF$conc.terms == "Canopy P Conc"] <- length(unique(canopy_p_concentration$Date))  
    treatDF$notes[treatDF$conc.terms == "Canopy P Conc"] <- "Only green leaf"

    
    ### Wood P concentration
    out <- summaryBy(PercP~Ring,data=wood_p_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Wood P Conc", 2:7] <- out$PercP
    treatDF$year_start[treatDF$conc.terms == "Wood P Conc"] <- min(year(wood_p_concentration$Date))    
    treatDF$year_end[treatDF$conc.terms == "Wood P Conc"] <- max(year(wood_p_concentration$Date))    
    treatDF$timepoint[treatDF$conc.terms == "Wood P Conc"] <- length(unique(wood_p_concentration$Date)) 
    treatDF$notes[treatDF$conc.terms == "Wood P Conc"] <- "Only one data point per ring"
    
    
    ### Fine root P concentration
    out <- summaryBy(PercP~Ring,data=fineroot_p_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Fine Root P Conc", 2:7] <- out$PercP
    treatDF$year_start[treatDF$conc.terms == "Fine Root P Conc"] <- min(year(fineroot_p_concentration$Date))    
    treatDF$year_end[treatDF$conc.terms == "Fine Root P Conc"] <- max(year(fineroot_p_concentration$Date))    
    treatDF$timepoint[treatDF$conc.terms == "Fine Root P Conc"] <- length(unique(fineroot_p_concentration$Date))  
    treatDF$notes[treatDF$conc.terms == "Fine Root P Conc"] <- "Averaged over depths of 0-10cm and 10-30cm"
    
    
    ### Leaf litter P concentration
    out <- summaryBy(PercP~Ring,data=leaflitter_p_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Leaflitter P Conc", 2:7] <- out$PercP
    treatDF$year_start[treatDF$conc.terms == "Leaflitter P Conc"] <- min(year(leaflitter_p_concentration$Date))    
    treatDF$year_end[treatDF$conc.terms == "Leaflitter P Conc"] <- max(year(leaflitter_p_concentration$Date))    
    treatDF$timepoint[treatDF$conc.terms == "Leaflitter P Conc"] <- length(unique(leaflitter_p_concentration$Date))  
    treatDF$notes[treatDF$conc.terms == "Leaflitter P Conc"] <- "Considered both senecsed leaf and leaf litter"

    
    ### Understorey P concentration
    out <- summaryBy(PercP~Ring,data=understorey_p_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Understorey P Conc", 2:7] <- out$PercP
    treatDF$year_start[treatDF$conc.terms == "Understorey P Conc"] <- min(year(understorey_p_concentration$Date))    
    treatDF$year_end[treatDF$conc.terms == "Understorey P Conc"] <- max(year(understorey_p_concentration$Date))    
    treatDF$timepoint[treatDF$conc.terms == "Understorey P Conc"] <- length(unique(understorey_p_concentration$Date))  
    treatDF$notes[treatDF$conc.terms == "Understorey P Conc"] <- "Assumed Cymbopogon and Microlaena contributed equally"

    ### Frass P concentration
    out <- summaryBy(PercP~Ring,data=frass_p_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Frass P Conc", 2:7] <- out$PercP
    treatDF$year_start[treatDF$conc.terms == "Frass P Conc"] <- min(year(frass_p_concentration$Date))    
    treatDF$year_end[treatDF$conc.terms == "Frass P Conc"] <- max(year(frass_p_concentration$Date))    
    treatDF$timepoint[treatDF$conc.terms == "Frass P Conc"] <- length(unique(frass_p_concentration$Date))  
    treatDF$notes[treatDF$conc.terms == "Frass P Conc"] <- "Direct measurement"
    
    ### Microbial P concentration
    out <- summaryBy(PercP~Ring,data=microbial_p_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Microbial P Conc", 2:7] <- out$PercP
    treatDF$year_start[treatDF$conc.terms == "Microbial P Conc"] <- min(year(microbial_p_concentration$Date))    
    treatDF$year_end[treatDF$conc.terms == "Microbial P Conc"] <- max(year(microbial_p_concentration$Date))    
    treatDF$timepoint[treatDF$conc.terms == "Microbial P Conc"] <- length(unique(microbial_p_concentration$Date))  
    treatDF$notes[treatDF$conc.terms == "Microbial P Conc"] <- "Averaged across depths"
    
    ### Soil P concentration
    out <- summaryBy(PercP~Ring,data=soil_p_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Soil P Conc", 2:7] <- out$PercP
    treatDF$year_start[treatDF$conc.terms == "Soil P Conc"] <- min(year(soil_p_concentration$Date))    
    treatDF$year_end[treatDF$conc.terms == "Soil P Conc"] <- max(year(soil_p_concentration$Date))    
    treatDF$timepoint[treatDF$conc.terms == "Soil P Conc"] <- length(unique(soil_p_concentration$Date))  
    treatDF$notes[treatDF$conc.terms == "Soil P Conc"] <- "Averaged across all P forms"
    
    ### Soil Phosphate P concentration
    out <- summaryBy(PercP~Ring,data=soil_phosphate_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Soil Phosphate P Conc", 2:7] <- out$PercP
    treatDF$year_start[treatDF$conc.terms == "Soil Phosphate P Conc"] <- min(year(soil_phosphate_concentration$Date))    
    treatDF$year_end[treatDF$conc.terms == "Soil Phosphate P Conc"] <- max(year(soil_phosphate_concentration$Date))    
    treatDF$timepoint[treatDF$conc.terms == "Soil Phosphate P Conc"] <- length(unique(soil_phosphate_concentration$Date))  
    treatDF$notes[treatDF$conc.terms == "Soil Phosphate P Conc"] <- "Averaged across all P forms"
    
    ### Mycorrhizal P concentration
    treatDF$notes[treatDF$conc.terms == "Mycorrhizal P Conc"] <- "Data not yet available"
    
    ### calculate treatment averages
    treatDF$aCO2 <- round(rowMeans(subset(treatDF, select=c(R2, R3, R6)), na.rm=T), 5)
    treatDF$eCO2 <- round(rowMeans(subset(treatDF, select=c(R1, R4, R5)), na.rm=T), 5)
    
    
    ##### output tables
    return(treatDF)
      
}

