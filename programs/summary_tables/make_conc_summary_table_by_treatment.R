
#### To make EucFACE P summary table by CO2 treatment
#### Ignore time but produce time coverage information

make_conc_summary_table_by_treatment <- function() {
    
    ### Define concentration variable names
    conc.terms <- c("Wood P Conc", "Canopy P Conc", "Fine Root P Conc", "Coarse Root P Conc",
                    "Leaflitter P Conc","Understorey P Conc", "Understorey Litter P Conc", "Frass P Conc",
                    "Microbial P Conc", "Soil P Conc", "Soil Phosphate P Conc",
                    "Mycorrhizal P Conc", "Exhanagable Pi Conc", "Exhanagable Po Conc",
                    "Moderately labile Po Conc", "Secondary Fe bound Pi Conc", "Primary Ca bound Pi Conc",
                    "Occluded P Conc")
    
    treatDF <- data.frame(conc.terms)
    treatDF$R1 <- rep(NA, length(treatDF$conc.terms))
    treatDF$R2 <- rep(NA, length(treatDF$conc.terms))
    treatDF$R3 <- rep(NA, length(treatDF$conc.terms))
    treatDF$R4 <- rep(NA, length(treatDF$conc.terms))
    treatDF$R5 <- rep(NA, length(treatDF$conc.terms))
    treatDF$R6 <- rep(NA, length(treatDF$conc.terms))
    
    treatDF$aCO2 <- rep(NA, length(treatDF$conc.terms))
    treatDF$eCO2 <- rep(NA, length(treatDF$conc.terms))
    treatDF$diff <- rep(NA, length(treatDF$conc.terms))
    treatDF$percent_diff <- rep(NA, length(treatDF$conc.terms))
    
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
    
    ### Coarse root P concentration
    out <- summaryBy(PercP~Ring,data=wood_p_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Coarse Root P Conc", 2:7] <- out$PercP
    treatDF$year_start[treatDF$conc.terms == "Coarse Root P Conc"] <- min(year(wood_p_concentration$Date))    
    treatDF$year_end[treatDF$conc.terms == "Coarse Root P Conc"] <- max(year(wood_p_concentration$Date))    
    treatDF$timepoint[treatDF$conc.terms == "Coarse Root P Conc"] <- length(unique(wood_p_concentration$Date)) 
    treatDF$notes[treatDF$conc.terms == "Coarse Root P Conc"] <- "Used wood P concentration"
    
    
    ### Fine root P concentration
    out <- summaryBy(PercP~Ring,data=fineroot_p_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Fine Root P Conc", 2:7] <- out$PercP
    treatDF$year_start[treatDF$conc.terms == "Fine Root P Conc"] <- min(year(fineroot_p_concentration$Date))    
    treatDF$year_end[treatDF$conc.terms == "Fine Root P Conc"] <- max(year(fineroot_p_concentration$Date))    
    treatDF$timepoint[treatDF$conc.terms == "Fine Root P Conc"] <- length(unique(fineroot_p_concentration$Date))  
    treatDF$notes[treatDF$conc.terms == "Fine Root P Conc"] <- "Depth 0 - 30 cm"
    
    
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
    
    ### Understorey Litter P concentration
    out <- summaryBy(PercP~Ring,data=understorey_litter_p_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Understorey Litter P Conc", 2:7] <- out$PercP
    treatDF$year_start[treatDF$conc.terms == "Understorey Litter P Conc"] <- min(year(understorey_litter_p_concentration$Date))    
    treatDF$year_end[treatDF$conc.terms == "Understorey Litter P Conc"] <- max(year(understorey_litter_p_concentration$Date))    
    treatDF$timepoint[treatDF$conc.terms == "Understorey Litter P Conc"] <- length(unique(understorey_litter_p_concentration$Date))  
    treatDF$notes[treatDF$conc.terms == "Understorey Litter P Conc"] <- "Data from 2017"

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
    treatDF$notes[treatDF$conc.terms == "Microbial P Conc"] <- "Top 10 cm"
    
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
    treatDF$notes[treatDF$conc.terms == "Soil Phosphate P Conc"] <- "Top 10 cm"
    
    
    ### Exhanagable Pi Conc
    out <- summaryBy(F1_2_Pi_Exhanagable~Ring,data=soil_hedley_p_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Exhanagable Pi Conc", 2:7] <- out$F1_2_Pi_Exhanagable
    treatDF$year_start[treatDF$conc.terms == "Exhanagable Pi Conc"] <- min(soil_hedley_p_concentration$Year)    
    treatDF$year_end[treatDF$conc.terms == "Exhanagable Pi Conc"] <- max(soil_hedley_p_concentration$Year)   
    treatDF$timepoint[treatDF$conc.terms == "Exhanagable Pi Conc"] <- length(unique(soil_hedley_p_concentration$Year))  
    treatDF$notes[treatDF$conc.terms == "Exhanagable Pi Conc"] <- "unclear depth info"
    
    ### Exhanagable Po Conc
    out <- summaryBy(F1_2_Po_Exhanagable~Ring,data=soil_hedley_p_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Exhanagable Po Conc", 2:7] <- out$F1_2_Po_Exhanagable
    treatDF$year_start[treatDF$conc.terms == "Exhanagable Po Conc"] <- min(soil_hedley_p_concentration$Year)    
    treatDF$year_end[treatDF$conc.terms == "Exhanagable Po Conc"] <- max(soil_hedley_p_concentration$Year)   
    treatDF$timepoint[treatDF$conc.terms == "Exhanagable Po Conc"] <- length(unique(soil_hedley_p_concentration$Year))  
    treatDF$notes[treatDF$conc.terms == "Exhanagable Po Conc"] <- "unclear depth info"
    
    ### Moderately labile Po Conc
    out <- summaryBy(F3_Po_Moderately_labile~Ring,data=soil_hedley_p_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Moderately labile Po Conc", 2:7] <- out$F3_Po_Moderately_labile
    treatDF$year_start[treatDF$conc.terms == "Moderately labile Po Conc"] <- min(soil_hedley_p_concentration$Year)    
    treatDF$year_end[treatDF$conc.terms == "Moderately labile Po Conc"] <- max(soil_hedley_p_concentration$Year)   
    treatDF$timepoint[treatDF$conc.terms == "Moderately labile Po Conc"] <- length(unique(soil_hedley_p_concentration$Year))  
    treatDF$notes[treatDF$conc.terms == "Moderately labile Po Conc"] <- "unclear depth info"
    
    ### Secondary Fe bound Pi Conc
    out <- summaryBy(F3_Fe_bound_P_Secondary_mineral~Ring,data=soil_hedley_p_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Secondary Fe bound Pi Conc", 2:7] <- out$F3_Fe_bound_P_Secondary_mineral
    treatDF$year_start[treatDF$conc.terms == "Secondary Fe bound Pi Conc"] <- min(soil_hedley_p_concentration$Year)    
    treatDF$year_end[treatDF$conc.terms == "Secondary Fe bound Pi Conc"] <- max(soil_hedley_p_concentration$Year)   
    treatDF$timepoint[treatDF$conc.terms == "Secondary Fe bound Pi Conc"] <- length(unique(soil_hedley_p_concentration$Year))  
    treatDF$notes[treatDF$conc.terms == "Secondary Fe bound Pi Conc"] <- "unclear depth info"
    
    ### Primary Ca bound Pi Conc
    out <- summaryBy(F4_Ca_bound_Primary_Mineral~Ring,data=soil_hedley_p_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Primary Ca bound Pi Conc", 2:7] <- out$F4_Ca_bound_Primary_Mineral
    treatDF$year_start[treatDF$conc.terms == "Primary Ca bound Pi Conc"] <- min(soil_hedley_p_concentration$Year)    
    treatDF$year_end[treatDF$conc.terms == "Primary Ca bound Pi Conc"] <- max(soil_hedley_p_concentration$Year)   
    treatDF$timepoint[treatDF$conc.terms == "Primary Ca bound Pi Conc"] <- length(unique(soil_hedley_p_concentration$Year))  
    treatDF$notes[treatDF$conc.terms == "Primary Ca bound Pi Conc"] <- "unclear depth info"
    
    ### Occluded P Conc
    out <- summaryBy(F5_6_Occluded~Ring,data=soil_hedley_p_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Occluded P Conc", 2:7] <- out$F5_6_Occluded
    treatDF$year_start[treatDF$conc.terms == "Occluded P Conc"] <- min(soil_hedley_p_concentration$Year)    
    treatDF$year_end[treatDF$conc.terms == "Occluded P Conc"] <- max(soil_hedley_p_concentration$Year)   
    treatDF$timepoint[treatDF$conc.terms == "Occluded P Conc"] <- length(unique(soil_hedley_p_concentration$Year))  
    treatDF$notes[treatDF$conc.terms == "Occluded P Conc"] <- "unclear depth info"
    
    
    ### Mycorrhizal P concentration
    treatDF$notes[treatDF$conc.terms == "Mycorrhizal P Conc"] <- "Data not yet available"
    
    ### calculate treatment averages
    treatDF$aCO2 <- round(rowMeans(subset(treatDF, select=c(R2, R3, R6)), na.rm=T), 5)
    treatDF$eCO2 <- round(rowMeans(subset(treatDF, select=c(R1, R4, R5)), na.rm=T), 5)
    
    ###### Diff (eCO2 - aCO2)
    treatDF$diff <- round(treatDF$eCO2 - treatDF$aCO2, 4)
    
    ###### percent differences (eCO2 - aCO2) / aCO2 * 100
    treatDF$percent_diff <- round((treatDF$eCO2 - treatDF$aCO2) / (treatDF$aCO2) * 100, 2)
    
    
    ##### output tables
    return(treatDF)
      
}

