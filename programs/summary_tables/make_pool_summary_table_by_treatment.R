
#### To make EucFACE P summary table by CO2 treatment
#### Ignore time but produce time coverage information
#### This is for pools

make_pool_summary_table_by_treatment <- function() {
    
    ### Define pool variable names
    terms <- c("Wood P Pool", "Canopy P Pool", "Fine Root P Pool",
               "Coarse Root P Pool", "Understorey P Pool", 
               "Understorey Litter P Pool",
               "Microbial P Pool", "Soil Phosphate P Pool",
               "Soil P Pool", "Mycorrhizal P Pool")
    
    treatDF <- data.frame(terms)
    treatDF$R1 <- rep(NA, length(treatDF$terms))
    treatDF$R2 <- rep(NA, length(treatDF$terms))
    treatDF$R3 <- rep(NA, length(treatDF$terms))
    treatDF$R4 <- rep(NA, length(treatDF$terms))
    treatDF$R5 <- rep(NA, length(treatDF$terms))
    treatDF$R6 <- rep(NA, length(treatDF$terms))
    
    treatDF$aCO2 <- rep(NA, length(treatDF$terms))
    treatDF$eCO2 <- rep(NA, length(treatDF$terms))
    treatDF$diff <- rep(NA, length(treatDF$terms))
    treatDF$percent_diff <- rep(NA, length(treatDF$terms))
    
    treatDF$year_start <- rep(NA, length(treatDF$terms))
    treatDF$year_end <- rep(NA, length(treatDF$terms))
    treatDF$timepoint <- rep(NA, length(treatDF$terms))
    treatDF$notes <- rep(NA, length(treatDF$terms))
    
    ### Canopy P 
    out <- summaryBy(leaf_p_pool~Ring,data=canopy_p_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Canopy P Pool", 2:7] <- out$leaf_p_pool
    treatDF$year_start[treatDF$terms == "Canopy P Pool"] <- min(year(canopy_p_pool$Date))    
    treatDF$year_end[treatDF$terms == "Canopy P Pool"] <- max(year(canopy_p_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Canopy P Pool"] <- length(unique(canopy_p_pool$Date))  
    treatDF$notes[treatDF$terms == "Canopy P Pool"] <- "used monthly concentration values to extrapolate"

    
    ### Wood P 
    out <- summaryBy(wood_p_pool~Ring,data=wood_p_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Wood P Pool", 2:7] <- out$wood_p_pool
    treatDF$year_start[treatDF$terms == "Wood P Pool"] <- min(year(wood_p_pool$Date))    
    treatDF$year_end[treatDF$terms == "Wood P Pool"] <- max(year(wood_p_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Wood P Pool"] <- length(unique(wood_p_pool$Date)) 
    treatDF$notes[treatDF$terms == "Wood P Pool"] <- "Based on single time point concentration measurement"
    
    
    ### Fine root P pool
    out <- summaryBy(fineroot_p_pool~Ring,data=fineroot_p_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Fine Root P Pool", 2:7] <- out$fineroot_p_pool
    treatDF$year_start[treatDF$terms == "Fine Root P Pool"] <- min(year(fineroot_p_pool$Date))    
    treatDF$year_end[treatDF$terms == "Fine Root P Pool"] <- max(year(fineroot_p_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Fine Root P Pool"] <- length(unique(fineroot_p_pool$Date))  
    treatDF$notes[treatDF$terms == "Fine Root P Pool"] <- "Top 30 cm"
    
    ### Coarse root P pool
    out <- summaryBy(coarse_root_p_pool~Ring,data=coarse_root_p_pool_1,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Coarse Root P Pool", 2:7] <- out$coarse_root_p_pool
    treatDF$year_start[treatDF$terms == "Coarse Root P Pool"] <- min(year(coarse_root_p_pool_1$Date))    
    treatDF$year_end[treatDF$terms == "Coarse Root P Pool"] <- max(year(coarse_root_p_pool_1$Date))    
    treatDF$timepoint[treatDF$terms == "Coarse Root P Pool"] <- length(unique(coarse_root_p_pool_1$Date))  
    treatDF$notes[treatDF$terms == "Coarse Root P Pool"] <- "Allometric rlt with DBH"
    
    ### Understorey P pool
    out <- summaryBy(live_p_pool~Ring,data=understorey_p_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Understorey P Pool", 2:7] <- out$live_p_pool
    treatDF$year_start[treatDF$terms == "Understorey P Pool"] <- min(year(understorey_p_pool$Date))    
    treatDF$year_end[treatDF$terms == "Understorey P Pool"] <- max(year(understorey_p_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Understorey P Pool"] <- length(unique(understorey_p_pool$Date))  
    treatDF$notes[treatDF$terms == "Understorey P Pool"] <- "Used harvest estimate of C pool"
    
    ### Understorey Litter P pool
    out <- summaryBy(dead_p_pool~Ring,data=understorey_p_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Understorey Litter P Pool", 2:7] <- out$dead_p_pool
    treatDF$year_start[treatDF$terms == "Understorey Litter P Pool"] <- min(year(understorey_p_pool$Date))    
    treatDF$year_end[treatDF$terms == "Understorey Litter P Pool"] <- max(year(understorey_p_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Understorey Litter P Pool"] <- length(unique(understorey_p_pool$Date))  
    treatDF$notes[treatDF$terms == "Understorey Litter P Pool"] <- "Used harvest estimate of C pool"

    
    ### Microbial P pool
    out <- summaryBy(microbial_p_g_m2~Ring,data=microbial_p_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Microbial P Pool", 2:7] <- out$microbial_p_g_m2
    treatDF$year_start[treatDF$terms == "Microbial P Pool"] <- min(year(microbial_p_pool$Date))    
    treatDF$year_end[treatDF$terms == "Microbial P Pool"] <- max(year(microbial_p_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Microbial P Pool"] <- length(unique(microbial_p_pool$Date))  
    treatDF$notes[treatDF$terms == "Microbial P Pool"] <- "Top 10 cm"
    
    ### Soil Phosphate P pool
    out <- summaryBy(soil_phosphate_p_g_m2~Ring,data=soil_phosphate_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Soil Phosphate P Pool", 2:7] <- out$soil_phosphate_p_g_m2
    treatDF$year_start[treatDF$terms == "Soil Phosphate P Pool"] <- min(year(soil_phosphate_pool$Date))    
    treatDF$year_end[treatDF$terms == "Soil Phosphate P Pool"] <- max(year(soil_phosphate_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Soil Phosphate P Pool"] <- length(unique(soil_phosphate_pool$Date))  
    treatDF$notes[treatDF$terms == "Soil Phosphate P Pool"] <- "Top 10 cm"
    
    ### Soil P pool
    out <- summaryBy(soil_p_g_m2~Ring,data=soil_p_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Soil P Pool", 2:7] <- out$soil_p_g_m2
    treatDF$year_start[treatDF$terms == "Soil P Pool"] <- min(year(soil_p_pool$Date))    
    treatDF$year_end[treatDF$terms == "Soil P Pool"] <- max(year(soil_p_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Soil P Pool"] <- length(unique(soil_p_pool$Date))  
    treatDF$notes[treatDF$terms == "Soil P Pool"] <- "Averaged across all P forms"
    

    ### Mycorrhizal P pool
    treatDF$notes[treatDF$terms == "Mycorrhizal P Pool"] <- "Data not yet available"
    
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

