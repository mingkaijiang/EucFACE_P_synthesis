
#### To make EucFACE P summary table by CO2 treatment
#### Ignore time but produce time coverage information
#### This is for pools

make_pool_summary_table <- function() {
    
    ### Define pool variable names
    terms <- c("Wood P Pool", "Canopy P Pool", "Canopy Litter P Pool",
               "Fine Root P Pool",
               "Coarse Root P Pool", "Understorey P Pool", 
               "Understorey Litter P Pool",
               "Microbial P Pool", "Soil Phosphate P Pool",
               "Soil P Pool", "Mycorrhizal P Pool",
               "Exchangeable Pi Pool", "Exchangeable Po Pool",
               "Moderately labile Po Pool", "Secondary Fe bound Pi Pool", "Primary Ca bound Pi Pool",
               "Occluded P Pool")
    
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

    ### Canopy Litter P 
    out <- summaryBy(leaflitter_p_pool~Ring,data=leaflitter_p_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Canopy Litter P Pool", 2:7] <- out$leaflitter_p_pool
    treatDF$year_start[treatDF$terms == "Canopy Litter P Pool"] <- min(year(leaflitter_p_pool$Date))    
    treatDF$year_end[treatDF$terms == "Canopy Litter P Pool"] <- max(year(leaflitter_p_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Canopy Litter P Pool"] <- length(unique(leaflitter_p_pool$Date))  
    treatDF$notes[treatDF$terms == "Canopy Litter P Pool"] <- "calculated based on leaflitter p concentration and leaflitter pool"
    
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
    out <- summaryBy(coarse_root_p_pool~Ring,data=coarse_root_p_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Coarse Root P Pool", 2:7] <- out$coarse_root_p_pool
    treatDF$year_start[treatDF$terms == "Coarse Root P Pool"] <- min(year(coarse_root_p_pool$Date))    
    treatDF$year_end[treatDF$terms == "Coarse Root P Pool"] <- max(year(coarse_root_p_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Coarse Root P Pool"] <- length(unique(coarse_root_p_pool$Date))  
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
    
    ### Exchangeable Pi Pool
    out <- summaryBy(F1_2_Pi_Exchangeable~Ring,data=soil_p_pool_hedley,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Exchangeable Pi Pool", 2:7] <- out$F1_2_Pi_Exchangeable
    treatDF$year_start[treatDF$terms == "Exchangeable Pi Pool"] <- min(soil_hedley_p_concentration$Year)    
    treatDF$year_end[treatDF$terms == "Exchangeable Pi Pool"] <- max(soil_hedley_p_concentration$Year)   
    treatDF$timepoint[treatDF$terms == "Exchangeable Pi Pool"] <- length(unique(soil_hedley_p_concentration$Year))  
    treatDF$notes[treatDF$terms == "Exchangeable Pi Pool"] <- "unclear depth info"
    
    ### Exchangeable Po Pool
    out <- summaryBy(F1_2_Po_Exchangeable~Ring,data=soil_p_pool_hedley,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Exchangeable Po Pool", 2:7] <- out$F1_2_Po_Exchangeable
    treatDF$year_start[treatDF$terms == "Exchangeable Po Pool"] <- min(soil_hedley_p_concentration$Year)    
    treatDF$year_end[treatDF$terms == "Exchangeable Po Pool"] <- max(soil_hedley_p_concentration$Year)   
    treatDF$timepoint[treatDF$terms == "Exchangeable Po Pool"] <- length(unique(soil_hedley_p_concentration$Year))  
    treatDF$notes[treatDF$terms == "Exchangeable Po Pool"] <- "unclear depth info"
    
    ### Moderately labile Po Pool
    out <- summaryBy(F3_Po_Moderately_labile~Ring,data=soil_p_pool_hedley,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Moderately labile Po Pool", 2:7] <- out$F3_Po_Moderately_labile
    treatDF$year_start[treatDF$terms == "Moderately labile Po Pool"] <- min(soil_hedley_p_concentration$Year)    
    treatDF$year_end[treatDF$terms == "Moderately labile Po Pool"] <- max(soil_hedley_p_concentration$Year)   
    treatDF$timepoint[treatDF$terms == "Moderately labile Po Pool"] <- length(unique(soil_hedley_p_concentration$Year))  
    treatDF$notes[treatDF$terms == "Moderately labile Po Pool"] <- "unclear depth info"
    

    ### Secondary Fe bound Pi Pool
    out <- summaryBy(F3_Fe_bound_P_Secondary_mineral~Ring,data=soil_p_pool_hedley,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Secondary Fe bound Pi Pool", 2:7] <- out$F3_Fe_bound_P_Secondary_mineral
    treatDF$year_start[treatDF$terms == "Secondary Fe bound Pi Pool"] <- min(soil_hedley_p_concentration$Year)    
    treatDF$year_end[treatDF$terms == "Secondary Fe bound Pi Pool"] <- max(soil_hedley_p_concentration$Year)   
    treatDF$timepoint[treatDF$terms == "Secondary Fe bound Pi Pool"] <- length(unique(soil_hedley_p_concentration$Year))  
    treatDF$notes[treatDF$terms == "Secondary Fe bound Pi Pool"] <- "unclear depth info"
    
    ### Primary Ca bound Pi Pool
    out <- summaryBy(F4_Ca_bound_Primary_Mineral~Ring,data=soil_p_pool_hedley,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Primary Ca bound Pi Pool", 2:7] <- out$F4_Ca_bound_Primary_Mineral
    treatDF$year_start[treatDF$terms == "Primary Ca bound Pi Pool"] <- min(soil_hedley_p_concentration$Year)    
    treatDF$year_end[treatDF$terms == "Primary Ca bound Pi Pool"] <- max(soil_hedley_p_concentration$Year)   
    treatDF$timepoint[treatDF$terms == "Primary Ca bound Pi Pool"] <- length(unique(soil_hedley_p_concentration$Year))  
    treatDF$notes[treatDF$terms == "Primary Ca bound Pi Pool"] <- "unclear depth info"
    
    ### Occluded P Pool
    out <- summaryBy(F5_6_Occluded~Ring,data=soil_p_pool_hedley,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Occluded P Pool", 2:7] <- out$F5_6_Occluded
    treatDF$year_start[treatDF$terms == "Occluded P Pool"] <- min(soil_hedley_p_concentration$Year)    
    treatDF$year_end[treatDF$terms == "Occluded P Pool"] <- max(soil_hedley_p_concentration$Year)   
    treatDF$timepoint[treatDF$terms == "Occluded P Pool"] <- length(unique(soil_hedley_p_concentration$Year))  
    treatDF$notes[treatDF$terms == "Occluded P Pool"] <- "unclear depth info"
    
    
    ### Mycorrhizal P pool
    treatDF$notes[treatDF$terms == "Mycorrhizal P Pool"] <- "Data not yet available"
    
    ### calculate treatment averages
    treatDF$aCO2 <- round(rowMeans(subset(treatDF, select=c(R2, R3, R6)), na.rm=T), 5)
    treatDF$eCO2 <- round(rowMeans(subset(treatDF, select=c(R1, R4, R5)), na.rm=T), 5)
    
    treatDF$aCO2_sd <- rowSds(as.matrix(subset(treatDF, select=c(R2, R3, R6))), na.rm=T)
    treatDF$eCO2_sd <- rowSds(as.matrix(subset(treatDF, select=c(R1, R4, R5))), na.rm=T)
    
    ###### Diff (eCO2 - aCO2)
    treatDF$diff <- round(treatDF$eCO2 - treatDF$aCO2, 4)
    
    ###### percent differences (eCO2 - aCO2) / aCO2 * 100
    treatDF$percent_diff <- round((treatDF$eCO2 - treatDF$aCO2) / (treatDF$aCO2) * 100, 2)
    
    write.csv(treatDF, "plots_tables/summary_table_P_pool_unnormalized.csv", row.names=F)
    
    
    ##### output tables
    return(treatDF)
      
}

