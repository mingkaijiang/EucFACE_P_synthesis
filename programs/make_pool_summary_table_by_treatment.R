
#### To make EucFACE P summary table by CO2 treatment
#### Ignore time but produce time coverage information
#### This is for pools

make_pool_summary_table_by_treatment <- function() {
    
    ### Define pool variable names
    terms <- c("Wood P Pool", "Canopy P Pool", "Fine Root P Pool",
                "Understorey P Pool", "Microbial P Pool", "Soil P Pool", 
                "Mycorrhizal P Pool")
    
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
    
    ### Canopy P 
    out <- summaryBy(leaf_p_pool~Ring,data=canopy_p_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Canopy P Pool", 2:7] <- out$leaf_p_pool
    treatDF$year_start[treatDF$terms == "Canopy P Pool"] <- min(year(canopy_p_pool$Date))    
    treatDF$year_end[treatDF$terms == "Canopy P Pool"] <- max(year(canopy_p_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Canopy P Pool"] <- length(unique(canopy_p_pool$Date))  
    treatDF$notes[treatDF$terms == "Canopy P Pool"] <- "used monthly concentration values to extrapolate"

    
    ### Wood P 
    # out <- summaryBy(wood_p_pool~Ring,data=wood_p_pool,FUN=mean,keep.names=T,na.rm=T)
    # treatDF[treatDF$terms == "Wood P Pool", 2:7] <- out$wood_p_pool
    # treatDF$year_start[treatDF$terms == "Wood P Pool"] <- min(year(wood_p_pool$Date))    
    # treatDF$year_end[treatDF$terms == "Wood P Pool"] <- max(year(wood_p_pool$Date))    
    # treatDF$timepoint[treatDF$terms == "Wood P Pool"] <- length(unique(wood_p_pool$Date)) 
    treatDF$notes[treatDF$terms == "Wood P Pool"] <- "Data incomplete"
    
    
    ### Fine root P pool
    out <- summaryBy(fineroot_p_pool~Ring,data=fineroot_p_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Fine Root P Pool", 2:7] <- out$fineroot_p_pool
    treatDF$year_start[treatDF$terms == "Fine Root P Pool"] <- min(year(fineroot_p_pool$Date))    
    treatDF$year_end[treatDF$terms == "Fine Root P Pool"] <- max(year(fineroot_p_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Fine Root P Pool"] <- length(unique(fineroot_p_pool$Date))  
    treatDF$notes[treatDF$terms == "Fine Root P Pool"] <- "Top 30 cm"
    
    
    ### Understorey P pool
    # out <- summaryBy(PercP~Ring,data=understorey_p_pool,FUN=mean,keep.names=T,na.rm=T)
    # treatDF[treatDF$terms == "Understorey P Pool", 2:7] <- out$PercP
    # treatDF$year_start[treatDF$terms == "Understorey P Pool"] <- min(year(understorey_p_pool$Date))    
    # treatDF$year_end[treatDF$terms == "Understorey P Pool"] <- max(year(understorey_p_pool$Date))    
    # treatDF$timepoint[treatDF$terms == "Understorey P Pool"] <- length(unique(understorey_p_pool$Date))  
    treatDF$notes[treatDF$terms == "Understorey P Pool"] <- "Data incomplete"

    
    ### Microbial P pool
    out <- summaryBy(microbial_p_g_m2~Ring,data=microbial_p_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Microbial P Pool", 2:7] <- out$microbial_p_g_m2
    treatDF$year_start[treatDF$terms == "Microbial P Pool"] <- min(year(microbial_p_pool$Date))    
    treatDF$year_end[treatDF$terms == "Microbial P Pool"] <- max(year(microbial_p_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Microbial P Pool"] <- length(unique(microbial_p_pool$Date))  
    treatDF$notes[treatDF$terms == "Microbial P Pool"] <- "Top 10 cm"
    
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
    
    
    ##### output tables
    return(treatDF)
      
}

