
make_pool_summary_table <- function(norm) {
    
  #### To make EucFACE P summary table by CO2 treatment
  #### Ignore time but produce time coverage information
  #### This is for pools
  
    ### Define pool variable names
    terms <- c("Canopy P Pool", 
               "Total Wood P Pool", 
               "Sapwood P Pool",
               "Heartwood P Pool",
               "Forestfloor Leaf Litter P Pool",
               "Fine Root P Pool",
               "Coarse Root P Pool", 
               "Understorey P Pool", 
               "Understorey Litter P Pool",
               "Standing Dead Wood P Pool",
               "Microbial P Pool 0-10cm", 
               "Microbial P Pool 10-30cm", 
               "Microbial P Pool 30-60cm", 
               #"Mycorrhizal P Pool 0-10cm", 
               #"Mycorrhizal P Pool 10-30cm", 
               #"Mycorrhizal P Pool 30-60cm", 
               "Soil Phosphate P Pool 0-10cm",
               "Soil Phosphate P Pool 10-30cm",
               "Soil Phosphate P Pool 30-60cm",
               "Soil P Pool 0-10cm",
               "Soil P Pool 10-30cm", 
               "Soil P Pool 30-60cm", 
               "Soil Inorg P Pool 0-10cm",
               "Soil Inorg P Pool 10-30cm", 
               "Soil Inorg P Pool 30-60cm", 
               "Soil Org P Pool 0-10cm",
               "Soil Org P Pool 10-30cm", 
               "Soil Org P Pool 30-60cm", 
               "Exchangeable Pi Pool", 
               "Exchangeable Po Pool",
               "Moderately labile Po Pool", 
               "Secondary Fe bound Pi Pool", 
               "Primary Ca bound Pi Pool",
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

    ### Forestfloor Leaf litter
    out <- summaryBy(leaflitter_p_pool~Ring,data=leaflitter_p_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Forestfloor Leaf Litter P Pool", 2:7] <- out$leaflitter_p_pool
    treatDF$year_start[treatDF$terms == "Forestfloor Leaf Litter P Pool"] <- min(year(leaflitter_p_pool$Date))    
    treatDF$year_end[treatDF$terms == "Forestfloor Leaf Litter P Pool"] <- max(year(leaflitter_p_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Forestfloor Leaf Litter P Pool"] <- length(unique(leaflitter_p_pool$Date))  
    treatDF$notes[treatDF$terms == "Forestfloor Leaf Litter P Pool"] <- "calculated based on leaflitter p concentration and leaflitter pool"
    
    ### Wood P 
    out <- summaryBy(wood_p_pool~Ring,data=wood_p_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Total Wood P Pool", 2:7] <- out$wood_p_pool
    treatDF$year_start[treatDF$terms == "Total Wood P Pool"] <- min(year(wood_p_pool$Date))    
    treatDF$year_end[treatDF$terms == "Total Wood P Pool"] <- max(year(wood_p_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Total Wood P Pool"] <- length(unique(wood_p_pool$Date)) 
    treatDF$notes[treatDF$terms == "Total Wood P Pool"] <- "Based on single time point concentration measurement"
    
    ### Sapwood P 
    out <- summaryBy(wood_p_pool~Ring,data=sapwood_p_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Sapwood P Pool", 2:7] <- out$wood_p_pool
    treatDF$year_start[treatDF$terms == "Sapwood P Pool"] <- min(year(wood_p_pool$Date))    
    treatDF$year_end[treatDF$terms == "Sapwood P Pool"] <- max(year(wood_p_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Sapwood P Pool"] <- length(unique(wood_p_pool$Date)) 
    treatDF$notes[treatDF$terms == "Sapwood P Pool"] <- "Based on single time point concentration measurement"
    
    ### Heartwood P 
    out <- summaryBy(wood_p_pool~Ring,data=heartwood_p_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Heartwood P Pool", 2:7] <- out$wood_p_pool
    treatDF$year_start[treatDF$terms == "Heartwood P Pool"] <- min(year(wood_p_pool$Date))    
    treatDF$year_end[treatDF$terms == "Heartwood P Pool"] <- max(year(wood_p_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Heartwood P Pool"] <- length(unique(wood_p_pool$Date)) 
    treatDF$notes[treatDF$terms == "Heartwood P Pool"] <- "Based on single time point concentration measurement"
    
    ### Standing dead
    out <- summaryBy(wood_p_pool~Ring,data=standing_dead_p_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Standing Dead Wood P Pool", 2:7] <- out$wood_p_pool
    treatDF$year_start[treatDF$terms == "Standing Dead Wood P Pool"] <- min(year(wood_p_pool$Date))    
    treatDF$year_end[treatDF$terms == "Standing Dead Wood P Pool"] <- max(year(wood_p_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Standing Dead Wood P Pool"] <- length(unique(wood_p_pool$Date)) 
    treatDF$notes[treatDF$terms == "Standing Dead Wood P Pool"] <- "Based on single time point concentration measurement"
    
    
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
    out <- summaryBy(microbial_p_g_m2~Ring,data=microbial_p_pool[microbial_p_pool$Depth=="0_10",],FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Microbial P Pool 0-10cm", 2:7] <- out$microbial_p_g_m2
    treatDF$year_start[treatDF$terms == "Microbial P Pool 0-10cm"] <- min(year(microbial_p_pool$Date[microbial_p_pool$Depth=="0_10"]))    
    treatDF$year_end[treatDF$terms == "Microbial P Pool 0-10cm"] <- max(year(microbial_p_pool$Date[microbial_p_pool$Depth=="0_10"]))    
    treatDF$timepoint[treatDF$terms == "Microbial P Pool 0-10cm"] <- length(unique(microbial_p_pool$Date[microbial_p_pool$Depth=="0_10"]))  
    treatDF$notes[treatDF$terms == "Microbial P Pool 0-10cm"] <- "Top 10 cm"
    
    ### Microbial P pool
    out <- summaryBy(microbial_p_g_m2~Ring,data=microbial_p_pool[microbial_p_pool$Depth=="10_30",],FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Microbial P Pool 10-30cm", 2:7] <- out$microbial_p_g_m2
    treatDF$year_start[treatDF$terms == "Microbial P Pool 10-30cm"] <- min(year(microbial_p_pool$Date[microbial_p_pool$Depth=="10_30"]))    
    treatDF$year_end[treatDF$terms == "Microbial P Pool 10-30cm"] <- max(year(microbial_p_pool$Date[microbial_p_pool$Depth=="10_30"]))    
    treatDF$timepoint[treatDF$terms == "Microbial P Pool 10-30cm"] <- length(unique(microbial_p_pool$Date[microbial_p_pool$Depth=="10_30"]))  
    treatDF$notes[treatDF$terms == "Microbial P Pool 10-30cm"] <- "Top 10-30 cm"
    
    ### Microbial P pool
    out <- summaryBy(microbial_p_g_m2~Ring,data=microbial_p_pool[microbial_p_pool$Depth=="transition",],FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Microbial P Pool 30-60cm", 2:7] <- out$microbial_p_g_m2
    treatDF$year_start[treatDF$terms == "Microbial P Pool 30-60cm"] <- min(year(microbial_p_pool$Date[microbial_p_pool$Depth=="transition"]))    
    treatDF$year_end[treatDF$terms == "Microbial P Pool 30-60cm"] <- max(year(microbial_p_pool$Date[microbial_p_pool$Depth=="transition"]))    
    treatDF$timepoint[treatDF$terms == "Microbial P Pool 30-60cm"] <- length(unique(microbial_p_pool$Date[microbial_p_pool$Depth=="transition"]))  
    treatDF$notes[treatDF$terms == "Microbial P Pool 30-60cm"] <- "Top 10 cm"
    
    ### Soil Phosphate P pool
    out <- summaryBy(soil_phosphate_p_g_m2~Ring,data=soil_phosphate_pool[soil_phosphate_pool$Depth=="0_10",],FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Soil Phosphate P Pool 0-10cm", 2:7] <- out$soil_phosphate_p_g_m2
    treatDF$year_start[treatDF$terms == "Soil Phosphate P Pool 0-10cm"] <- min(year(soil_phosphate_pool$Date[soil_phosphate_pool$Depth=="0_10"]))    
    treatDF$year_end[treatDF$terms == "Soil Phosphate P Pool 0-10cm"] <- max(year(soil_phosphate_pool$Date[soil_phosphate_pool$Depth=="0_10"]))    
    treatDF$timepoint[treatDF$terms == "Soil Phosphate P Pool 0-10cm"] <- length(unique(soil_phosphate_pool$Date[soil_phosphate_pool$Depth=="0_10"]))  
    treatDF$notes[treatDF$terms == "Soil Phosphate P Pool 0-10cm"] <- "Top 10 cm"
    
    ### Soil Phosphate P pool
    out <- summaryBy(soil_phosphate_p_g_m2~Ring,data=soil_phosphate_pool[soil_phosphate_pool$Depth=="10_30",],FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Soil Phosphate P Pool 10-30cm", 2:7] <- out$soil_phosphate_p_g_m2
    treatDF$year_start[treatDF$terms == "Soil Phosphate P Pool 10-30cm"] <- min(year(soil_phosphate_pool$Date[soil_phosphate_pool$Depth=="10_30"]))    
    treatDF$year_end[treatDF$terms == "Soil Phosphate P Pool 10-30cm"] <- max(year(soil_phosphate_pool$Date[soil_phosphate_pool$Depth=="10_30"]))    
    treatDF$timepoint[treatDF$terms == "Soil Phosphate P Pool 10-30cm"] <- length(unique(soil_phosphate_pool$Date[soil_phosphate_pool$Depth=="10_30"]))  
    treatDF$notes[treatDF$terms == "Soil Phosphate P Pool 10-30cm"] <- "Top 10 cm"
    
    ### Soil Phosphate P pool
    out <- summaryBy(soil_phosphate_p_g_m2~Ring,data=soil_phosphate_pool[soil_phosphate_pool$Depth=="transition",],FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Soil Phosphate P Pool 30-60cm", 2:7] <- out$soil_phosphate_p_g_m2
    treatDF$year_start[treatDF$terms == "Soil Phosphate P Pool 30-60cm"] <- min(year(soil_phosphate_pool$Date[soil_phosphate_pool$Depth=="transition"]))    
    treatDF$year_end[treatDF$terms == "Soil Phosphate P Pool 30-60cm"] <- max(year(soil_phosphate_pool$Date[soil_phosphate_pool$Depth=="transition"]))    
    treatDF$timepoint[treatDF$terms == "Soil Phosphate P Pool 30-60cm"] <- length(unique(soil_phosphate_pool$Date[soil_phosphate_pool$Depth=="transition"]))  
    treatDF$notes[treatDF$terms == "Soil Phosphate P Pool 30-60cm"] <- "30-60"
    
    ### Soil P pool
    out <- summaryBy(soil_p_g_m2~Ring,data=soil_p_pool[soil_p_pool$Depth=="0_10",],FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Soil P Pool 0-10cm", 2:7] <- out$soil_p_g_m2
    treatDF$year_start[treatDF$terms == "Soil P Pool 0-10cm"] <- min(year(soil_p_pool$Date[soil_p_pool$Depth=="0_10"]))    
    treatDF$year_end[treatDF$terms == "Soil P Pool 0-10cm"] <- max(year(soil_p_pool$Date[soil_p_pool$Depth=="0_10"]))    
    treatDF$timepoint[treatDF$terms == "Soil P Pool 0-10cm"] <- length(unique(soil_p_pool$Date[soil_p_pool$Depth=="0_10"]))  
    treatDF$notes[treatDF$terms == "Soil P Pool 0-10cm"] <- "Averaged across all P forms"
    
    
    ### Soil P pool
    out <- summaryBy(soil_p_g_m2~Ring,data=soil_p_pool[soil_p_pool$Depth=="10_30",],FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Soil P Pool 10-30cm", 2:7] <- out$soil_p_g_m2
    treatDF$year_start[treatDF$terms == "Soil P Pool 10-30cm"] <- min(year(soil_p_pool$Date[soil_p_pool$Depth=="10_30"]))    
    treatDF$year_end[treatDF$terms == "Soil P Pool 10-30cm"] <- max(year(soil_p_pool$Date[soil_p_pool$Depth=="10_30"]))    
    treatDF$timepoint[treatDF$terms == "Soil P Pool 10-30cm"] <- length(unique(soil_p_pool$Date[soil_p_pool$Depth=="10_30"]))  
    treatDF$notes[treatDF$terms == "Soil P Pool 10-30cm"] <- "Averaged across all P forms"
    
    
    ### Soil P pool
    out <- summaryBy(soil_p_g_m2~Ring,data=soil_p_pool[soil_p_pool$Depth=="transition",],FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Soil P Pool 30-60cm", 2:7] <- out$soil_p_g_m2
    treatDF$year_start[treatDF$terms == "Soil P Pool 30-60cm"] <- min(year(soil_p_pool$Date[soil_p_pool$Depth=="transition"]))    
    treatDF$year_end[treatDF$terms == "Soil P Pool 30-60cm"] <- max(year(soil_p_pool$Date[soil_p_pool$Depth=="transition"]))    
    treatDF$timepoint[treatDF$terms == "Soil P Pool 30-60cm"] <- length(unique(soil_p_pool$Date[soil_p_pool$Depth=="transition"]))  
    treatDF$notes[treatDF$terms == "Soil P Pool 30-60cm"] <- "Averaged across all P forms"
    
    
    ### Soil Inorg P pool
    out <- summaryBy(soil_inorganic_p_g_m2~Ring,data=soil_inorganic_p_pool[soil_inorganic_p_pool$Depth=="0_10",],FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Soil Inorg P Pool 0-10cm", 2:7] <- out$soil_inorganic_p_g_m2
    treatDF$year_start[treatDF$terms == "Soil Inorg P Pool 0-10cm"] <- min(year(soil_inorganic_p_pool$Date[soil_inorganic_p_pool$Depth=="0_10"]))    
    treatDF$year_end[treatDF$terms == "Soil Inorg P Pool 0-10cm"] <- max(year(soil_inorganic_p_pool$Date[soil_inorganic_p_pool$Depth=="0_10"]))    
    treatDF$timepoint[treatDF$terms == "Soil Inorg P Pool 0-10cm"] <- length(unique(soil_inorganic_p_pool$Date[soil_inorganic_p_pool$Depth=="0_10"]))  
    treatDF$notes[treatDF$terms == "Soil Inorg P Pool 0-10cm"] <- "Averaged across all P forms"
    
    
    ### Soil Inorg P pool
    out <- summaryBy(soil_inorganic_p_g_m2~Ring,data=soil_inorganic_p_pool[soil_inorganic_p_pool$Depth=="10_30",],FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Soil Inorg P Pool 10-30cm", 2:7] <- out$soil_inorganic_p_g_m2
    treatDF$year_start[treatDF$terms == "Soil Inorg P Pool 10-30cm"] <- min(year(soil_inorganic_p_pool$Date[soil_inorganic_p_pool$Depth=="10_30"]))    
    treatDF$year_end[treatDF$terms == "Soil Inorg P Pool 10-30cm"] <- max(year(soil_inorganic_p_pool$Date[soil_inorganic_p_pool$Depth=="10_30"]))    
    treatDF$timepoint[treatDF$terms == "Soil Inorg P Pool 10-30cm"] <- length(unique(soil_inorganic_p_pool$Date[soil_inorganic_p_pool$Depth=="10_30"]))  
    treatDF$notes[treatDF$terms == "Soil Inorg P Pool 10-30cm"] <- "Averaged across all P forms"
    
    
    ### Soil inorg_P pool
    out <- summaryBy(soil_inorganic_p_g_m2~Ring,data=soil_inorganic_p_pool[soil_inorganic_p_pool$Depth=="transition",],FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Soil Inorg P Pool 30-60cm", 2:7] <- out$soil_inorganic_p_g_m2
    treatDF$year_start[treatDF$terms == "Soil Inorg P Pool 30-60cm"] <- min(year(soil_inorganic_p_pool$Date[soil_inorganic_p_pool$Depth=="transition"]))    
    treatDF$year_end[treatDF$terms == "Soil Inorg P Pool 30-60cm"] <- max(year(soil_inorganic_p_pool$Date[soil_inorganic_p_pool$Depth=="transition"]))    
    treatDF$timepoint[treatDF$terms == "Soil Inorg P Pool 30-60cm"] <- length(unique(soil_inorganic_p_pool$Date[soil_inorganic_p_pool$Depth=="transition"]))  
    treatDF$notes[treatDF$terms == "Soil Inorg P Pool 30-60cm"] <- "Averaged across all P forms"
    
    
    ### Soil Org P pool
    out <- summaryBy(soil_organic_p_g_m2~Ring,data=soil_organic_p_pool[soil_organic_p_pool$Depth=="0_10",],FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Soil Org P Pool 0-10cm", 2:7] <- out$soil_organic_p_g_m2
    treatDF$year_start[treatDF$terms == "Soil Org P Pool 0-10cm"] <- min(year(soil_organic_p_pool$Date[soil_organic_p_pool$Depth=="0_10"]))    
    treatDF$year_end[treatDF$terms == "Soil Org P Pool 0-10cm"] <- max(year(soil_organic_p_pool$Date[soil_organic_p_pool$Depth=="0_10"]))    
    treatDF$timepoint[treatDF$terms == "Soil Org P Pool 0-10cm"] <- length(unique(soil_organic_p_pool$Date[soil_organic_p_pool$Depth=="0_10"]))  
    treatDF$notes[treatDF$terms == "Soil Org P Pool 0-10cm"] <- "Averaged across all P forms"
    
    
    ### Soil Org P pool
    out <- summaryBy(soil_organic_p_g_m2~Ring,data=soil_organic_p_pool[soil_organic_p_pool$Depth=="10_30",],FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Soil Org P Pool 10-30cm", 2:7] <- out$soil_organic_p_g_m2
    treatDF$year_start[treatDF$terms == "Soil Org P Pool 10-30cm"] <- min(year(soil_organic_p_pool$Date[soil_organic_p_pool$Depth=="10_30"]))    
    treatDF$year_end[treatDF$terms == "Soil Org P Pool 10-30cm"] <- max(year(soil_organic_p_pool$Date[soil_organic_p_pool$Depth=="10_30"]))    
    treatDF$timepoint[treatDF$terms == "Soil Org P Pool 10-30cm"] <- length(unique(soil_organic_p_pool$Date[soil_organic_p_pool$Depth=="10_30"]))  
    treatDF$notes[treatDF$terms == "Soil Org P Pool 10-30cm"] <- "Averaged across all P forms"
    
    
    ### Soil Org P pool
    out <- summaryBy(soil_organic_p_g_m2~Ring,data=soil_organic_p_pool[soil_organic_p_pool$Depth=="transition",],FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Soil Org P Pool 30-60cm", 2:7] <- out$soil_organic_p_g_m2
    treatDF$year_start[treatDF$terms == "Soil Org P Pool 30-60cm"] <- min(year(soil_organic_p_pool$Date[soil_organic_p_pool$Depth=="transition"]))    
    treatDF$year_end[treatDF$terms == "Soil Org P Pool 30-60cm"] <- max(year(soil_organic_p_pool$Date[soil_organic_p_pool$Depth=="transition"]))    
    treatDF$timepoint[treatDF$terms == "Soil Org P Pool 30-60cm"] <- length(unique(soil_organic_p_pool$Date[soil_organic_p_pool$Depth=="transition"]))  
    treatDF$notes[treatDF$terms == "Soil Org P Pool 30-60cm"] <- "Averaged across all P forms"
    
    
    
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
    #out <- summaryBy(mycorrhizal_p_g_m2~Ring,data=mycorrhizal_p_pool[mycorrhizal_p_pool$Depth=="0_10",],FUN=mean,keep.names=T,na.rm=T)
    #treatDF[treatDF$terms == "Mycorrhizal P pool 0-10cm", 2:7] <- out$mycorrhizal_p_g_m2
    #treatDF$year_start[treatDF$terms == "Mycorrhizal P pool 0-10cm"] <- min(year(mycorrhizal_p_pool$Date[mycorrhizal_p_pool$Depth=="0_10"]))    
    #treatDF$year_end[treatDF$terms == "Mycorrhizal P pool 0-10cm"] <- max(year(mycorrhizal_p_pool$Date[mycorrhizal_p_pool$Depth=="0_10"]))    
    #treatDF$timepoint[treatDF$terms == "Mycorrhizal P pool 0-10cm"] <- length(unique(mycorrhizal_p_pool$Date[mycorrhizal_p_pool$Depth=="0_10"]))  
    #treatDF$notes[treatDF$terms == "Mycorrhizal P pool 0-10cm"] <- "Top 10 cm"
    #
    #### Mycorrhizal P pool
    #out <- summaryBy(mycorrhizal_p_g_m2~Ring,data=mycorrhizal_p_pool[mycorrhizal_p_pool$Depth=="10_30",],FUN=mean,keep.names=T,na.rm=T)
    #treatDF[treatDF$terms == "Mycorrhizal P pool 10-30cm", 2:7] <- out$mycorrhizal_p_g_m2
    #treatDF$year_start[treatDF$terms == "Mycorrhizal P pool 10-30cm"] <- min(year(mycorrhizal_p_pool$Date[mycorrhizal_p_pool$Depth=="10_30"]))    
    #treatDF$year_end[treatDF$terms == "Mycorrhizal P pool 10-30cm"] <- max(year(mycorrhizal_p_pool$Date[mycorrhizal_p_pool$Depth=="10_30"]))    
    #treatDF$timepoint[treatDF$terms == "Mycorrhizal P pool 10-30cm"] <- length(unique(mycorrhizal_p_pool$Date[mycorrhizal_p_pool$Depth=="10_30"]))  
    #treatDF$notes[treatDF$terms == "Mycorrhizal P pool 10-30cm"] <- "Top 10-30 cm"
    #
    #### MMycorrhizal P pool
    #out <- summaryBy(mycorrhizal_p_g_m2~Ring,data=mycorrhizal_p_pool[mycorrhizal_p_pool$Depth=="transition",],FUN=mean,keep.names=T,na.rm=T)
    #treatDF[treatDF$terms == "Mycorrhizal P pool 30-60cm", 2:7] <- out$mycorrhizal_p_g_m2
    #treatDF$year_start[treatDF$terms == "Mycorrhizal P pool 30-60cm"] <- min(year(mycorrhizal_p_pool$Date[mycorrhizal_p_pool$Depth=="transition"]))    
    #treatDF$year_end[treatDF$terms == "Mycorrhizal P pool 30-60cm"] <- max(year(mycorrhizal_p_pool$Date[mycorrhizal_p_pool$Depth=="transition"]))    
    #treatDF$timepoint[treatDF$terms == "Mycorrhizal P pool 30-60cm"] <- length(unique(mycorrhizal_p_pool$Date[mycorrhizal_p_pool$Depth=="transition"]))  
    #treatDF$notes[treatDF$terms == "Mycorrhizal P pool 30-60cm"] <- "Top 10 cm"
    
    ### calculate treatment averages
    treatDF$aCO2 <- round(rowMeans(subset(treatDF, select=c(R2, R3, R6)), na.rm=T), 5)
    treatDF$eCO2 <- round(rowMeans(subset(treatDF, select=c(R1, R4, R5)), na.rm=T), 5)
    
    treatDF$aCO2_sd <- rowSds(as.matrix(subset(treatDF, select=c(R2, R3, R6))), na.rm=T)
    treatDF$eCO2_sd <- rowSds(as.matrix(subset(treatDF, select=c(R1, R4, R5))), na.rm=T)
    
    ###### Diff (eCO2 - aCO2)
    treatDF$diff <- round(treatDF$eCO2 - treatDF$aCO2, 4)
    
    ###### percent differences (eCO2 - aCO2) / aCO2 * 100
    treatDF$percent_diff <- round((treatDF$eCO2 - treatDF$aCO2) / (treatDF$aCO2) * 100, 2)
    
    write.csv(treatDF, paste0("plots_tables/summary_tables/", 
                              norm, "/summary_table_P_pool_unnormalized.csv"), 
              row.names=F)
    
    
    ### plot
    tmpDF1 <- treatDF[,c("terms", "aCO2", "eCO2")]
    tmpDF2 <- treatDF[,c("terms", "aCO2_sd", "eCO2_sd")]
    
    plotDF1 <- reshape::melt(tmpDF1, id.var="terms")
    plotDF2 <- reshape::melt(tmpDF2, id.var="terms")
    colnames(plotDF2) <- c("terms", "variable", "value_sd")
    plotDF2$variable <- gsub("_sd", "", plotDF2$variable)
    
    plotDF <- merge(plotDF1, plotDF2, by=c("terms", "variable"))
    plotDF$terms <- gsub(" P Pool", "", plotDF$terms)
    
    p1 <- ggplot(plotDF, aes(terms, value, group=variable))+
      geom_point(aes(fill=variable), pch=21) +
      geom_errorbar(aes(ymin=value-value_sd, ymax=value+value_sd),
                    width=0.2)+
      coord_flip()
    
    pdf(paste0("plots_tables/summary_tables/", norm, "/P_pool_comparison.pdf"))
    plot(p1)
    dev.off()
    
    ##### output tables
    return(treatDF)
      
}

