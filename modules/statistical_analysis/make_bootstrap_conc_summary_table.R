
#### To make EucFACE P summary table by CO2 treatment
#### Ignore time but produce time coverage information

make_bootstrap_conc_summary_table <- function(norm,
                                              canopy_p_concentration,
                                              sapwood_p_concentration,
                                              fineroot_p_concentration,
                                              leaflitter_p_concentration,
                                              understorey_p_concentration,
                                              understorey_litter_p_concentration,
                                              frass_p_concentration,
                                              microbial_p_concentration,
                                              soil_p_concentration,
                                              soil_inorganic_p_concentration,
                                              soil_organic_p_concentration,
                                              soil_phosphate_concentration,
                                              soil_hedley_p_concentration) {
    
    ### Define concentration variable names
    conc.terms <- c("Canopy P Conc", 
                    "Sapwood P Conc", 
                    "Heartwood P Conc",
                    "Fine Root P Conc", 
                    "Coarse Root P Conc",
                    "Leaflitter P Conc",
                    "Understorey P Conc", 
                    "Understorey Litter P Conc", 
                    "Frass P Conc",
                    "Microbial P Conc 0-10cm", 
                    "Microbial P Conc 10-30cm", 
                    "Microbial P Conc 30-60cm", 
                    "Soil P Conc 0-10cm", 
                    "Soil P Conc 10-30cm", 
                    "Soil P Conc 30-60cm", 
                    "Soil Inorg P Conc 0-10cm", 
                    "Soil Inorg P Conc 10-30cm", 
                    "Soil Inorg P Conc 30-60cm", 
                    "Soil Org P Conc 0-10cm", 
                    "Soil Org P Conc 10-30cm", 
                    "Soil Org P Conc 30-60cm", 
                    "Soil Phosphate P Conc 0-10cm",
                    "Soil Phosphate P Conc 10-30cm",
                    "Soil Phosphate P Conc 30-60cm",
                    "Exchangeable Pi Conc 0-10cm", 
                    "Exchangeable Po Conc 0-10cm",
                    "Moderately labile Po Conc 0-10cm", 
                    "Secondary Fe bound Pi Conc 0-10cm", 
                    "Primary Ca bound Pi Conc 0-10cm",
                    "Occluded P Conc 0-10cm")
    
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
    
    
    out <- summaryBy(PercP~Ring,data=canopy_p_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Canopy P Conc", 2:7] <- out$PercP
    treatDF$year_start[treatDF$conc.terms == "Canopy P Conc"] <- min(year(canopy_p_concentration$Date))    
    treatDF$year_end[treatDF$conc.terms == "Canopy P Conc"] <- max(year(canopy_p_concentration$Date))    
    treatDF$timepoint[treatDF$conc.terms == "Canopy P Conc"] <- length(unique(canopy_p_concentration$Date))  
    treatDF$notes[treatDF$conc.terms == "Canopy P Conc"] <- "Only green leaf"

    
    ### Sapwood P concentration
    out <- summaryBy(PercP~Ring,data=sapwood_p_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Sapwood P Conc", 2:7] <- out$PercP
    treatDF$year_start[treatDF$conc.terms == "Sapwood P Conc"] <- min(year(sapwood_p_concentration$Date))    
    treatDF$year_end[treatDF$conc.terms == "Sapwood P Conc"] <- max(year(sapwood_p_concentration$Date))    
    treatDF$timepoint[treatDF$conc.terms == "Sapwood P Conc"] <- length(unique(sapwood_p_concentration$Date)) 
    treatDF$notes[treatDF$conc.terms == "Sapwood P Conc"] <- "Only one data point per ring"
    
    ### Coarse root P concentration
    out <- summaryBy(PercP~Ring,data=sapwood_p_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Coarse Root P Conc", 2:7] <- out$PercP
    treatDF$year_start[treatDF$conc.terms == "Coarse Root P Conc"] <- min(year(sapwood_p_concentration$Date))    
    treatDF$year_end[treatDF$conc.terms == "Coarse Root P Conc"] <- max(year(sapwood_p_concentration$Date))    
    treatDF$timepoint[treatDF$conc.terms == "Coarse Root P Conc"] <- length(unique(sapwood_p_concentration$Date)) 
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
    out <- summaryBy(PercP~Ring,
                     data=microbial_p_concentration[microbial_p_concentration$Depth=="0_10",],
                     FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Microbial P Conc 0-10cm", 2:7] <- out$PercP
    treatDF$year_start[treatDF$conc.terms == "Microbial P Conc 0-10cm"] <- min(year(microbial_p_concentration$Date[microbial_p_concentration$Depth=="0_10"]))    
    treatDF$year_end[treatDF$conc.terms == "Microbial P Conc 0-10cm"] <- max(year(microbial_p_concentration$Date[microbial_p_concentration$Depth=="0_10"]))    
    treatDF$timepoint[treatDF$conc.terms == "Microbial P Conc 0-10cm"] <- length(unique(microbial_p_concentration$Date[microbial_p_concentration$Depth=="0_10"]))  
    treatDF$notes[treatDF$conc.terms == "Microbial P Conc 0-10cm"] <- "Top 10 cm"
    
    
    ### Microbial P concentration
    out <- summaryBy(PercP~Ring,
                     data=microbial_p_concentration[microbial_p_concentration$Depth=="10_30",],
                     FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Microbial P Conc 10-30cm", 2:7] <- out$PercP
    treatDF$year_start[treatDF$conc.terms == "Microbial P Conc 10-30cm"] <- min(year(microbial_p_concentration$Date[microbial_p_concentration$Depth=="10_30"]))    
    treatDF$year_end[treatDF$conc.terms == "Microbial P Conc 10-30cm"] <- max(year(microbial_p_concentration$Date[microbial_p_concentration$Depth=="10_30"]))    
    treatDF$timepoint[treatDF$conc.terms == "Microbial P Conc 10-30cm"] <- length(unique(microbial_p_concentration$Date[microbial_p_concentration$Depth=="10_30"]))  
    treatDF$notes[treatDF$conc.terms == "Microbial P Conc 10-30cm"] <- ""
    
    
    ### Microbial P concentration
    out <- summaryBy(PercP~Ring,
                     data=microbial_p_concentration[microbial_p_concentration$Depth=="transition",],
                     FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Microbial P Conc 30-60cm", 2:7] <- out$PercP
    treatDF$year_start[treatDF$conc.terms == "Microbial P Conc 30-60cm"] <- min(year(microbial_p_concentration$Date[microbial_p_concentration$Depth=="transition"]))    
    treatDF$year_end[treatDF$conc.terms == "Microbial P Conc 30-60cm"] <- max(year(microbial_p_concentration$Date[microbial_p_concentration$Depth=="transition"]))    
    treatDF$timepoint[treatDF$conc.terms == "Microbial P Conc 30-60cm"] <- length(unique(microbial_p_concentration$Date[microbial_p_concentration$Depth=="transition"]))  
    treatDF$notes[treatDF$conc.terms == "Microbial P Conc 30-60cm"] <- "transition"
    
    
    ### Soil P concentration
    out <- summaryBy(PercP~Ring,data=soil_p_concentration[soil_p_concentration$Depth=="0_10",],
                     FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Soil P Conc 0-10cm", 2:7] <- out$PercP
    treatDF$year_start[treatDF$conc.terms == "Soil P Conc 0-10cm"] <- min(year(soil_p_concentration$Date[soil_p_concentration$Depth=="0_10"]))    
    treatDF$year_end[treatDF$conc.terms == "Soil P Conc 0-10cm"] <- max(year(soil_p_concentration$Date[soil_p_concentration$Depth=="0_10"]))    
    treatDF$timepoint[treatDF$conc.terms == "Soil P Conc 0-10cm"] <- length(unique(soil_p_concentration$Date[soil_p_concentration$Depth=="0_10"]))  
    treatDF$notes[treatDF$conc.terms == "Soil P Conc 0-10cm"] <- "Averaged across all P forms"
    
    
    ### Soil P concentration
    out <- summaryBy(PercP~Ring,data=soil_p_concentration[soil_p_concentration$Depth=="10_30",],
                     FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Soil P Conc 10-30cm", 2:7] <- out$PercP
    treatDF$year_start[treatDF$conc.terms == "Soil P Conc 10-30cm"] <- min(year(soil_p_concentration$Date[soil_p_concentration$Depth=="10_30"]))    
    treatDF$year_end[treatDF$conc.terms == "Soil P Conc 10-30cm"] <- max(year(soil_p_concentration$Date[soil_p_concentration$Depth=="10_30"]))    
    treatDF$timepoint[treatDF$conc.terms == "Soil P Conc 10-30cm"] <- length(unique(soil_p_concentration$Date[soil_p_concentration$Depth=="10_30"]))  
    treatDF$notes[treatDF$conc.terms == "Soil P Conc 10-30cm"] <- "Averaged across all P forms"
    
    
    ### Soil P concentration
    out <- summaryBy(PercP~Ring,data=soil_p_concentration[soil_p_concentration$Depth=="transition",],
                     FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Soil P Conc 30-60cm", 2:7] <- out$PercP
    treatDF$year_start[treatDF$conc.terms == "Soil P Conc 30-60cm"] <- min(year(soil_p_concentration$Date[soil_p_concentration$Depth=="transition"]))    
    treatDF$year_end[treatDF$conc.terms == "Soil P Conc 30-60cm"] <- max(year(soil_p_concentration$Date[soil_p_concentration$Depth=="transition"]))    
    treatDF$timepoint[treatDF$conc.terms == "Soil P Conc 30-60cm"] <- length(unique(soil_p_concentration$Date[soil_p_concentration$Depth=="transition"]))  
    treatDF$notes[treatDF$conc.terms == "Soil P Conc 30-60cm"] <- "Averaged across all P forms"
    
    
    ### Soil Inorganic P concentration
    out <- summaryBy(PercP~Ring,data=soil_inorganic_p_concentration[soil_inorganic_p_concentration$Depth=="0_10",],
                     FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Soil Inorg P Conc 0-10cm", 2:7] <- out$PercP
    treatDF$year_start[treatDF$conc.terms == "Soil Inorg P Conc 0-10cm"] <- min(year(soil_inorganic_p_concentration$Date[soil_inorganic_p_concentration$Depth=="0_10"]))    
    treatDF$year_end[treatDF$conc.terms == "Soil Inorg P Conc 0-10cm"] <- max(year(soil_inorganic_p_concentration$Date[soil_inorganic_p_concentration$Depth=="0_10"]))    
    treatDF$timepoint[treatDF$conc.terms == "Soil Inorg P Conc 0-10cm"] <- length(unique(soil_inorganic_p_concentration$Date[soil_inorganic_p_concentration$Depth=="0_10"]))  
    treatDF$notes[treatDF$conc.terms == "Soil Inorg P Conc 0-10cm"] <- "Averaged across all P forms"
    
    
    ### Soil Inorg P concentration
    out <- summaryBy(PercP~Ring,data=soil_inorganic_p_concentration[soil_inorganic_p_concentration$Depth=="10_30",],
                     FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Soil Inorg P Conc 10-30cm", 2:7] <- out$PercP
    treatDF$year_start[treatDF$conc.terms == "Soil Inorg P Conc 10-30cm"] <- min(year(soil_inorganic_p_concentration$Date[soil_inorganic_p_concentration$Depth=="10_30"]))    
    treatDF$year_end[treatDF$conc.terms == "Soil Inorg P Conc 10-30cm"] <- max(year(soil_inorganic_p_concentration$Date[soil_inorganic_p_concentration$Depth=="10_30"]))    
    treatDF$timepoint[treatDF$conc.terms == "Soil Inorg P Conc 10-30cm"] <- length(unique(soil_inorganic_p_concentration$Date[soil_inorganic_p_concentration$Depth=="10_30"]))  
    treatDF$notes[treatDF$conc.terms == "Soil Inorg P Conc 10-30cm"] <- "Averaged across all P forms"
    
    
    ### Soil Inorg P concentration
    out <- summaryBy(PercP~Ring,data=soil_inorganic_p_concentration[soil_inorganic_p_concentration$Depth=="transition",],
                     FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Soil Inorg P Conc 30-60cm", 2:7] <- out$PercP
    treatDF$year_start[treatDF$conc.terms == "Soil Inorg P Conc 30-60cm"] <- min(year(soil_inorganic_p_concentration$Date[soil_inorganic_p_concentration$Depth=="transition"]))    
    treatDF$year_end[treatDF$conc.terms == "Soil Inorg P Conc 30-60cm"] <- max(year(soil_inorganic_p_concentration$Date[soil_inorganic_p_concentration$Depth=="transition"]))    
    treatDF$timepoint[treatDF$conc.terms == "Soil Inorg P Conc 30-60cm"] <- length(unique(soil_inorganic_p_concentration$Date[soil_inorganic_p_concentration$Depth=="transition"]))  
    treatDF$notes[treatDF$conc.terms == "Soil Inorg P Conc 30-60cm"] <- "Averaged across all P forms"
    
    
    ### Soil Organic P concentration
    out <- summaryBy(PercP~Ring,data=soil_organic_p_concentration[soil_organic_p_concentration$Depth=="0_10",],
                     FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Soil Org P Conc 0-10cm", 2:7] <- out$PercP
    treatDF$year_start[treatDF$conc.terms == "Soil Org P Conc 0-10cm"] <- min(year(soil_organic_p_concentration$Date[soil_organic_p_concentration$Depth=="0_10"]))    
    treatDF$year_end[treatDF$conc.terms == "Soil Org P Conc 0-10cm"] <- max(year(soil_organic_p_concentration$Date[soil_organic_p_concentration$Depth=="0_10"]))    
    treatDF$timepoint[treatDF$conc.terms == "Soil Org P Conc 0-10cm"] <- length(unique(soil_organic_p_concentration$Date[soil_organic_p_concentration$Depth=="0_10"]))  
    treatDF$notes[treatDF$conc.terms == "Soil Org P Conc 0-10cm"] <- "Averaged across all P forms"
    
    
    ### Soil Org P concentration
    out <- summaryBy(PercP~Ring,data=soil_organic_p_concentration[soil_organic_p_concentration$Depth=="10_30",],
                     FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Soil Org P Conc 10-30cm", 2:7] <- out$PercP
    treatDF$year_start[treatDF$conc.terms == "Soil Org P Conc 10-30cm"] <- min(year(soil_organic_p_concentration$Date[soil_organic_p_concentration$Depth=="10_30"]))    
    treatDF$year_end[treatDF$conc.terms == "Soil Org P Conc 10-30cm"] <- max(year(soil_organic_p_concentration$Date[soil_organic_p_concentration$Depth=="10_30"]))    
    treatDF$timepoint[treatDF$conc.terms == "Soil Org P Conc 10-30cm"] <- length(unique(soil_organic_p_concentration$Date[soil_organic_p_concentration$Depth=="10_30"]))  
    treatDF$notes[treatDF$conc.terms == "Soil Org P Conc 10-30cm"] <- "Averaged across all P forms"
    
    
    ### Soil Org P concentration
    out <- summaryBy(PercP~Ring,data=soil_organic_p_concentration[soil_organic_p_concentration$Depth=="transition",],
                     FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Soil Org P Conc 30-60cm", 2:7] <- out$PercP
    treatDF$year_start[treatDF$conc.terms == "Soil Org P Conc 30-60cm"] <- min(year(soil_organic_p_concentration$Date[soil_organic_p_concentration$Depth=="transition"]))    
    treatDF$year_end[treatDF$conc.terms == "Soil Org P Conc 30-60cm"] <- max(year(soil_organic_p_concentration$Date[soil_organic_p_concentration$Depth=="transition"]))    
    treatDF$timepoint[treatDF$conc.terms == "Soil Org P Conc 30-60cm"] <- length(unique(soil_organic_p_concentration$Date[soil_organic_p_concentration$Depth=="transition"]))  
    treatDF$notes[treatDF$conc.terms == "Soil Org P Conc 30-60cm"] <- "Averaged across all P forms"
    
    
    
    ### Soil Phosphate P concentration
    out <- summaryBy(PercP~Ring,data=soil_phosphate_concentration[soil_phosphate_concentration$Depth=="0_10",],FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Soil Phosphate P Conc 0-10cm", 2:7] <- out$PercP
    treatDF$year_start[treatDF$conc.terms == "Soil Phosphate P Conc 0-10cm"] <- min(year(soil_phosphate_concentration$Date[soil_phosphate_concentration$Depth=="0_10"]))    
    treatDF$year_end[treatDF$conc.terms == "Soil Phosphate P Conc 0-10cm"] <- max(year(soil_phosphate_concentration$Date[soil_phosphate_concentration$Depth=="0_10"]))    
    treatDF$timepoint[treatDF$conc.terms == "Soil Phosphate P Conc 0-10cm"] <- length(unique(soil_phosphate_concentration$Date[soil_phosphate_concentration$Depth=="0_10"]))  
    treatDF$notes[treatDF$conc.terms == "Soil Phosphate P Conc 0-10cm"] <- "Top 10 cm"
    
    ### Soil Phosphate P concentration
    out <- summaryBy(PercP~Ring,data=soil_phosphate_concentration[soil_phosphate_concentration$Depth=="10_30",],FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Soil Phosphate P Conc 10-30cm", 2:7] <- out$PercP
    treatDF$year_start[treatDF$conc.terms == "Soil Phosphate P Conc 10-30cm"] <- min(year(soil_phosphate_concentration$Date[soil_phosphate_concentration$Depth=="10_30"]))    
    treatDF$year_end[treatDF$conc.terms == "Soil Phosphate P Conc 10-30cm"] <- max(year(soil_phosphate_concentration$Date[soil_phosphate_concentration$Depth=="10_30"]))    
    treatDF$timepoint[treatDF$conc.terms == "Soil Phosphate P Conc 10-30cm"] <- length(unique(soil_phosphate_concentration$Date[soil_phosphate_concentration$Depth=="10_30"]))  
    treatDF$notes[treatDF$conc.terms == "Soil Phosphate P Conc 10-30cm"] <- "Top 10-30 cm"
    
    
    ### Soil Phosphate P concentration
    out <- summaryBy(PercP~Ring,data=soil_phosphate_concentration[soil_phosphate_concentration$Depth=="transition",],FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Soil Phosphate P Conc 30-60cm", 2:7] <- out$PercP
    treatDF$year_start[treatDF$conc.terms == "Soil Phosphate P Conc 30-60cm"] <- min(year(soil_phosphate_concentration$Date[soil_phosphate_concentration$Depth=="transition"]))    
    treatDF$year_end[treatDF$conc.terms == "Soil Phosphate P Conc 30-60cm"] <- max(year(soil_phosphate_concentration$Date[soil_phosphate_concentration$Depth=="transition"]))    
    treatDF$timepoint[treatDF$conc.terms == "Soil Phosphate P Conc 30-60cm"] <- length(unique(soil_phosphate_concentration$Date[soil_phosphate_concentration$Depth=="transition"]))  
    treatDF$notes[treatDF$conc.terms == "Soil Phosphate P Conc 30-60cm"] <- "Top 30-60 cm"
    
    
    ### Exchangeable Pi Conc
    out <- summaryBy(F1_2_Pi_Exchangeable~Ring,data=soil_hedley_p_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Exchangeable Pi Conc 0-10cm", 2:7] <- out$F1_2_Pi_Exchangeable
    treatDF$year_start[treatDF$conc.terms == "Exchangeable Pi Conc 0-10cm"] <- min(soil_hedley_p_concentration$Year)    
    treatDF$year_end[treatDF$conc.terms == "Exchangeable Pi Conc 0-10cm"] <- max(soil_hedley_p_concentration$Year)   
    treatDF$timepoint[treatDF$conc.terms == "Exchangeable Pi Conc 0-10cm"] <- length(unique(soil_hedley_p_concentration$Year))  
    treatDF$notes[treatDF$conc.terms == "Exchangeable Pi Conc 0-10cm"] <- " 0-10cm"
    
    ### Exchangeable Po Conc
    out <- summaryBy(F1_2_Po_Exchangeable~Ring,data=soil_hedley_p_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Exchangeable Po Conc 0-10cm", 2:7] <- out$F1_2_Po_Exchangeable
    treatDF$year_start[treatDF$conc.terms == "Exchangeable Po Conc 0-10cm"] <- min(soil_hedley_p_concentration$Year)    
    treatDF$year_end[treatDF$conc.terms == "Exchangeable Po Conc 0-10cm"] <- max(soil_hedley_p_concentration$Year)   
    treatDF$timepoint[treatDF$conc.terms == "Exchangeable Po Conc 0-10cm"] <- length(unique(soil_hedley_p_concentration$Year))  
    treatDF$notes[treatDF$conc.terms == "Exchangeable Po Conc 0-10cm"] <- " 0-10cm"
    
    ### Moderately labile Po Conc
    out <- summaryBy(F3_Po_Moderately_labile~Ring,data=soil_hedley_p_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Moderately labile Po Conc 0-10cm", 2:7] <- out$F3_Po_Moderately_labile
    treatDF$year_start[treatDF$conc.terms == "Moderately labile Po Conc 0-10cm"] <- min(soil_hedley_p_concentration$Year)    
    treatDF$year_end[treatDF$conc.terms == "Moderately labile Po Conc 0-10cm"] <- max(soil_hedley_p_concentration$Year)   
    treatDF$timepoint[treatDF$conc.terms == "Moderately labile Po Conc 0-10cm"] <- length(unique(soil_hedley_p_concentration$Year))  
    treatDF$notes[treatDF$conc.terms == "Moderately labile Po Conc 0-10cm"] <- " 0-10cm"
    
    ### Secondary Fe bound Pi Conc
    out <- summaryBy(F3_Fe_bound_P_Secondary_mineral~Ring,data=soil_hedley_p_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Secondary Fe bound Pi Conc 0-10cm", 2:7] <- out$F3_Fe_bound_P_Secondary_mineral
    treatDF$year_start[treatDF$conc.terms == "Secondary Fe bound Pi Conc 0-10cm"] <- min(soil_hedley_p_concentration$Year)    
    treatDF$year_end[treatDF$conc.terms == "Secondary Fe bound Pi Conc 0-10cm"] <- max(soil_hedley_p_concentration$Year)   
    treatDF$timepoint[treatDF$conc.terms == "Secondary Fe bound Pi Conc 0-10cm"] <- length(unique(soil_hedley_p_concentration$Year))  
    treatDF$notes[treatDF$conc.terms == "Secondary Fe bound Pi Conc 0-10cm"] <- " 0-10cm"
    
    ### Primary Ca bound Pi Conc
    out <- summaryBy(F4_Ca_bound_Primary_Mineral~Ring,data=soil_hedley_p_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Primary Ca bound Pi Conc 0-10cm", 2:7] <- out$F4_Ca_bound_Primary_Mineral
    treatDF$year_start[treatDF$conc.terms == "Primary Ca bound Pi Conc 0-10cm"] <- min(soil_hedley_p_concentration$Year)    
    treatDF$year_end[treatDF$conc.terms == "Primary Ca bound Pi Conc 0-10cm"] <- max(soil_hedley_p_concentration$Year)   
    treatDF$timepoint[treatDF$conc.terms == "Primary Ca bound Pi Conc 0-10cm"] <- length(unique(soil_hedley_p_concentration$Year))  
    treatDF$notes[treatDF$conc.terms == "Primary Ca bound Pi Conc 0-10cm"] <- " 0-10cm"
    
    ### Occluded P Conc
    out <- summaryBy(F5_6_Occluded~Ring,data=soil_hedley_p_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Occluded P Conc 0-10cm", 2:7] <- out$F5_6_Occluded
    treatDF$year_start[treatDF$conc.terms == "Occluded P Conc 0-10cm"] <- min(soil_hedley_p_concentration$Year)    
    treatDF$year_end[treatDF$conc.terms == "Occluded P Conc 0-10cm"] <- max(soil_hedley_p_concentration$Year)   
    treatDF$timepoint[treatDF$conc.terms == "Occluded P Conc 0-10cm"] <- length(unique(soil_hedley_p_concentration$Year))  
    treatDF$notes[treatDF$conc.terms == "Occluded P Conc 0-10cm"] <- " 0-10cm"
    
    
    ### Mycorrhizal P concentration
    #treatDF$notes[treatDF$conc.terms == "Mycorrhizal P Conc"] <- "Data not yet available"
    
    ### sapwood - from Kristine one value only
    treatDF[treatDF$conc.terms == "Heartwood P Conc", 2:7] <- 0.004
    
    
    ### calculate treatment averages
    treatDF$aCO2 <- round(rowMeans(subset(treatDF, select=c(R2, R3, R6)), na.rm=T), 5)
    treatDF$eCO2 <- round(rowMeans(subset(treatDF, select=c(R1, R4, R5)), na.rm=T), 5)
    
    treatDF$aCO2_sd <- rowSds(as.matrix(subset(treatDF, select=c(R2, R3, R6))), na.rm=T)
    treatDF$eCO2_sd <- rowSds(as.matrix(subset(treatDF, select=c(R1, R4, R5))), na.rm=T)
    
    ###### Diff (eCO2 - aCO2)
    treatDF$diff <- round(treatDF$eCO2 - treatDF$aCO2, 4)
    
    ### sd of the diff
    treatDF$diff_sd <- sqrt((treatDF$aCO2_sd^2+treatDF$eCO2_sd^2)/2)
    
    ### confidence interval of the diff
    treatDF$diff_cf <- qt(0.95, 4) * treatDF$diff_sd * (sqrt(2/3))
    
    ###### percent differences (eCO2 - aCO2) / aCO2 * 100
    treatDF$percent_diff <- round((treatDF$eCO2 - treatDF$aCO2) / (treatDF$aCO2) * 100, 2)
    
    ### save
    write.csv(treatDF, paste0("plots_tables/summary_tables/", norm, 
                              "/summary_table_P_concentration_", norm, 
                              ".csv"), row.names=F)
    
    
    ### plot
    tmpDF1 <- treatDF[,c("conc.terms", "aCO2", "eCO2")]
    tmpDF2 <- treatDF[,c("conc.terms", "aCO2_sd", "eCO2_sd")]
    
    plotDF1 <- reshape::melt(tmpDF1, id.var="conc.terms")
    plotDF2 <- reshape::melt(tmpDF2, id.var="conc.terms")
    colnames(plotDF2) <- c("conc.terms", "variable", "value_sd")
    plotDF2$variable <- gsub("_sd", "", plotDF2$variable)
    
    plotDF <- merge(plotDF1, plotDF2, by=c("conc.terms", "variable"))
    plotDF$conc.terms <- gsub(" P Conc", "", plotDF$conc.terms)
    
    p1 <- ggplot(plotDF, aes(conc.terms, value, group=variable))+
      geom_errorbar(aes(ymin=value-value_sd, ymax=value+value_sd),
                    position=position_dodge2(), width=0.3)+
      geom_point(aes(fill=variable), pch=21, stat = "identity", size=2,
                 position=position_dodge2(width=0.3)) +
      coord_flip()+
      scale_fill_manual(name="",
                        values=c("aCO2"="blue3",
                                 "eCO2"="red2"))
    
    pdf(paste0("plots_tables/summary_tables/", norm, "/P_concentration_comparison.pdf"))
    plot(p1)
    dev.off()
    
    ##### output tables
    return(treatDF)
      
}

