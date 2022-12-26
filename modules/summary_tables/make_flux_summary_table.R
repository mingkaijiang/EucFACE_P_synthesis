
#### To make EucFACE P summary table by CO2 treatment
make_flux_summary_table <- function(norm,
                                    soil_p_mineralization,
                                    soil_p_leaching,
                                    canopy_p_flux,
                                    frass_p_production,
                                    leaflitter_p_flux,
                                    fineroot_p_production,
                                    fineroot_litter_p_flux,
                                    twig_litter_p_flux,
                                    bark_litter_p_flux,
                                    seed_litter_p_flux,
                                    wood_p_flux,
                                    coarse_root_p_flux,
                                    understorey_p_flux,
                                    understorey_litter_p_flux) {

  
    #### Ignore time but produce time coverage information
    #### This is for fluxes
  
  ### Define production variable names
  terms <- c("Canopy P flux",              # include retranslocated and new uptake flux
             "Herbivory P flux",
             "Wood P flux",                # include retranslocated and new uptake flux
             "Fine Root P flux",           # include retranslocated and new uptake flux
             "Coarse Root P flux",         # include retranslocated and new uptake flux
             "Leaflitter P flux",          # Proxy for new uptake
             "Fineroot Litter P flux",     # Proxy for new uptake
             "Twig litter P flux",         # to be included in the new uptake
             "Bark litter P flux",         # to be included in the new uptake
             "Seed litter P flux",         # to be included in the new uptake
             "Frass P flux",               # to be included in the new uptake
             "Understorey P flux",         # include retranslocated and new uptake flux
             "Understorey Litter P flux",  # Proxy for new uptake 
             "Canopy retrans P flux",     
             "Sapwood retrans P flux",
             "Fineroot retrans P flux",
             "Coarseroot retrans P flux",
             "Understorey retrans P flux",
             "Total vegetation production P flux",
             "Total vegetation retranslocation P flux",
             "Total vegetation uptake P flux",
             "Mineralization P flux 0-10cm",
             "Mineralization P flux 10-30cm",
             "Mineralization P flux 30-60cm",
             "Leaching P flux")
  
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
  
  
  ### convert daily flux in mg P m2 d-1 to g P m-2 yr-1
  conv <- 365 / 1000
  
  
    if (norm == "unnormalized") {
      
      ### Canopy P flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Canopy P flux", i+1] <- with(canopy_p_flux[canopy_p_flux$Ring ==i,],
                                                               sum(canopy_p_flux*Days)/sum(Days)) * conv
      }
      treatDF$year_start[treatDF$terms == "Canopy P flux"] <- min(year(canopy_p_flux$Date))    
      treatDF$year_end[treatDF$terms == "Canopy P flux"] <- max(year(canopy_p_flux$Date))    
      treatDF$timepoint[treatDF$terms == "Canopy P flux"] <- length(unique(canopy_p_flux$Date))  
      treatDF$notes[treatDF$terms == "Canopy P flux"] <- "need to consider leaf turnover"
      
      
      ### Herbivory P flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Herbivory P flux", i+1] <- with(canopy_p_flux[canopy_p_flux$Ring ==i,],
                                                               sum(herbivory_p_flux*Days)/sum(Days)) * conv
      }
      treatDF$year_start[treatDF$terms == "Herbivory P flux"] <- min(year(canopy_p_flux$Date))    
      treatDF$year_end[treatDF$terms == "Herbivory P flux"] <- max(year(canopy_p_flux$Date))    
      treatDF$timepoint[treatDF$terms == "Herbivory P flux"] <- length(unique(canopy_p_flux$Date))  
      treatDF$notes[treatDF$terms == "Herbivory P flux"] <- "need to consider leaf turnover"
      
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
        treatDF[treatDF$terms == "Coarse Root P flux", i+1] <- with(coarse_root_p_flux[coarse_root_p_flux$Ring ==i,],
                                                                    sum(coarse_root_p_flux*Days)/sum(Days)) * conv
      }
      treatDF$year_start[treatDF$terms == "Coarse Root P flux"] <- min(year(coarse_root_p_flux$Date))    
      treatDF$year_end[treatDF$terms == "Coarse Root P flux"] <- max(year(coarse_root_p_flux$Date))    
      treatDF$timepoint[treatDF$terms == "Coarse Root P flux"] <- length(unique(coarse_root_p_flux$Date))  
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
      
      ### Understorey Litter P flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Understorey Litter P flux", i+1] <- with(understorey_litter_p_flux[understorey_litter_p_flux$Ring ==i,],
                                                                           sum(understorey_litter_p_flux*Days)/sum(Days)) * conv
      }
      treatDF$year_start[treatDF$terms == "Understorey Litter P flux"] <- min(year(understorey_litter_p_flux$Date))    
      treatDF$year_end[treatDF$terms == "Understorey Litter P flux"] <- max(year(understorey_litter_p_flux$Date))    
      treatDF$timepoint[treatDF$terms == "Understorey Litter P flux"] <- length(unique(understorey_litter_p_flux$Date))  
      treatDF$notes[treatDF$terms == "Understorey Litter P flux"] <- "Used Varsha's harvest data"
      
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
      
      ### Fine Root litter flux
      # assume it's the same as fine root production flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Fineroot Litter P flux", i+1] <- with(fineroot_litter_p_flux[fineroot_litter_p_flux$Ring ==i,],
                                                                        sum(fineroot_litter_p_flux*Days)/sum(Days)) * conv
      }
      treatDF$year_start[treatDF$terms == "Fineroot Litter P flux"] <- min(year(fineroot_litter_p_flux$Date))    
      treatDF$year_end[treatDF$terms == "Fineroot Litter P flux"] <- max(year(fineroot_litter_p_flux$Date))    
      treatDF$timepoint[treatDF$terms == "Fineroot Litter P flux"] <- length(unique(fineroot_litter_p_flux$Date))  
      treatDF$notes[treatDF$terms == "Fineroot Litter P flux"] <- "Assuming fineroot production = fineroot litter production"
      
      
      ### twig litter flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Twig litter P flux", i+1] <- with(twig_litter_p_flux[twig_litter_p_flux$Ring ==i,],
                                                                    sum(twiglitter_p_flux_mg_m2_d*Days)/sum(Days)) * conv
      }
      treatDF$year_start[treatDF$terms == "Twig litter P flux"] <- min(year(twig_litter_p_flux$Date))    
      treatDF$year_end[treatDF$terms == "Twig litter P flux"] <- max(year(twig_litter_p_flux$Date))    
      treatDF$timepoint[treatDF$terms == "Twig litter P flux"] <- length(unique(twig_litter_p_flux$Date))  
      treatDF$notes[treatDF$terms == "Twig litter P flux"] <- "Assume wood P concentration"
      
      ### bark litter flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Bark litter P flux", i+1] <- with(bark_litter_p_flux[bark_litter_p_flux$Ring ==i,],
                                                                    sum(barklitter_p_flux_mg_m2_d*Days)/sum(Days)) * conv
      }
      treatDF$year_start[treatDF$terms == "Bark litter P flux"] <- min(year(bark_litter_p_flux$Date))    
      treatDF$year_end[treatDF$terms == "Bark litter P flux"] <- max(year(bark_litter_p_flux$Date))    
      treatDF$timepoint[treatDF$terms == "Bark litter P flux"] <- length(unique(bark_litter_p_flux$Date))  
      treatDF$notes[treatDF$terms == "Bark litter P flux"] <- "Assume wood P concentration"
      
      ### seed litter flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Seed litter P flux", i+1] <- with(seed_litter_p_flux[seed_litter_p_flux$Ring ==i,],
                                                                    sum(seedlitter_p_flux_mg_m2_d*Days)/sum(Days)) * conv
      }
      treatDF$year_start[treatDF$terms == "Seed litter P flux"] <- min(year(seed_litter_p_flux$Date))    
      treatDF$year_end[treatDF$terms == "Seed litter P flux"] <- max(year(seed_litter_p_flux$Date))    
      treatDF$timepoint[treatDF$terms == "Seed litter P flux"] <- length(unique(seed_litter_p_flux$Date))  
      treatDF$notes[treatDF$terms == "Seed litter P flux"] <- "Assume wood P concentration"
      
      
      ###  P mineralization flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Mineralization P flux 0-10cm", i+1] <- with(soil_p_mineralization[soil_p_mineralization$Ring ==i&soil_p_mineralization$Depth =="0_10",],
                                                                              sum(p_mineralization_mg_m2_d*Days)/sum(Days)) * conv
      }
      treatDF$year_start[treatDF$terms == "Mineralization P flux 0-10cm"] <- min(year(soil_p_mineralization$Date[soil_p_mineralization$Depth =="0_10"]))    
      treatDF$year_end[treatDF$terms == "Mineralization P flux 0-10cm"] <- max(year(soil_p_mineralization$Date[soil_p_mineralization$Depth =="0_10"]))    
      treatDF$timepoint[treatDF$terms == "Mineralization P flux 0-10cm"] <- length(unique(soil_p_mineralization$Date[soil_p_mineralization$Depth =="0_10"]))  
      treatDF$notes[treatDF$terms == "Mineralization P flux 0-10cm"] <- " 0-10cm"
      
      ###  P mineralization flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Mineralization P flux 10-30cm", i+1] <- with(soil_p_mineralization[soil_p_mineralization$Ring ==i&soil_p_mineralization$Depth =="10_30",],
                                                                               sum(p_mineralization_mg_m2_d*Days)/sum(Days)) * conv
      }
      treatDF$year_start[treatDF$terms == "Mineralization P flux 10-30cm"] <- min(year(soil_p_mineralization$Date[soil_p_mineralization$Depth =="10_30"]))    
      treatDF$year_end[treatDF$terms == "Mineralization P flux 10-30cm"] <- max(year(soil_p_mineralization$Date[soil_p_mineralization$Depth =="10_30"]))    
      treatDF$timepoint[treatDF$terms == "Mineralization P flux 10-30cm"] <- length(unique(soil_p_mineralization$Date[soil_p_mineralization$Depth =="10_30"]))  
      treatDF$notes[treatDF$terms == "Mineralization P flux 10-30cm"] <- " 10-30cm"
      
      ###  P mineralization flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Mineralization P flux 30-60cm", i+1] <- with(soil_p_mineralization[soil_p_mineralization$Ring ==i&soil_p_mineralization$Depth =="transition",],
                                                                               sum(p_mineralization_mg_m2_d*Days)/sum(Days)) * conv
      }
      treatDF$year_start[treatDF$terms == "Mineralization P flux 30-60cm"] <- min(year(soil_p_mineralization$Date[soil_p_mineralization$Depth =="transition"]))    
      treatDF$year_end[treatDF$terms == "Mineralization P flux 30-60cm"] <- max(year(soil_p_mineralization$Date[soil_p_mineralization$Depth =="transition"]))    
      treatDF$timepoint[treatDF$terms == "Mineralization P flux 30-60cm"] <- length(unique(soil_p_mineralization$Date[soil_p_mineralization$Depth =="transition"]))  
      treatDF$notes[treatDF$terms == "Mineralization P flux 30-60cm"] <- " 30-60cm"
      
      ###  P leaching flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Leaching P flux", i+1] <- with(soil_p_leaching[soil_p_leaching$Ring ==i,],
                                                                 sum(phosphate_leaching_flux*Days)/sum(Days)) * conv
      }
      treatDF$year_start[treatDF$terms == "Leaching P flux"] <- min(year(soil_p_leaching$Date))    
      treatDF$year_end[treatDF$terms == "Leaching P flux"] <- max(year(soil_p_leaching$Date))    
      treatDF$timepoint[treatDF$terms == "Leaching P flux"] <- length(unique(soil_p_leaching$Date))  
      treatDF$notes[treatDF$terms == "Leaching P flux"] <- "drainage 20 ml/d"
      
      
      
      ###  Canopy retrans P flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Canopy retrans P flux", i+1] <- with(canopy_P_retranslocation_flux[canopy_P_retranslocation_flux$Ring ==i,],
                                                                       sum(canopy_p_retrans_flux*Days)/sum(Days)) * conv
      }
      treatDF$year_start[treatDF$terms == "Canopy retrans P flux"] <- min(year(canopy_P_retranslocation_flux$Date))    
      treatDF$year_end[treatDF$terms == "Canopy retrans P flux"] <- max(year(canopy_P_retranslocation_flux$Date))    
      treatDF$timepoint[treatDF$terms == "Canopy retrans P flux"] <- length(unique(canopy_P_retranslocation_flux$Date))  
      treatDF$notes[treatDF$terms == "Canopy retrans P flux"] <- "calculated as diff of canopy production and litterfall"
      
      
      ###  Sapwood retrans P flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Sapwood retrans P flux", i+1] <- with(sapwood_P_retranslocation_flux[sapwood_P_retranslocation_flux$Ring ==i,],
                                                                        sum(sapwood_p_retrans_flux*Days)/sum(Days)) * conv
      }
      treatDF$year_start[treatDF$terms == "Sapwood retrans P flux"] <- min(year(sapwood_P_retranslocation_flux$Date))    
      treatDF$year_end[treatDF$terms == "Sapwood retrans P flux"] <- max(year(sapwood_P_retranslocation_flux$Date))    
      treatDF$timepoint[treatDF$terms == "Sapwood retrans P flux"] <- length(unique(sapwood_P_retranslocation_flux$Date))  
      treatDF$notes[treatDF$terms == "Sapwood retrans P flux"] <- "heartwood and sapwood P conc."
      
      ###  Coarseroot retrans P flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Coarseroot retrans P flux", i+1] <- with(coarseroot_P_retranslocation_flux[coarseroot_P_retranslocation_flux$Ring ==i,],
                                                                           sum(coarseroot_p_retrans_flux*Days)/sum(Days)) * conv
      }
      treatDF$year_start[treatDF$terms == "Coarseroot retrans P flux"] <- min(year(coarseroot_P_retranslocation_flux$Date))    
      treatDF$year_end[treatDF$terms == "Coarseroot retrans P flux"] <- max(year(coarseroot_P_retranslocation_flux$Date))    
      treatDF$timepoint[treatDF$terms == "Coarseroot retrans P flux"] <- length(unique(coarseroot_P_retranslocation_flux$Date))  
      treatDF$notes[treatDF$terms == "Coarseroot retrans P flux"] <- "heartwood and sapwood P conc."
      
      ###  Fineroot retrans P flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Fineroot retrans P flux", i+1] <- with(fineroot_P_retranslocation_flux[fineroot_P_retranslocation_flux$Ring ==i,],
                                                                         sum(fineroot_p_retrans_flux*Days)/sum(Days)) * conv
      }
      treatDF$year_start[treatDF$terms == "Fineroot retrans P flux"] <- min(year(fineroot_P_retranslocation_flux$Date))    
      treatDF$year_end[treatDF$terms == "Fineroot retrans P flux"] <- max(year(fineroot_P_retranslocation_flux$Date))    
      treatDF$timepoint[treatDF$terms == "Fineroot retrans P flux"] <- length(unique(fineroot_P_retranslocation_flux$Date))  
      treatDF$notes[treatDF$terms == "Fineroot retrans P flux"] <- "assumed"
      
      
      ###  Understorey retrans P flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Understorey retrans P flux", i+1] <- with(understorey_P_retranslocation_flux[understorey_P_retranslocation_flux$Ring ==i,],
                                                                            sum(understorey_p_retrans_flux*Days)/sum(Days)) * conv
      }
      treatDF$year_start[treatDF$terms == "Understorey retrans P flux"] <- min(year(understorey_P_retranslocation_flux$Date))    
      treatDF$year_end[treatDF$terms == "Understorey retrans P flux"] <- max(year(understorey_P_retranslocation_flux$Date))    
      treatDF$timepoint[treatDF$terms == "Understorey retrans P flux"] <- length(unique(understorey_P_retranslocation_flux$Date))  
      treatDF$notes[treatDF$terms == "Understorey retrans P flux"] <- "production - litterfall"
      
      
      
      ###  Total vegetation production P flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Total vegetation production P flux", i+1] <- sum(c(treatDF[treatDF$terms == "Canopy P flux", i+1],
                                                                                     treatDF[treatDF$terms == "Wood P flux", i+1],
                                                                                     treatDF[treatDF$terms == "Fine Root P flux", i+1],
                                                                                     treatDF[treatDF$terms == "Coarse Root P flux", i+1],
                                                                                     treatDF[treatDF$terms == "Twig litter P flux", i+1],
                                                                                     treatDF[treatDF$terms == "Bark litter P flux", i+1],
                                                                                     treatDF[treatDF$terms == "Seed litter P flux", i+1],
                                                                                     #treatDF[treatDF$terms == "Frass P flux", i+1],
                                                                                     treatDF[treatDF$terms == "Understorey P flux", i+1]), na.rm=T)
      }
      
      
      ###  Total vegetation production P flux
      #for (i in c(1:6)) {
      #  treatDF[treatDF$terms == "Total vegetation production P flux", i+1] <- sum(c(treatDF[treatDF$terms == "Canopy P flux", i+1],
      #                                                                               treatDF[treatDF$terms == "Wood P flux", i+1],
      #                                                                               treatDF[treatDF$terms == "Fine Root P flux", i+1],
      #                                                                               treatDF[treatDF$terms == "Coarse Root P flux", i+1],
      #                                                                               treatDF[treatDF$terms == "Twig P flux", i+1],
      #                                                                               treatDF[treatDF$terms == "Bark P flux", i+1],
      #                                                                               treatDF[treatDF$terms == "Seed P flux", i+1],
      #                                                                               treatDF[treatDF$terms == "Frass P flux", i+1],
      #                                                                               treatDF[treatDF$terms == "Understorey P flux", i+1]), na.rm=T)
      #}
      
      ###  Total vegetation retranslocation P flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Total vegetation retranslocation P flux", i+1] <- sum(c(treatDF[treatDF$terms == "Canopy retrans P flux", i+1],
                                                                                          treatDF[treatDF$terms == "Sapwood retrans P flux", i+1],
                                                                                          treatDF[treatDF$terms == "Fineroot retrans P flux", i+1],
                                                                                          treatDF[treatDF$terms == "Coarseroot retrans P flux", i+1],
                                                                                          treatDF[treatDF$terms == "Understorey retrans P flux", i+1]), na.rm=T)
      }
      
      ###  Total vegetation uptake P flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Total vegetation uptake P flux", i+1] <- treatDF[treatDF$terms == "Total vegetation production P flux", i+1] - treatDF[treatDF$terms == "Total vegetation retranslocation P flux", i+1]
      }
      
    } else {
      ### return normalized response, already in unit of g m-2 yr-1
      
      
      ### Canopy P flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Canopy P flux", i+1] <- with(canopy_p_flux[canopy_p_flux$Ring ==i,],
                                                               mean(canopy_p_flux))
      }
      treatDF$year_start[treatDF$terms == "Canopy P flux"] <- min(year(canopy_p_flux$Date))    
      treatDF$year_end[treatDF$terms == "Canopy P flux"] <- max(year(canopy_p_flux$Date))    
      treatDF$timepoint[treatDF$terms == "Canopy P flux"] <- length(unique(canopy_p_flux$Date))  
      treatDF$notes[treatDF$terms == "Canopy P flux"] <- "need to consider leaf turnover"
      
      ### Herbivory P flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Herbivory P flux", i+1] <- with(canopy_p_flux[canopy_p_flux$Ring ==i,],
                                                                  sum(herbivory_p_flux*Days)/sum(Days)) * conv
      }
      treatDF$year_start[treatDF$terms == "Herbivory P flux"] <- min(year(canopy_p_flux$Date))    
      treatDF$year_end[treatDF$terms == "Herbivory P flux"] <- max(year(canopy_p_flux$Date))    
      treatDF$timepoint[treatDF$terms == "Herbivory P flux"] <- length(unique(canopy_p_flux$Date))  
      treatDF$notes[treatDF$terms == "Herbivory P flux"] <- "need to consider leaf turnover"
      
      
      ### Wood P 
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Wood P flux", i+1] <- with(wood_p_flux[wood_p_flux$Ring ==i,],
                                                             mean(wood_p_flux)) 
      }
      treatDF$year_start[treatDF$terms == "Wood P flux"] <- min(year(wood_p_flux$Date))    
      treatDF$year_end[treatDF$terms == "Wood P flux"] <- max(year(wood_p_flux$Date))    
      treatDF$timepoint[treatDF$terms == "Wood P flux"] <- length(unique(wood_p_flux$Date)) 
      treatDF$notes[treatDF$terms == "Wood P flux"] <- "Based on single time point measurement"
      
      ### Fine root P flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Fine Root P flux", i+1] <- with(fineroot_p_production[fineroot_p_production$Ring ==i,],
                                                                  mean(fineroot_p_flux_mg_m2_d)) 
      }
      treatDF$year_start[treatDF$terms == "Fine Root P flux"] <- min(year(fineroot_p_production$Date))    
      treatDF$year_end[treatDF$terms == "Fine Root P flux"] <- max(year(fineroot_p_production$Date))    
      treatDF$timepoint[treatDF$terms == "Fine Root P flux"] <- length(unique(fineroot_p_production$Date))  
      treatDF$notes[treatDF$terms == "Fine Root P flux"] <- "Top 30 cm"
      
      ### Coarse root P flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Coarse Root P flux", i+1] <- with(coarse_root_p_flux[coarse_root_p_flux$Ring ==i,],
                                                                    mean(coarse_root_p_flux)) 
      }
      treatDF$year_start[treatDF$terms == "Coarse Root P flux"] <- min(year(coarse_root_p_flux$Date))    
      treatDF$year_end[treatDF$terms == "Coarse Root P flux"] <- max(year(coarse_root_p_flux$Date))    
      treatDF$timepoint[treatDF$terms == "Coarse Root P flux"] <- length(unique(coarse_root_p_flux$Date))  
      treatDF$notes[treatDF$terms == "Coarse Root P flux"] <- "Allometric rlt with DBH"
      
      ### Understorey P flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Understorey P flux", i+1] <- with(understorey_p_flux[understorey_p_flux$Ring ==i,],
                                                                    mean(understorey_p_flux)) 
      }
      treatDF$year_start[treatDF$terms == "Understorey P flux"] <- min(year(understorey_p_flux$Date))    
      treatDF$year_end[treatDF$terms == "Understorey P flux"] <- max(year(understorey_p_flux$Date))    
      treatDF$timepoint[treatDF$terms == "Understorey P flux"] <- length(unique(understorey_p_flux$Date))  
      treatDF$notes[treatDF$terms == "Understorey P flux"] <- "Used Varsha's harvest data"
      
      ### Understorey Litter P flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Understorey Litter P flux", i+1] <- with(understorey_litter_p_flux[understorey_litter_p_flux$Ring ==i,],
                                                                           mean(understorey_litter_p_flux)) 
      }
      treatDF$year_start[treatDF$terms == "Understorey Litter P flux"] <- min(year(understorey_litter_p_flux$Date))    
      treatDF$year_end[treatDF$terms == "Understorey Litter P flux"] <- max(year(understorey_litter_p_flux$Date))    
      treatDF$timepoint[treatDF$terms == "Understorey Litter P flux"] <- length(unique(understorey_litter_p_flux$Date))  
      treatDF$notes[treatDF$terms == "Understorey Litter P flux"] <- "Used Varsha's harvest data"
      
      ### Frass production flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Frass P flux", i+1] <- with(frass_p_production[frass_p_production$Ring ==i,],
                                                              mean(frass_p_flux_mg_m2_d)) 
      }
      treatDF$year_start[treatDF$terms == "Frass P flux"] <- min(year(frass_p_production$Date))    
      treatDF$year_end[treatDF$terms == "Frass P flux"] <- max(year(frass_p_production$Date))    
      treatDF$timepoint[treatDF$terms == "Frass P flux"] <- length(unique(frass_p_production$Date))  
      treatDF$notes[treatDF$terms == "Frass P flux"] <- "NA"
      
      ### Leaf litter flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Leaflitter P flux", i+1] <- with(leaflitter_p_flux[leaflitter_p_flux$Ring ==i,],
                                                                   mean(leaflitter_p_flux_mg_m2_d)) 
      }
      treatDF$year_start[treatDF$terms == "Leaflitter P flux"] <- min(year(leaflitter_p_flux$Date))    
      treatDF$year_end[treatDF$terms == "Leaflitter P flux"] <- max(year(leaflitter_p_flux$Date))    
      treatDF$timepoint[treatDF$terms == "Leaflitter P flux"] <- length(unique(leaflitter_p_flux$Date))  
      treatDF$notes[treatDF$terms == "Leaflitter P flux"] <- "Only leaves, exclude twig, barks and seeds"
      
      ### Fine Root litter flux
      # assume it's the same as fine root production flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Fineroot Litter P flux", i+1] <- with(fineroot_litter_p_flux[fineroot_litter_p_flux$Ring ==i,],
                                                                        mean(fineroot_litter_p_flux)) 
      }
      treatDF$year_start[treatDF$terms == "Fineroot Litter P flux"] <- min(year(fineroot_litter_p_flux$Date))    
      treatDF$year_end[treatDF$terms == "Fineroot Litter P flux"] <- max(year(fineroot_litter_p_flux$Date))    
      treatDF$timepoint[treatDF$terms == "Fineroot Litter P flux"] <- length(unique(fineroot_litter_p_flux$Date))  
      treatDF$notes[treatDF$terms == "Fineroot Litter P flux"] <- "Assuming fineroot production = fineroot litter production"
      
      
      ### twig litter flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Twig litter P flux", i+1] <- with(twig_litter_p_flux[twig_litter_p_flux$Ring ==i,],
                                                                    mean(twiglitter_p_flux_mg_m2_d)) 
      }
      treatDF$year_start[treatDF$terms == "Twig litter P flux"] <- min(year(twig_litter_p_flux$Date))    
      treatDF$year_end[treatDF$terms == "Twig litter P flux"] <- max(year(twig_litter_p_flux$Date))    
      treatDF$timepoint[treatDF$terms == "Twig litter P flux"] <- length(unique(twig_litter_p_flux$Date))  
      treatDF$notes[treatDF$terms == "Twig litter P flux"] <- "Assume wood P concentration"
      
      ### bark litter flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Bark litter P flux", i+1] <- with(bark_litter_p_flux[bark_litter_p_flux$Ring ==i,],
                                                                    mean(barklitter_p_flux_mg_m2_d))
      }
      treatDF$year_start[treatDF$terms == "Bark litter P flux"] <- min(year(bark_litter_p_flux$Date))    
      treatDF$year_end[treatDF$terms == "Bark litter P flux"] <- max(year(bark_litter_p_flux$Date))    
      treatDF$timepoint[treatDF$terms == "Bark litter P flux"] <- length(unique(bark_litter_p_flux$Date))  
      treatDF$notes[treatDF$terms == "Bark litter P flux"] <- "Assume wood P concentration"
      
      ### seed litter flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Seed litter P flux", i+1] <- with(seed_litter_p_flux[seed_litter_p_flux$Ring ==i,],
                                                                    mean(seedlitter_p_flux_mg_m2_d)) 
      }
      treatDF$year_start[treatDF$terms == "Seed litter P flux"] <- min(year(seed_litter_p_flux$Date))    
      treatDF$year_end[treatDF$terms == "Seed litter P flux"] <- max(year(seed_litter_p_flux$Date))    
      treatDF$timepoint[treatDF$terms == "Seed litter P flux"] <- length(unique(seed_litter_p_flux$Date))  
      treatDF$notes[treatDF$terms == "Seed litter P flux"] <- "Assume wood P concentration"
      
      ###  P mineralization flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Mineralization P flux 0-10cm", i+1] <- with(soil_p_mineralization[soil_p_mineralization$Ring ==i&soil_p_mineralization$Depth =="0_10",],
                                                                              mean(p_mineralization_mg_m2_d)) 
      }
      treatDF$year_start[treatDF$terms == "Mineralization P flux 0-10cm"] <- min(year(soil_p_mineralization$Date[soil_p_mineralization$Depth =="0_10"]))    
      treatDF$year_end[treatDF$terms == "Mineralization P flux 0-10cm"] <- max(year(soil_p_mineralization$Date[soil_p_mineralization$Depth =="0_10"]))    
      treatDF$timepoint[treatDF$terms == "Mineralization P flux 0-10cm"] <- length(unique(soil_p_mineralization$Date[soil_p_mineralization$Depth =="0_10"]))  
      treatDF$notes[treatDF$terms == "Mineralization P flux 0-10cm"] <- " 0-10cm"
      
      ###  P mineralization flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Mineralization P flux 10-30cm", i+1] <- with(soil_p_mineralization[soil_p_mineralization$Ring ==i&soil_p_mineralization$Depth =="10_30",],
                                                                               mean(p_mineralization_mg_m2_d))
      }
      treatDF$year_start[treatDF$terms == "Mineralization P flux 10-30cm"] <- min(year(soil_p_mineralization$Date[soil_p_mineralization$Depth =="10_30"]))    
      treatDF$year_end[treatDF$terms == "Mineralization P flux 10-30cm"] <- max(year(soil_p_mineralization$Date[soil_p_mineralization$Depth =="10_30"]))    
      treatDF$timepoint[treatDF$terms == "Mineralization P flux 10-30cm"] <- length(unique(soil_p_mineralization$Date[soil_p_mineralization$Depth =="10_30"]))  
      treatDF$notes[treatDF$terms == "Mineralization P flux 10-30cm"] <- " 10-30cm"
      
      ###  P mineralization flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Mineralization P flux 30-60cm", i+1] <- with(soil_p_mineralization[soil_p_mineralization$Ring ==i&soil_p_mineralization$Depth =="transition",],
                                                                               mean(p_mineralization_mg_m2_d))
      }
      treatDF$year_start[treatDF$terms == "Mineralization P flux 30-60cm"] <- min(year(soil_p_mineralization$Date[soil_p_mineralization$Depth =="transition"]))    
      treatDF$year_end[treatDF$terms == "Mineralization P flux 30-60cm"] <- max(year(soil_p_mineralization$Date[soil_p_mineralization$Depth =="transition"]))    
      treatDF$timepoint[treatDF$terms == "Mineralization P flux 30-60cm"] <- length(unique(soil_p_mineralization$Date[soil_p_mineralization$Depth =="transition"]))  
      treatDF$notes[treatDF$terms == "Mineralization P flux 30-60cm"] <- " 30-60cm"

      
      ###  P leaching flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Leaching P flux", i+1] <- with(soil_p_leaching[soil_p_leaching$Ring ==i,],
                                                                 mean(phosphate_leaching_flux)) 
      }
      treatDF$year_start[treatDF$terms == "Leaching P flux"] <- min(year(soil_p_leaching$Date))    
      treatDF$year_end[treatDF$terms == "Leaching P flux"] <- max(year(soil_p_leaching$Date))    
      treatDF$timepoint[treatDF$terms == "Leaching P flux"] <- length(unique(soil_p_leaching$Date))  
      treatDF$notes[treatDF$terms == "Leaching P flux"] <- "drainage 20 ml/d"
      
      
      
      ###  Canopy retrans P flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Canopy retrans P flux", i+1] <- with(canopy_P_retranslocation_flux[canopy_P_retranslocation_flux$Ring ==i,],
                                                                       sum(canopy_p_retrans_flux*Days)/sum(Days)) * conv
      }
      treatDF$year_start[treatDF$terms == "Canopy retrans P flux"] <- min(year(canopy_P_retranslocation_flux$Date))    
      treatDF$year_end[treatDF$terms == "Canopy retrans P flux"] <- max(year(canopy_P_retranslocation_flux$Date))    
      treatDF$timepoint[treatDF$terms == "Canopy retrans P flux"] <- length(unique(canopy_P_retranslocation_flux$Date))  
      treatDF$notes[treatDF$terms == "Canopy retrans P flux"] <- "calculated as diff of canopy production and litterfall"
      
      
      ###  Sapwood retrans P flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Sapwood retrans P flux", i+1] <- with(sapwood_P_retranslocation_flux[sapwood_P_retranslocation_flux$Ring ==i,],
                                                                        sum(sapwood_p_retrans_flux*Days)/sum(Days)) * conv
      }
      treatDF$year_start[treatDF$terms == "Sapwood retrans P flux"] <- min(year(sapwood_P_retranslocation_flux$Date))    
      treatDF$year_end[treatDF$terms == "Sapwood retrans P flux"] <- max(year(sapwood_P_retranslocation_flux$Date))    
      treatDF$timepoint[treatDF$terms == "Sapwood retrans P flux"] <- length(unique(sapwood_P_retranslocation_flux$Date))  
      treatDF$notes[treatDF$terms == "Sapwood retrans P flux"] <- "heartwood and sapwood P conc."
      
      ###  Coarseroot retrans P flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Coarseroot retrans P flux", i+1] <- with(coarseroot_P_retranslocation_flux[coarseroot_P_retranslocation_flux$Ring ==i,],
                                                                           sum(coarseroot_p_retrans_flux*Days)/sum(Days)) * conv
      }
      treatDF$year_start[treatDF$terms == "Coarseroot retrans P flux"] <- min(year(coarseroot_P_retranslocation_flux$Date))    
      treatDF$year_end[treatDF$terms == "Coarseroot retrans P flux"] <- max(year(coarseroot_P_retranslocation_flux$Date))    
      treatDF$timepoint[treatDF$terms == "Coarseroot retrans P flux"] <- length(unique(coarseroot_P_retranslocation_flux$Date))  
      treatDF$notes[treatDF$terms == "Coarseroot retrans P flux"] <- "heartwood and sapwood P conc."
      
      ###  Fineroot retrans P flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Fineroot retrans P flux", i+1] <- with(fineroot_P_retranslocation_flux[fineroot_P_retranslocation_flux$Ring ==i,],
                                                                         sum(fineroot_p_retrans_flux*Days)/sum(Days)) * conv
      }
      treatDF$year_start[treatDF$terms == "Fineroot retrans P flux"] <- min(year(fineroot_P_retranslocation_flux$Date))    
      treatDF$year_end[treatDF$terms == "Fineroot retrans P flux"] <- max(year(fineroot_P_retranslocation_flux$Date))    
      treatDF$timepoint[treatDF$terms == "Fineroot retrans P flux"] <- length(unique(fineroot_P_retranslocation_flux$Date))  
      treatDF$notes[treatDF$terms == "Fineroot retrans P flux"] <- "assumed"
      
      
      ###  Understorey retrans P flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Understorey retrans P flux", i+1] <- with(understorey_P_retranslocation_flux[understorey_P_retranslocation_flux$Ring ==i,],
                                                                            sum(understorey_p_retrans_flux*Days)/sum(Days)) * conv
      }
      treatDF$year_start[treatDF$terms == "Understorey retrans P flux"] <- min(year(understorey_P_retranslocation_flux$Date))    
      treatDF$year_end[treatDF$terms == "Understorey retrans P flux"] <- max(year(understorey_P_retranslocation_flux$Date))    
      treatDF$timepoint[treatDF$terms == "Understorey retrans P flux"] <- length(unique(understorey_P_retranslocation_flux$Date))  
      treatDF$notes[treatDF$terms == "Understorey retrans P flux"] <- "production - litterfall"
      
      
      
      ###  Total vegetation production P flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Total vegetation production P flux", i+1] <- sum(c(treatDF[treatDF$terms == "Canopy P flux", i+1],
                                                                                     treatDF[treatDF$terms == "Wood P flux", i+1],
                                                                                     treatDF[treatDF$terms == "Fine Root P flux", i+1],
                                                                                     treatDF[treatDF$terms == "Coarse Root P flux", i+1],
                                                                                     treatDF[treatDF$terms == "Twig litter P flux", i+1],
                                                                                     treatDF[treatDF$terms == "Bark litter P flux", i+1],
                                                                                     treatDF[treatDF$terms == "Seed litter P flux", i+1],
                                                                                     treatDF[treatDF$terms == "Frass P flux", i+1],
                                                                                     treatDF[treatDF$terms == "Understorey P flux", i+1]), na.rm=T)
      }
      
      
      ###  Total vegetation production P flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Total vegetation production P flux", i+1] <- sum(c(treatDF[treatDF$terms == "Canopy P flux", i+1],
                                                                                     treatDF[treatDF$terms == "Wood P flux", i+1],
                                                                                     treatDF[treatDF$terms == "Fine Root P flux", i+1],
                                                                                     treatDF[treatDF$terms == "Coarse Root P flux", i+1],
                                                                                     treatDF[treatDF$terms == "Twig P flux", i+1],
                                                                                     treatDF[treatDF$terms == "Bark P flux", i+1],
                                                                                     treatDF[treatDF$terms == "Seed P flux", i+1],
                                                                                     #treatDF[treatDF$terms == "Frass P flux", i+1],
                                                                                     treatDF[treatDF$terms == "Understorey P flux", i+1]), na.rm=T)
      }
      
      ###  Total vegetation retranslocation P flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Total vegetation retranslocation P flux", i+1] <- sum(c(treatDF[treatDF$terms == "Canopy retrans P flux", i+1],
                                                                                          treatDF[treatDF$terms == "Sapwood retrans P flux", i+1],
                                                                                          treatDF[treatDF$terms == "Fineroot retrans P flux", i+1],
                                                                                          treatDF[treatDF$terms == "Coarseroot retrans P flux", i+1],
                                                                                          treatDF[treatDF$terms == "Understorey retrans P flux", i+1]), na.rm=T)
      }
      
      ###  Total vegetation uptake P flux
      for (i in c(1:6)) {
        treatDF[treatDF$terms == "Total vegetation uptake P flux", i+1] <- treatDF[treatDF$terms == "Total vegetation production P flux", i+1] - treatDF[treatDF$terms == "Total vegetation retranslocation P flux", i+1]
      }
      
      
    }
    
    
    ### calculate treatment averages
    treatDF$aCO2 <- round(rowMeans(subset(treatDF, select=c(R2, R3, R6)), na.rm=T), 5)
    treatDF$eCO2 <- round(rowMeans(subset(treatDF, select=c(R1, R4, R5)), na.rm=T), 5)    
    
    treatDF$aCO2_sd <- rowSds(as.matrix(subset(treatDF, select=c(R2, R3, R6))), na.rm=T)
    treatDF$eCO2_sd <- rowSds(as.matrix(subset(treatDF, select=c(R1, R4, R5))), na.rm=T)
    
    ###### Diff (eCO2 - aCO2)
    treatDF$diff <- round(treatDF$eCO2 - treatDF$aCO2, 4)
    
    ### se of the diff
    treatDF$diff_se <- sqrt((treatDF$aCO2_sd^2+treatDF$eCO2_sd^2)/2) * (sqrt(2/3))
    
    ### confidence interval of the diff
    treatDF$diff_cf <- qt(0.975, 4) * treatDF$diff_se
    
    ###### percent differences (eCO2 - aCO2) / aCO2 * 100
    treatDF$percent_diff <- round((treatDF$eCO2 - treatDF$aCO2) / (treatDF$aCO2) * 100, 2)
    
    write.csv(treatDF, paste0("plots_tables/summary_tables/", norm, 
                              "/summary_table_P_flux_", norm, ".csv"), 
              row.names=F)
    
    
    ### plot
    tmpDF1 <- treatDF[,c("terms", "aCO2", "eCO2")]
    tmpDF2 <- treatDF[,c("terms", "aCO2_sd", "eCO2_sd")]
    
    plotDF1 <- reshape::melt(tmpDF1, id.var="terms")
    plotDF2 <- reshape::melt(tmpDF2, id.var="terms")
    colnames(plotDF2) <- c("terms", "variable", "value_sd")
    plotDF2$variable <- gsub("_sd", "", plotDF2$variable)
    
    plotDF <- merge(plotDF1, plotDF2, by=c("terms", "variable"))
    plotDF$terms <- gsub(" P flux", "", plotDF$terms)
    
    p1 <- ggplot(plotDF, aes(terms, value, group=variable))+
      geom_errorbar(aes(ymin=value-value_sd, ymax=value+value_sd),
                    position=position_dodge2(), width=0.3)+
      geom_point(aes(fill=variable), pch=21, stat = "identity", size=2,
                 position=position_dodge2(width=0.3)) +
      coord_flip()+
      scale_fill_manual(name="",
                        values=c("aCO2"="blue3",
                                 "eCO2"="red2"))
    
    pdf(paste0("plots_tables/summary_tables/", norm, "/P_flux_comparison.pdf"))
    plot(p1)
    dev.off()
    
    ##### output tables
    return(treatDF)
      
}

