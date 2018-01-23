
#### To make EucFACE P summary table b CO2 treatment
#### Ignore time and ring variability

make_summary_table_by_treatment <- function() {
    
    ### Define concentration variable names
    conc.terms <- c("Wood P Conc", "Canopy P Conc", "Fine Root P Conc",
                    "Leaflitter P Conc","Understorey P Conc", "Frass P Conc",
                    "Microbial P Conc", "Soil P Conc", "Mycorrhizal P Conc")
    
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
    out <- summaryBy(Fineroot_P_concentration~Ring,data=fineroot_P_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Fine Root P Conc", 2:7] <- out$Fineroot_P_concentration
    treatDF$year_start[treatDF$conc.terms == "Fine Root P Conc"] <- min(year(fineroot_P_concentration$Date))    
    treatDF$year_end[treatDF$conc.terms == "Fine Root P Conc"] <- max(year(fineroot_P_concentration$Date))    
    treatDF$timepoint[treatDF$conc.terms == "Fine Root P Conc"] <- length(unique(fineroot_P_concentration$Date))  
    treatDF$notes[treatDF$conc.terms == "Fine Root P Conc"] <- "Averaged over depths of 0-10cm and 10-30cm"

#    
#    ##### output tables
#    return(list(inout = data.table(inout), 
#                npp = data.table(npp), 
#                pool = data.table(pool)))
#    
}

