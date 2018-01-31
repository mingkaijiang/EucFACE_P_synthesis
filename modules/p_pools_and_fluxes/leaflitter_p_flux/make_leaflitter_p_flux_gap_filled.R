make_leaflitter_p_flux_gap_fill <- function(p_conc) {
    
    ### Leaf litter flux in unit of mg m-2 d-1 of leaf, not C!!!
    litter_flux <- make_leaflitter_flux()
    
    ### prepare output df
    out <- litter_flux
    
    ### prepare out df dates
    out$s.diff <- difftime(out$Start_date, "2010-01-01", units="days")
    out$e.diff <- difftime(out$End_date, "2010-01-01", units="days")
    p_conc$numd <- difftime(p_conc$Date, "2010-01-01", units="days")
    
    
    ### find the common month and year
    for (i in c(1:6)) {
        mydf1 <- subset(p_conc, Ring == i)
        
        for (j in mydf1$numd) {
            
            mydf2 <- subset(mydf1, numd == j)
            
            out[out$Ring == i & out$s.diff <= j & out$e.diff >= j, "PercP"] <- mydf2$PercP
            out[out$Ring == i & out$s.diff <= j & out$e.diff >= j, "date"] <- difftime(mydf2$Date, "2010-01-01", units="days")
            
        }
    }
    
    ### Calculate ring-average P conc values
    percP <- summaryBy(PercP~Ring, data=p_conc, FUN=mean, na.rm=T, keep.names=T)
    
    ### Fill the gap PercP by using the average values
    for (i in c(1:6)) {
        out[out$Ring == i & is.na(out$PercP), "PercP"] <- percP$PercP[i]
    }
    
    outDF <- out
    
    ### calculate leaflitter P flux mg P m-2 d-1
    outDF$leaflitter_p_flux_mg_m2_d <- outDF$leaf_flux*outDF$PercP/100
    
    outDF$Days <- as.numeric(with(outDF, End_date - Start_date))
    
    outDF <- outDF[,c("Date", "Start_date", "End_date", "Ring", "leaflitter_p_flux_mg_m2_d", "Days")]
    
    return(outDF)
}