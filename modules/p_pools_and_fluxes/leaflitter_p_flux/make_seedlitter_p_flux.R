make_seedlitter_p_flux <- function(p_conc, litter_flux) {
    

    ### prepare output df
    out <- litter_flux
    
    ### prepare out df dates
    out$s.diff <- difftime(out$Start_date, "2010-01-01", units="days")
    out$e.diff <- difftime(out$End_date, "2010-01-01", units="days")
    p_conc$numd <- difftime(p_conc$Date, "2010-01-01", units="days")
    
    
    
    ### find the common month and year
    for (i in c(1:6)) {
        out[out$Ring == i, "PercP"] <- p_conc$PercP[p_conc$Ring==i]
    }
    
    outDF <- out[complete.cases(out),]
    
    ### calculate twiglitter P flux mg P m-2 d-1
    outDF$seedlitter_p_flux_mg_m2_d <- outDF$seed_flux*outDF$PercP/100
    
    outDF$Days <- as.numeric(with(outDF, End_date - Start_date))
    
    outDF <- outDF[,c("Date", "Start_date", "End_date", "Ring", "seedlitter_p_flux_mg_m2_d", "Days")]
    
    return(outDF)
}