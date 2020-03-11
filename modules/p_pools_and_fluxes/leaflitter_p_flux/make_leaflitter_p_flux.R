make_leaflitter_p_flux <- function(p_conc, c_flux,c_frac) {
    
    
    ### prepare output df
    out <- c_flux
    
    ### average P concentration
    p_avg <- summaryBy(PercP~Ring, data=p_conc, FUN=mean, keep.names=T, na.rm=T)
    
    ### find the common month and year
    for (i in c(1:6)) {
        out[out$Ring == i, "PercP"] <- p_avg[p_avg$Ring == i, "PercP"]
    }
    
    outDF <- out
    
    ### calculate leaflitter P flux mg P m-2 d-1
    outDF$leaflitter_p_flux_mg_m2_d <- outDF$leaf_flux/c_frac*outDF$PercP/100
    
    outDF$Days <- as.numeric(with(outDF, End_date - Start_date))
    
    outDF <- outDF[,c("Date", "Start_date", "End_date", "Ring", "leaflitter_p_flux_mg_m2_d", "Days")]
    
    return(outDF)
}