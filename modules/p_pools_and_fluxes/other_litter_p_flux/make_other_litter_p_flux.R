make_other_litter_p_flux <- function(p_conc) {
    
    ### Other litter flux in unit of mg m-2 d-1 of biomass, not C!!!
    litter_flux <- make_other_litter_flux()
    
    ### prepare output df
    out <- litter_flux
    
    ### prepare out df dates
    for (i in c(1:6)) {
        out[out$Ring == i, "PercP"] <- p_conc[p_conc$Ring == i, "PercP"]
    }
    
    outDF <- out[complete.cases(out),]
    
    ### calculate other litter P flux mg P m-2 d-1
    outDF$other_litter_p_flux_mg_m2_d <- outDF$other_flux * outDF$PercP / 100
    
    outDF$Days <- as.numeric(with(outDF, End_date - Start_date))
    
    outDF <- outDF[,c("Date", "Start_date", "End_date", "Ring", "other_litter_p_flux_mg_m2_d", "Days")]
    
    return(outDF)
}