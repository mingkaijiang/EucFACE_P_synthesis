
make_wood_p_production <- function(p_conc, c_flux) {
    ### apply all concentration to different timepoints
    
    out <- merge(c_flux, p_conc, by="Ring")
    
    # calculate p flux
    out$wood_p_flux <- out$wood_production_flux / c_fraction * out$PercP / 100
    
    outDF <- out[complete.cases(out),]

    outDF <- outDF[, c("Date.x", "Start_date", "End_date", "Ring", "wood_p_flux", "Days")]
    names(outDF) <- c("Date", "Start_date", "End_date", "Ring", "wood_p_flux", "Days") 
    
    return(outDF)
}