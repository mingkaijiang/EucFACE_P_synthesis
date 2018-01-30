
make_wood_p_production <- function(p_conc, c_flux) {
    ### apply all concentration to different timepoints
    
    out <- merge(c_flux, p_conc, by="Ring")
    
    # calculate p flux
    out$wood_p_flux <- out$wood_production_flux / c_fraction * out$PercP / 100
    
    outDF <- out[complete.cases(out),]
    
    outDF <- outDF[, c("Date.x", "Ring", "wood_p_flux")]
    names(outDF) <- c("Date", "Ring", "wood_p_flux") 
    
    return(outDF)
}