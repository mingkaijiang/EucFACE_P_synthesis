##### Make the canopy P production
make_canopy_p_production <- function(p_conc,
                                     c_flux,
                                     c_frac){
    
    ### prepare output df
    out <- c_flux
    
    ### averaging p concentration for each ring
    p_avg <- summaryBy(PercP~Ring, data=p_conc, FUN=mean, keep.names=T, na.rm=T)
    
    ### find the common month and year
    for (i in c(1:6)) {
        out[out$Ring == i, "PercP"] <- p_avg[p_avg$Ring == i, "PercP"]
    }
    
    outDF <- out
    
    # calculate p flux
    outDF$canopy_p_flux <- outDF$total_flux / c_frac * outDF$PercP / 100
    outDF$leaf_p_flux <- outDF$leaf_flux / c_frac * outDF$PercP / 100
    outDF$herbivory_p_flux <- outDF$herbivory_leaf_consumption_flux / c_frac * outDF$PercP / 100
    
    
    outDF <- outDF[,c("Date", "Start_date", "End_date", "Ring", "canopy_p_flux",
                      "leaf_p_flux", "herbivory_p_flux",
                      "Days")]
    
    return(outDF)
}