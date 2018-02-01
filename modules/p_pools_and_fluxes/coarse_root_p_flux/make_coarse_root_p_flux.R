
# Make the coarse root P flux
make_coarse_root_p_flux <- function(p_conc, c_flux, c_frac){

    for (i in c(1:6)) {
        c_flux[c_flux$Ring == i, "PercP"] <- p_conc[p_conc$Ring == i, "PercP"]
    }
    
    ### calculate P flux mg P m-2
    c_flux$coarse_root_p_flux <- c_flux$coarse_root_production_flux / c_frac * c_flux$PercP / 100
    
    outDF <- c_flux[,c("Start_date", "End_date", "Date", "Ring", "coarse_root_p_flux", "Days")]
    
    
    
    return(outDF)
    
}
