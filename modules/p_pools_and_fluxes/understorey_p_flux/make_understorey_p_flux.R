#- Make the understorey P production flux
make_understorey_p_flux <- function(p_conc, c_flux, c_frac){
    
    ### obtaining month and year information 
    p_avg <- summaryBy(PercP~Ring, data=p_conc, FUN=mean, na.rm=T, keep.names=T)
    
    for (i in c(1:6)) {
        c_flux[c_flux$Ring == i, "PercP"] <- p_avg[p_avg$Ring == i, "PercP"]
    }
    
    ### calculate P flux mg P m-2 d-1
    c_flux$understorey_p_flux <- c_flux$understorey_production_flux / c_frac * c_flux$PercP / 100
    
    outDF <- c_flux[,c("Date", "Start_date", "End_date", "Ring", "understorey_p_flux", "Days")]
    
    
    return(outDF)
    
}
