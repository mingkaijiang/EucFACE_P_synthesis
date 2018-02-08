
make_wood_p_pool <- function(p_conc, c_pool, case_consideration) {
    ### apply all concentration to different timepoints
    
    out <- merge(c_pool, p_conc, by="Ring")
    
    # calculate p pool
    if (case_consideration == "total") {
        out$wood_p_pool <- out$wood_pool / c_fraction * out$PercP / 100
    } else if (case_consideration == "sapwood") {
        out$wood_p_pool <- out$sap_pool / c_fraction * out$PercP / 100
    }

    outDF <- out[complete.cases(out),]
    
    outDF <- outDF[, c("Date.x", "Ring", "wood_p_pool")]
    names(outDF) <- c("Date", "Ring", "wood_p_pool") 
    
    return(outDF)
}