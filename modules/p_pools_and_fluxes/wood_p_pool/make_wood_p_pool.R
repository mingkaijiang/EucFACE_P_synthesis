
make_wood_p_pool <- function(p_conc, c_pool, case_consideration) {
    ### apply all concentration to different timepoints
    
    out <- merge(c_pool, p_conc, by="Ring")
    
    # calculate p pool
    if (case_consideration == "total") {
        ### data from Kristine: sapwood P conc: 0.13 mg P g-1, heartwood P conc: 0.04 mg P g-1
        out$wood_p_pool <- (out$sap_pool / c_fraction * out$PercP / 100) + (out$heart_pool / c_fraction * 0.004 / 100)
        
        #out$wood_p_pool <- (out$wood_pool / c_fraction * out$PercP / 100) 
    } else if (case_consideration == "sapwood") {
        out$wood_p_pool <- out$sap_pool / c_fraction * out$PercP / 100
    } else if (case_consideration == "heartwood") {
        out$wood_p_pool <- (out$heart_pool / c_fraction * 0.004 / 100)
    }

    outDF <- out[complete.cases(out),]
    
    outDF <- outDF[, c("Date.x", "Ring", "wood_p_pool")]
    names(outDF) <- c("Date", "Ring", "wood_p_pool") 
    
    return(outDF)
}