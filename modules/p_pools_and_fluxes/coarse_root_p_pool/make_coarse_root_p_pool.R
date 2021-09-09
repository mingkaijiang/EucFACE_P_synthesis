
# Make the coarse root P pool
make_coarse_root_p_pool <- function(p_conc, c_pool, c_frac){

    for (i in c(1:6)) {
        c_pool[c_pool$Ring == i, "PercP"] <- p_conc[p_conc$Ring == i, "PercP"]
    }
    
    #c_pool$PercP <- 0.004
    
    ### calculate P pool g P m-2
    c_pool$coarse_root_p_pool <- c_pool$coarse_root_pool / c_frac * c_pool$PercP / 100
    
    outDF <- c_pool[,c("Date", "Ring", "coarse_root_p_pool")]
    
    return(outDF)
    
}
