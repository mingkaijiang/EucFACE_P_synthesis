#- Make the understorey P pool
make_understorey_p_pool <- function(p_conc, p_lit_conc,
                                    c_pool, c_frac,
                                    live_or_total){
    
    ### obtaining month and year information 
    p_avg <- summaryBy(PercP~Ring, data=p_conc, FUN=mean, na.rm=T, keep.names=T)
    
    for (i in c(1:6)) {
        c_pool[c_pool$Ring == i, "PercP"] <- p_avg[p_avg$Ring == i, "PercP"]
        c_pool[c_pool$Ring == i, "LitPercP"] <- p_lit_conc[p_lit_conc$Ring == i, "PercP"]
        
    }
    
    if (live_or_total == "Live") {
        ### calculate P pool g P m-2
        c_pool$understorey_p_pool <- c_pool$Live_g_C_m2 / c_frac * c_pool$PercP / 100
    } else if (live_or_total == "Total") {
        ### calculate P pool g P m-2
        c_pool$live_p_pool <- c_pool$Live_g_C_m2 / c_frac * c_pool$PercP / 100
        c_pool$dead_p_pool <- c_pool$Dead_g_C_m2 / c_frac * c_pool$LitPercP / 100
        
    }

    c_pool$understorey_p_pool <- c_pool$live_p_pool + c_pool$dead_p_pool
    
    outDF <- c_pool[,c("Date", "Ring", "understorey_p_pool", "live_p_pool", "dead_p_pool")]
    
    
    return(outDF)
    
}
