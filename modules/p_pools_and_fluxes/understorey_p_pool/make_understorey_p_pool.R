#- Make the understorey P pool
make_understorey_p_pool <- function(p_conc, c_pool, c_frac){
    
    ### obtaining month and year information 
    p_avg <- summaryBy(PercP~Ring, data=p_conc, FUN=mean, na.rm=T, keep.names=T)
    
    for (i in c(1:6)) {
        c_pool[c_pool$Ring == i, "PercP"] <- p_avg[p_avg$Ring == i, "PercP"]
    }
    
    ### calculate P pool g P m-2
    c_pool$understorey_p_pool <- c_pool$Total_g_C_m2 / c_frac * c_pool$PercP / 100
    
    outDF <- c_pool[,c("Date", "Ring", "understorey_p_pool")]
    
    
    return(outDF)
    
}
