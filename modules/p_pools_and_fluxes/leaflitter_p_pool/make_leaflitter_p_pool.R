make_leaflitter_p_pool <- function(p_conc, c_pool, c_frac) {

    ### convert c_pool to biomass
    c_pool$leaflitter_biomass <- c_pool$leaflitter_pool / c_frac
    
    c_pool$Year <- year(c_pool$Date)
    p_conc$Year <- year(p_conc$Date)
    
    ### assign P concentration
    for (i in 1:6) {
        for (j in 2013:2016) {
            c_pool[c_pool$Ring==i&c_pool$Year==j, "p_conc"] <- p_conc[p_conc$Ring==i&p_conc$Year==j, "PercP"]
        }
    }
    
    ### calculate p pool
    c_pool$leaflitter_p_pool <- c_pool$p_conc / 100 * c_pool$leaflitter_biomass
    
    out <- c_pool[,c("Date", "Ring", "leaflitter_p_pool")]

    return(out)

}


