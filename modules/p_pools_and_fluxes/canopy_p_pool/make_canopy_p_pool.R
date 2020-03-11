#- Make the canopy P pool 
make_canopy_p_pool <- function(biom, p_conc) {
    ### return ring-specific canopy P data (mg/kg)
    ### p_conc: P concentration in %
    ### biom: canopy biomass pool (dry weight)
    
    ### read in P concentration dataset
    p_conc.m <- summaryBy(PercP~Ring,
                     data=p_conc,FUN=mean,keep.names=T,na.rm=T)
    
    ### prepare output df
    out <- biom
    out$leaf_pool <- out$leaf_pool / c_fraction

    ### assign values
    for (i in c(1:6)) {
       
        out[out$Ring == i, "PercP"] <- p_conc.m[p_conc.m$Ring==i, "PercP"]
        
    }

    ### calculate leaf P pool g P m-2
    out$leaf_p_pool <- out$leaf_pool*out$PercP/100
    
    out <- out[,c("Date", "Ring", "leaf_p_pool")]

    return(out)

}


