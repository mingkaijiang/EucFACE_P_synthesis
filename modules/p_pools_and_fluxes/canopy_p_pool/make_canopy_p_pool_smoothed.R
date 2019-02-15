#- Make the canopy P pool 
make_canopy_p_pool_smoothed <- function(biom) {
    ### return ring-specific canopy P data (mg/kg)
    ### p_conc: P concentration in %
    ### biom: canopy biomass pool (dry weight)
    
    ### read in biomass data
    biom$leaf_pool <- biom$leaf_pool / c_fraction
    biom$new_pool <- (biom$dLEAF + biom$leaflit) / c_fraction
    
    ### calculate old leaf pool
    biom$old_pool <- biom$leaf_pool - biom$new_pool
    
    ### complete cases
    biom <- biom[complete.cases(biom$dLEAF),]
    
    ### read in P concentration dataset
    df <- read.csv("temp_files/GreenLeaves_allP-clean_Mingkai.csv")
    
    ### setting up the date
    df$Date <- paste0("01-", as.character(df$Campaign))
    df$Date <- as.Date(df$Date, "%d-%b-%y")
    
    p_conc <- summaryBy(Perc.P~Ring+AGE,
                     data=df,FUN=mean,keep.names=T,na.rm=T)
    
    colnames(p_conc) <- c("Ring", "Age", "PercP")
    
    ## prepare output
    out <- biom
    
    for (i in 1:6) {
        out[out$Ring==i, "old_p_pool"] <- out[out$Ring==i, "old_pool"] * p_conc[p_conc$Ring==i & p_conc$Age=="old", "PercP"] / 100
        out[out$Ring==i, "new_p_pool"] <- out[out$Ring==i, "new_pool"] * p_conc[p_conc$Ring==i & p_conc$Age=="new", "PercP"] / 100
    }
    
    out$leaf_p_pool <- out$old_p_pool + out$new_p_pool
    

    outDF <- out[,c("Date", "Ring", "leaf_p_pool", "old_p_pool", "new_p_pool")]

    return(outDF)

}


