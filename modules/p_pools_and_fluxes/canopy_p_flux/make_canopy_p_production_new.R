##### Make the canopy P production
make_canopy_p_production_new <- function(c_flux,
                                         c_frac){
    
    ### read in P concentration dataset
    ### this is an old file, don't use!
    df <- read.csv("temp_files/GreenLeaves_allP-clean_Mingkai.csv")
    
    ### setting up the date
    df$Date <- paste0("01-", as.character(df$Campaign))
    df$Date <- as.Date(df$Date, "%d-%b-%y")
    
    p_conc <- summaryBy(Perc.P~Ring+AGE,
                        data=df,FUN=mean,keep.names=T,na.rm=T)
    
    colnames(p_conc) <- c("Ring", "Age", "PercP")
        
        
    ### prepare output df
    out <- c_flux
    
    for (i in 1:6) {
        out[out$Ring==i, "PercP"] <- p_conc[p_conc$Ring==i & p_conc$Age=="old", "PercP"]
    }
    
    
    # calculate p flux
    out$canopy_p_flux <- out$leaf_flux / c_frac * out$PercP / 100
    
    out <- out[,c("Date", "Start_date", "End_date", "Ring", "canopy_p_flux", "Days")]
    
    out <- out[complete.cases(out$canopy_p_flux),]
    
    return(out)
}