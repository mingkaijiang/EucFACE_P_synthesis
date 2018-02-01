##### Make the canopy P production
#make_canopy_p_production <- function(p_conc,
#                                     c_flux,
#                                     c_frac){
#    
#    ### prepare output df
#    out <- c_flux
#    
#    ### prepare out df dates
#    out$s.diff <- difftime(out$Start_date, "2010-01-01", units="days")
#    out$e.diff <- difftime(out$End_date, "2010-01-01", units="days")
#    p_conc$numd <- difftime(p_conc$Date, "2010-01-01", units="days")
#    
#    
#    ### find the common month and year
#    for (i in c(1:6)) {
#        mydf1 <- subset(p_conc, Ring == i)
#        
#        for (j in mydf1$numd) {
#            
#            mydf2 <- subset(mydf1, numd == j)
#            
#            out[out$Ring == i & out$s.diff <= j & out$e.diff >= j, "PercP"] <- mydf2$PercP
#            out[out$Ring == i & out$s.diff <= j & out$e.diff >= j, "date"] <- difftime(mydf2$Date, "2010-01-01", units="days")
#            
#        }
#    }
#    
#    outDF <- out[complete.cases(out),]
#    
#    # calculate p flux
#    outDF$canopy_p_flux <- outDF$leaf_flux / c_frac * outDF$PercP / 100
#    
#    outDF <- outDF[,c("Date", "Start_date", "End_date", "Ring", "canopy_p_flux", "Days")]
#    
#    return(outDF)
#}


make_canopy_p_production <- function(p_conc,
                                     c_flux,
                                     c_frac){
    
    ### prepare output df
    out <- c_flux
    
    ### averaging p concentration for each ring
    p_avg <- summaryBy(PercP~Ring, data=p_conc, FUN=mean, keep.names=T, na.rm=T)
    
    ### find the common month and year
    for (i in c(1:6)) {
        out[out$Ring == i, "PercP"] <- p_avg[p_avg$Ring == i, "PercP"]
    }
    
    outDF <- out
    
    # calculate p flux
    outDF$canopy_p_flux <- outDF$leaf_flux / c_frac * outDF$PercP / 100
    
    outDF <- outDF[,c("Date", "Start_date", "End_date", "Ring", "canopy_p_flux", "Days")]
    
    return(outDF)
}