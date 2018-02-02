#### Make the fine root litter P production
make_fineroot_litter_p_production <- function(p_conc,
                                              c_flux,
                                              p_retrans){
    
    ### prepare output df
    out <- c_flux
    
    ### prepare out df dates
    out$s.diff <- difftime(out$Start_date, "2010-01-01", units="days")
    out$e.diff <- difftime(out$End_date, "2010-01-01", units="days")
    p_conc$numd <- difftime(p_conc$Date, "2010-01-01", units="days")
    
    
    ### find the common month and year
    for (i in c(1:6)) {
        mydf1 <- subset(p_conc, Ring == i)
        
        for (j in mydf1$numd) {
            
            mydf2 <- subset(mydf1, numd == j)
            
            out[out$Ring == i & out$s.diff <= j & out$e.diff >= j, "PercP"] <- mydf2$PercP
            out[out$Ring == i & out$s.diff <= j & out$e.diff >= j, "date"] <- difftime(mydf2$Date, "2010-01-01", units="days")
            
        }
    }
    
    out$PercP <- out$PercP * (1 - p_retrans)
    
    outDF <- out[complete.cases(out),]
    
    # calculate litter p flux
    outDF$fineroot_litter_p_flux <- outDF$fineroot_production_flux / c_fraction_fr * outDF$PercP / 100
    
    outDF <- outDF[,c("Date", "Start_date", "End_date", "Ring", "fineroot_litter_p_flux", "Days")]
    
    return(outDF)
}