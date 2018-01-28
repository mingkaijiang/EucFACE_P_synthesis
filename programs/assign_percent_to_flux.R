assign_percent_to_flux <- function(input1, input2) {
    
    ### input1 <- leaf % p
    ### input2 <- flux

    out <- input2
    out$s.diff <- difftime(out$Start_date, "2010-01-01", units="days")
    out$e.diff <- difftime(out$End_date, "2010-01-01", units="days")
    input1$numd <- difftime(input1$Date, "2010-01-01", units="days")
    
    
    ### find the common month and year
    for (i in c(1:6)) {
        mydf1 <- subset(input1, Ring == i)

        for (j in mydf1$numd) {
            
            mydf2 <- subset(mydf1, numd == j)
            
            out[out$Ring == i & out$s.diff <= j & out$e.diff >= j, "percentP"] <- mydf2$PercP
            out[out$Ring == i & out$s.diff <= j & out$e.diff >= j, "date"] <- difftime(mydf2$Date, "2010-01-01", units="days")
            
        }
    }
    
    ### complete cases
    outDF <- out[complete.cases(out),]
    outDF$Date <- as.Date(outDF$date, origin = "2010-01-01")
    
    ### reduce number of variables
    outDF2 <- outDF[,c("Date", "Ring", "Start_date", "End_date", "leaf_flux", "percentP")]
    
    ### convert percent p to mg P m-2 d-1
    outDF2$leaf_litter_p_flux <- outDF2$leaf_flux * outDF2$percentP / 100
    
    
    return(outDF2)
}