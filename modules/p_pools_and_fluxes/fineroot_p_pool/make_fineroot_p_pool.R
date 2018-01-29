#### Make the fine root P pool
make_fineroot_p_pool <- function(p_conc,
                                 c_pool){
    
    ### obtaining month and year information 
    p_conc$month <- month(p_conc$Date)
    p_conc$year <- year(p_conc$Date)
    
    ### obtaining month and year information 
    c_pool$month <- month(c_pool$Date)
    c_pool$year <- year(c_pool$Date)
    
    ### prepare output df
    out <- c_pool
    
    ### find the common month and year 
    for (i in c(1:6)) {
        mydf1 <- subset(p_conc, Ring == i)
        
        for (j in unique(mydf1$year)) {
            mydf2 <- subset(mydf1, year == j)
            
            for (k in unique(mydf2$month)) {
                mydf3 <- subset(mydf2, month == k)
                
                out[out$Ring == i & out$year == j & out$month == k, "PercP"] <- mydf3$PercP
            }
        }
    }
    
    # calculate p pool
    out$fineroot_p_pool <- out$fineroot_pool / c_fraction_fr * out$PercP / 100
    
    outDF <- out[complete.cases(out),]
    
    outDF <- outDF[, c("Date", "Ring", "fineroot_p_pool")]
        
    return(outDF)
}