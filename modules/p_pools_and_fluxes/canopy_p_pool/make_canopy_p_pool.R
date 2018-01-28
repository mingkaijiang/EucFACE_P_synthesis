#- Make the canopy P pool 
make_canopy_p_pool <- function(p_conc,
                               biom) {
    ### return ring-specific canopy P data (mg/kg)
    ### p_conc: P concentration in %
    ### biom: canopy biomass pool (dry weight)
    

    ### obtaining month and year information 
    p_conc$month <- month(p_conc$Date)
    p_conc$year <- year(p_conc$Date)

    ### obtaining month and year information 
    biom$month <- month(biom$Date)
    biom$year <- year(biom$Date)
    
    ### prepare output df
    out <- biom

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

    outDF <- out[complete.cases(out),]
    

    ### calculate leaf P pool g P m-2
    outDF$leaf_p_pool <- outDF$leaf_pool*outDF$PercP/100
    
    outDF <- outDF[,c("Date", "Ring", "leaf_p_pool")]

    return(outDF)

}


