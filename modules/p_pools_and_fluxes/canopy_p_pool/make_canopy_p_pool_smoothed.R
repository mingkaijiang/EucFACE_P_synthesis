#- Make the canopy P pool 
make_canopy_p_pool_smoothed <- function(biom) {
    ### return ring-specific canopy P data (mg/kg)
    ### p_conc: P concentration in %
    ### biom: canopy biomass pool (dry weight)
    
    ### read in biomass data
    biom$leaf_pool <- biom$leaf_pool / c_fraction
    biom$dLEAF <- biom$dLEAF / c_fraction
    
    ### read in leaf litter data
    
    
    
    ### read in P concentration dataset
    df <- read.csv("temp_files/GreenLeaves_allP-clean_Mingkai.csv")
    
    ### setting up the date
    df$Date <- paste0("01-", as.character(df$Campaign))
    df$Date <- as.Date(df$Date, "%d-%b-%y")
    
    p_conc <- summaryBy(Perc.P~Ring+AGE,
                     data=df,FUN=mean,keep.names=T,na.rm=T)
    p_conc$month <- month(p_conc$Date)
    p_conc$year <- year(p_conc$Date)
    
    colnames(p_conc) <- c("Ring", "Date", "Age", "PercP", "month", "year")
    
    
    ### obtaining month and year information 
    biom$month <- month(biom$Date)
    biom$year <- year(biom$Date)
    

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


