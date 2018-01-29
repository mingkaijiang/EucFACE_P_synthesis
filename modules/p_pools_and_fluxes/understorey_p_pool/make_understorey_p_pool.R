#- Make the understorey P pool
make_understorey_p_pool <- function(p_conc, c_pool){
    
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
    
    outDF <- out[complete.cases(out),]
    
    
    ### calculate leaf P pool g P m-2
    outDF$understorey_p_pool <- outDF$Total_g_C_m2 / c_fraction_ud * outDF$PercP / 100
    
    outDF <- outDF[,c("Date", "Ring", "understorey_p_pool")]
    
    
    return(outDF)
    
}
