assign_percent_to_pool <- function(input1, input2, input3, colname1, colname2) {
    
    ### input1 <- leaf % c
    ### input2 <- leaf % p
    ### input3 <- leaf_c_pool
    
    out <- input3
    
    ### find the common month and year for dead leaf
    for (i in c(1:6)) {
        mydf1 <- subset(input1, Ring == i)
        mydf2 <- subset(input2, Ring == i)
        
        for (j in mydf1$year) {
            
            mydf3 <- subset(mydf1, year == j)
            mydf4 <- subset(mydf2, year == j)
            
            for (k in mydf3$month) {
                mydf5 <- subset(mydf3, month == k)
                mydf6 <- subset(mydf4, month == k)
                
                out[out$Ring == i & out$year == j & out$month == k, colname1] <- mydf5$PercC
                out[out$Ring == i & out$year == j & out$month == k, colname2] <- mydf6$PercP
            }
        }
    }
    ### complete cases
    outDF <- out[complete.cases(out),]
    
    return(outDF)
}