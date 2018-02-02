make_total_p_retranslocation <- function(sumDF) {
    ### Calculate total p retranslocated
    ### canopy p - litterfall p + biomass P increment
    
    df1 <- sumDF[sumDF$term == "Wood P flux", 2:9] + sumDF[sumDF$term == "Canopy P flux", 2:9]
    df2 <- df1 - sumDF[sumDF$term == "Leaflitter P flux", 2:9]
    
    retranDF <- df2
    
    retranDF <- as.data.frame(retranDF)
    
    return(retranDF)
}