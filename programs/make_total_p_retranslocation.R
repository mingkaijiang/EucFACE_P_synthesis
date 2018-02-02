make_total_p_retranslocation <- function(sumDF) {
    ### Calculate total p retranslocated
    ### canopy p - litterfall p + biomass P increment
    
    retranDF <- (sumDF[sumDF$terms == "Wood P flux", 2:9] + sumDF[sumDF$terms == "Canopy P flux", 2:9] - 
                     sumDF[sumDF$terms == "Leaflitter P flux", 2:9])
    
    retranDF <- as.data.frame(retranDF)
    
    return(retranDF)
}