make_overstorey_p_retranslocation <- function(sumDF) {
    ### Calculate overstorey p retranslocated
    ### canopy p - litterfall p + wood P increment + fineroot P - fineroot litter P
    
    df1 <- sumDF[sumDF$term == "Wood P flux", 2:9] + sumDF[sumDF$term == "Canopy P flux", 2:9] +
        sumDF[sumDF$term == "Fine Root P flux", 2:9]
    df2 <- df1 - sumDF[sumDF$term == "Leaflitter P flux", 2:9] - sumDF[sumDF$term == "Fineroot Litter P flux", 2:9]

    retranDF <- df2
    
    retranDF <- as.data.frame(retranDF)
    
    return(retranDF)
}
