make_total_p_retranslocation <- function(understorey_retrans_coef, sumDF) {
    ### Calculate total p retranslocated
    ### canopy, wood, fine root and understorey

    df1 <- sumDF[sumDF$term == "Wood P flux", 2:9] + sumDF[sumDF$term == "Canopy P flux", 2:9] +
        sumDF[sumDF$term == "Fine Root P flux", 2:9]
    df2 <- df1 - sumDF[sumDF$term == "Leaflitter P flux", 2:9] - sumDF[sumDF$term == "Fineroot Litter P flux", 2:9]
    df3 <- df2 + understorey_retrans_coef * sumDF[sumDF$term == "Understorey P flux", 2:9]
    
    retranDF <- df3
    
    retranDF <- as.data.frame(retranDF)
    
    return(retranDF)
}
