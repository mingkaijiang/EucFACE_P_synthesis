make_total_p_retranslocation <- function(under_retrans_calc_method, understorey_retrans_coef, sumDF) {
    ### Calculate total p retranslocated
    ### canopy, wood, fine root and understorey

    df1 <- sumDF[sumDF$term == "Wood P flux", 2:9] + sumDF[sumDF$term == "Canopy P flux", 2:9] +
        sumDF[sumDF$term == "Fine Root P flux", 2:9]
    df2 <- df1 - sumDF[sumDF$term == "Leaflitter P flux", 2:9] - sumDF[sumDF$term == "Fineroot Litter P flux", 2:9]
    
    if (under_retrans_calc_method == "Simple") {
        df3 <- df2 + understorey_retrans_coef * sumDF[sumDF$term == "Understorey P flux", 2:9]
        retranDF <- df3
        
    } else if (under_retrans_calc_method == "Mortality") {
        mortality <- summaryBy((1-percent_live)~Ring, data=understorey_live_percent, 
                               FUN=mean, keep.names=T)
        colnames(mortality) <- c("Ring", "percent_dead")
        
        for (i in 1:6) {
            mortality[mortality$Ring == i, "P_flux"] <- sumDF[sumDF$term == "Understorey P flux", i+1] 
        }
        
        mortality$retrans_flux <- with(mortality, percent_dead * P_flux)
        
        out <- t(mortality$retrans_flux)
        colnames(out) <- c("R1", "R2", "R3", "R4", "R5", "R6")
        out <- as.data.frame(out)
        out$aCO2 <- round(rowMeans(data.frame(out$R2, out$R3, out$R6)), 4)
        out$eCO2 <- round(rowMeans(data.frame(out$R1, out$R4, out$R5)) , 4)
        
        df3 <- df2 + out
        retranDF <- df3
    }


    retranDF <- as.data.frame(retranDF)
    
    return(retranDF)
}
