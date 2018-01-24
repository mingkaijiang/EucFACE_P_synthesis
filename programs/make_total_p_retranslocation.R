make_total_p_retranslocation <- function(sumDF) {
    ### Calculate total p retranslocated
    ### canopy p - litterfall p
    
    ### need to calculate total leaf litter at an annual basis
    ### then multiple by the leaflitter P concentration
    ### for now, use hardwired leaflitter data
    nppDF <- matrix(nrow=1,ncol=2)
    nppDF <- as.data.frame(nppDF)
    colnames(nppDF) <- c("aCO2", "eCO2")
    pconDF <- nppDF
    ppoolDF <- nppDF
    
    ### assign values
    nppDF$aCO2 <- 161.58
    nppDF$eCO2 <- 141.41
    
    pconDF$aCO2 <- sumDF$aCO2[sumDF$conc.terms == "Leaflitter P Conc"]
    pconDF$eCO2 <- sumDF$eCO2[sumDF$conc.terms == "Leaflitter P Conc"]
    
    ### Calculate P in pool leaf litter
    ppoolDF$aCO2 <- (pconDF$aCO2 * nppDF$aCO2) / 100 
    ppoolDF$eCO2 <- (pconDF$eCO2 * nppDF$eCO2) / 100 
    
    ### read in leaf P data and calculate the difference
    canDF <- summaryBy(leaf_p_pool~Ring,
                       data=canopy_p_pool$greenleaf,FUN=mean,keep.names=T,na.rm=T) 
    
    aCO2 <- mean(as.numeric(canDF[2,2]), as.numeric(canDF[3,2]), as.numeric(canDF[6,2]))
    eCO2 <- mean(as.numeric(canDF[1,2]), as.numeric(canDF[4,2]), as.numeric(canDF[5,2]))
    
    ### Calculate p retranslocated
    pretDF <- ppoolDF
    pretDF$aCO2 <- aCO2 - ppoolDF$aCO2
    pretDF$eCO2 <- eCO2 - ppoolDF$eCO2
    
    return(pretDF)
}