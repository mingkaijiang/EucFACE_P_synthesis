make_fineroot_p_retrans_coefficient <- function(retrans) {
    ### assumed to be x %
    
    
    out <- data.frame(c(1:6), retrans)
    
    colnames(out) <- c("Ring", "retrans_coef")
    
    return(out)
    
}