make_belowground_standing_p_stock_normalized <- function(froot, croot) {
    ### summary dfs by year & ring
    froot.y <- summaryBy(predicted~Ring, data=froot, FUN=mean, na.rm=T, keep.names=T)
    croot.y <- summaryBy(predicted~Ring, data=croot, FUN=mean, na.rm=T, keep.names=T)
    
    ### compute annual averages for each pool and ring
    out <- cbind(froot.y, croot.y$predicted)
    colnames(out) <- c("Ring", "froot", "croot")
    
    ### Calculate total
    out$total <- with(out, froot+croot)
    
    
    return(out)
}