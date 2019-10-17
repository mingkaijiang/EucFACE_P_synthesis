make_overstorey_standing_p_stock_normalized <- function(leaf, wood) {
    ### summary dfs by year & ring
    leaf.y <- summaryBy(predicted~Ring, data=leaf, FUN=mean, na.rm=T, keep.names=T)
    wood.y <- summaryBy(predicted~Ring, data=wood, FUN=mean, na.rm=T, keep.names=T)
    
    ### compute annual averages for each pool and ring
    out <- cbind(leaf.y, wood.y$predicted)
    colnames(out) <- c("Ring", "leaf", "wood")
    
    ### Calculate total
    out$total <- with(out, leaf+wood)
    
    
    return(out)
}