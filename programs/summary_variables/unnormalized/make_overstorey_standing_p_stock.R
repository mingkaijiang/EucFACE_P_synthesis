make_overstorey_standing_p_stock <- function(leaf, wood) {
    ### summary dfs by year & ring
    leaf.y <- summaryBy(leaf_p_pool~Ring, data=leaf, FUN=mean, na.rm=T, keep.names=T)
    wood.y <- summaryBy(wood_p_pool~Ring, data=wood, FUN=mean, na.rm=T, keep.names=T)
    
    ### compute annual averages for each pool and ring
    out <- cbind(leaf.y, wood.y$wood_p_pool)
    colnames(out) <- c("Ring", "leaf", "wood")
    
    ### Calculate total
    out$total <- with(out, leaf+wood)
    
    
    return(out)
}