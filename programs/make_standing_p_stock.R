make_standing_p_stock <- function(leaf, wood, froot, croot) {
    ### make standing p stock per year
    ### only include years with all 4 pools have data
    
    ### assign year to dfs
    leaf$year <- year(leaf$Date)
    wood$year <- year(wood$Date)
    froot$year <- year(froot$Date)
    croot$year <- year(croot$Date)
    
    ### check min and max years
    min.year <- c(min(leaf$year), min(wood$year), min(froot$year), min(croot$year))
    max.year <- c(max(leaf$year), max(wood$year), max(froot$year), max(croot$year))
    s.year <- max(min.year)
    e.year <- min(max.year)
    
    ### create an output df to store all the pools and rings
    yr.range <- c(s.year:e.year)
    ring <- c(1:6)
    out <- matrix(NA, nrow = (length(ring) * length(yr.range)), ncol = 6)
    out <- as.data.frame(out)
    colnames(out) <- c("Ring", "Year", "leaf", "wood", "froot", "croot")
    out$Ring <- rep(ring, by=2)
    out$Year <- rep(yr.range, each=6)
    
    ### summary dfs by year & ring
    leaf.y <- summaryBy(leaf_p_pool~Ring+year, data=leaf, FUN=mean, na.rm=T, keep.names=T)
    wood.y <- summaryBy(wood_p_pool~Ring+year, data=wood, FUN=mean, na.rm=T, keep.names=T)
    froot.y <- summaryBy(fineroot_p_pool~Ring+year, data=froot, FUN=mean, na.rm=T, keep.names=T)
    croot.y <- summaryBy(coarse_root_p_pool~Ring+year, data=croot, FUN=mean, na.rm=T, keep.names=T)
    
    ### compute annual averages for each pool and ring
    for (i in ring) {
        for (j in yr.range) {
            out[out$Ring == i & out$Year == j, "leaf"] <- leaf.y[leaf.y$Ring == i & leaf.y$year == j, "leaf_p_pool"]
            out[out$Ring == i & out$Year == j, "wood"] <- wood.y[wood.y$Ring == i & wood.y$year == j, "wood_p_pool"]
            out[out$Ring == i & out$Year == j, "froot"] <- froot.y[froot.y$Ring == i & froot.y$year == j, "fineroot_p_pool"]
            out[out$Ring == i & out$Year == j, "croot"] <- croot.y[croot.y$Ring == i & croot.y$year == j, "coarse_root_p_pool"]
        }
    }
    
    ### Calculate total
    out$total <- with(out, leaf+wood+froot+croot)
    
    return(out)
}