make_vegetation_standing_p_stock <- function(norm, 
                                             leaf, 
                                             wood, 
                                             sapwood,
                                             understorey, 
                                             fineroot, 
                                             coarseroot,
                                             dead,
                                             forestfloor) {
    
    ### ignores bark and twigs
    
    ### summary dfs by year & ring
    leaf.y <- summaryBy(leaf_p_pool~Ring, data=leaf, FUN=mean, na.rm=T, keep.names=T)
    wood.y <- summaryBy(wood_p_pool~Ring, data=wood, FUN=mean, na.rm=T, keep.names=T)
    froot.y <- summaryBy(fineroot_p_pool~Ring, data=fineroot, FUN=mean, na.rm=T, keep.names=T)
    croot.y <- summaryBy(coarse_root_p_pool~Ring, data=coarseroot, FUN=mean, na.rm=T, keep.names=T)
    ua.y <- summaryBy(understorey_p_pool~Ring, data=understorey, FUN=mean, na.rm=T, keep.names=T)
    dead.y <- summaryBy(wood_p_pool~Ring, data=dead, FUN=mean, na.rm=T, keep.names=T)
    forestfloor.y <- summaryBy(leaflitter_p_pool~Ring, data=forestfloor, FUN=mean, na.rm=T, keep.names=T)
    sapwood.y <- summaryBy(wood_p_pool~Ring, data=sapwood, FUN=mean, na.rm=T, keep.names=T)
    
    
    ### compute annual averages for each pool and ring
    out <- cbind(leaf.y, 
                 wood.y$wood_p_pool,
                 sapwood.y$wood_p_pool,
                 froot.y$fineroot_p_pool,
                 croot.y$coarse_root_p_pool,
                 ua.y$understorey_p_pool,
                 dead.y$wood_p_pool,
                 forestfloor.y$leaflitter_p_pool)
    colnames(out) <- c("Ring", "leaf", "wood", "sapwood",
                       "fineroot", "coarseroot", "understorey",
                       "dead", "forestfloor")
    
    ### Calculate total
    out$total <- with(out, (leaf+wood+fineroot+coarseroot+understorey+dead+forestfloor))
    
    ### calculate oa
    out$oa <- with(out, (leaf+wood))
    
    ### calculate belowground
    out$belowground <- with(out, (fineroot+coarseroot))
    
    ### caclulate total dead
    out$litter <- with(out, (forestfloor+dead))
    
    
    ### assign aCO2 and eCO2
    out$Trt <- c("eCO2", "aCO2", "aCO2", "eCO2", "eCO2", "aCO2")
    
    return(out)
}