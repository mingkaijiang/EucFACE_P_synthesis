calculate_plant_p_retranslocation_coefficients <- function (canopy_p_concentration,
                                                            leaflitter_p_concentration,
                                                            understorey_p_concentration,
                                                            understorey_litter_p_concentration,
                                                            sapwood_p_concentration,
                                                            fineroot_retranslocation_coefficient,
                                                            plot.option = T) {
    
    #### 2.1 retranslocation coefficients
    ### canopy leaf n retranslocation coefficient
    leaf_p_retrans_coefficient <- make_canopy_leaf_p_retranslocation_coefficient(df1=canopy_p_concentration,
                                                                                 df2=leaflitter_p_concentration)
    
    ### 2.2 understorey leaf p retranslocation coefficient
    understorey_p_retrans_coefficient <- make_understorey_p_retranslocation_coefficient(df1=understorey_p_concentration,
                                                                                        df2=understorey_litter_p_concentration)
    
    ### 2.3 fineroot retrans
    ### assumed value
    fineroot_p_retrans_coefficient <- make_fineroot_p_retrans_coefficient(retrans=fineroot_retranslocation_coefficient)
    
    ### 2.4 wood retrans
    wood_p_retrans_coefficient <- make_stem_p_retrans_coefficient(sapwood=sapwood_p_concentration)
    
    ### 2.5 coarseroot retrans
    coarseroot_p_retrans_coefficient <- make_stem_p_retrans_coefficient(sapwood=sapwood_p_concentration)
    
    
    ### merge all
    outDF <- merge(leaf_p_retrans_coefficient, understorey_p_retrans_coefficient, by="Ring")
    colnames(outDF) <- c("Ring", "canopy", "understorey")
    
    outDF <- merge(outDF, fineroot_p_retrans_coefficient, by="Ring")
    colnames(outDF) <- c("Ring", "canopy", "understorey", "fineroot")
    
    outDF <- merge(outDF, wood_p_retrans_coefficient, by="Ring")
    colnames(outDF) <- c("Ring", "canopy", "understorey", "fineroot", "sapwood")
    
    outDF <- merge(outDF, coarseroot_p_retrans_coefficient, by="Ring")
    colnames(outDF) <- c("Ring", "canopy", "understorey", "fineroot", "sapwood", "coarseroot")
    
    
    ### ploting script
    if (plot.option == T) {
        
        plotDF <- melt(outDF, id.vars = c("Ring"), variable.name = "component")
        
        plotDF$Ring <- as.character(plotDF$Ring)
        plotDF$Trt[plotDF$Ring%in%c("1","4","5")] <- "ele"
        plotDF$Trt[plotDF$Ring%in%c("2","3","6")] <- "amb"
        
        plotDF2 <- summaryBy(value~Trt+component, FUN=c(mean,sd),
                             data=plotDF,
                             na.rm=T, keep.names=T)
        
        p1 <- ggplot(plotDF, aes(x=component, y=value, group=Ring))+
            geom_bar(aes(fill=Ring), position="dodge", stat="identity") 
        
        p2 <- ggplot(plotDF2, aes(x=component, y=value.mean, group=Trt))+
            geom_bar(aes(fill=Trt), position="dodge", stat="identity")+
            geom_errorbar(aes(ymin=value.mean-value.sd, ymax=value.mean+value.sd),
                          position="dodge", stat="identity")
        
        pdf("plots_tables/checks/retranslocation_coefficients.pdf")
        plot(p1)
        plot(p2)
        dev.off()
        
    }
    
    
    return(outDF)
    
}