make_understorey_production_flux_comparison <- function(inDF1, 
                                                        inDF2, 
                                                        plot.option) {
    
    ### calculate means across rings
    DF1 <- inDF1[,c("Date", "Ring", "leaf_flux")]
    
    DF2 <- inDF2[,c("Date", "Ring", "leaf_flux")]
    
    myDF <- merge(DF1, DF2, by=c("Date", "Ring"), all=T)
    colnames(myDF) <- c("Date", "Ring", "litterfall", "litterfall_dLeaf")
    
    plotDF <- melt(myDF, id.var=c("Date", "Ring"))
    
    if (plot.option == T) {
        
        p1 <- ggplot(plotDF, aes(Date, value, group=Ring)) + 
            geom_point() + 
            stat_smooth() +
            facet_wrap(~variable)+
            ylab("Canopy C production flux (mg C m-2 d-1)")
        
        
        pdf("plots_tables/checks/canopy_production_flux_comparison.pdf", 
            width=8, height=4)
        
        plot(p1)
        
        dev.off()
        
    } 

}