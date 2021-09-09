make_understorey_pool_size_comparison <- function(inDF1, 
                                                  inDF2, 
                                                  plot.option) {
    
    ### calculate means across rings
    DF1 <- summaryBy(Live_g_C_m2+Dead_g_C_m2+Total_g_C_m2~Date, 
                     data=inDF1, FUN=mean, na.rm=T, keep.names=T)
    
    DF2 <- summaryBy(Live_g_C_m2+Dead_g_C_m2+Total_g_C_m2~Date, 
                     data=inDF2, FUN=mean, na.rm=T, keep.names=T)
    
    date.list <- unique(c(DF1$Date, DF2$Date))
    myDF <- data.frame(date.list, NA, NA)
    colnames(myDF) <- c("Date", "clipping", "camera")
    
    myDF2 <- myDF3 <- myDF
    
    ### total 
    for (i in DF1$Date) {
        myDF[myDF$Date == i, "clipping"] <- DF1[DF1$Date == i, "Total_g_C_m2"]
    }
    
    for (i in DF2$Date) {
        myDF[myDF$Date == i, "camera"] <- DF2[DF2$Date == i, "Total_g_C_m2"]
    }
    
    ### live
    for (i in DF1$Date) {
        myDF2[myDF2$Date == i, "clipping"] <- DF1[DF1$Date == i, "Live_g_C_m2"]
    }
    
    for (i in DF2$Date) {
        myDF2[myDF2$Date == i, "camera"] <- DF2[DF2$Date == i, "Live_g_C_m2"]
    }
    
    ### dead
    for (i in DF1$Date) {
        myDF3[myDF3$Date == i, "clipping"] <- DF1[DF1$Date == i, "Dead_g_C_m2"]
    }
    
    for (i in DF2$Date) {
        myDF3[myDF3$Date == i, "camera"] <- DF2[DF2$Date == i, "Dead_g_C_m2"]
    }
    
    plotDF1 <- melt(myDF, id.var="Date")
    plotDF2 <- melt(myDF2, id.var="Date")
    plotDF3 <- melt(myDF3, id.var="Date")
    
    if (plot.option == T) {
        
        p1 <- ggplot(plotDF1, aes(Date,value)) + 
            geom_point() + 
            stat_smooth() +
            facet_wrap(~variable)+
            ylab("Total (g C m-2)")
        
        p2 <- ggplot(plotDF2, aes(Date,value)) + 
            geom_point() + 
            stat_smooth() +
            facet_wrap(~variable)+
            ylab("Live (g C m-2)")
        
        
        p3 <- ggplot(plotDF3, aes(Date,value)) + 
            geom_point() + 
            stat_smooth() +
            facet_wrap(~variable)+
            ylab("Dead (g C m-2)")
        
        
        pdf("plots_tables/checks/understorey_pool_size_comparison.pdf", 
            width=8, height=4)
        
        plot(p1)
        plot(p2)
        plot(p3)
        
        dev.off()
        
    } 

}