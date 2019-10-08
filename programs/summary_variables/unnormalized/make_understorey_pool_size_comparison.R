make_understorey_pool_size_comparison <- function(inDF1, inDF2, plotting) {
    
    DF1 <- summaryBy(Total_g_C_m2~Date, data=inDF1, FUN=mean, na.rm=T, keep.names=T)
    DF2 <- summaryBy(Total_g_C_m2~Date, data=inDF2, FUN=mean, na.rm=T, keep.names=T)
    
    date.list <- unique(c(DF1$Date, DF2$Date))
    myDF <- data.frame(date.list, NA, NA)
    colnames(myDF) <- c("Date", "DF1", "DF2")
    
    for (i in DF1$Date) {
        myDF[myDF$Date == i, "DF1"] <- DF1[DF1$Date == i, "Total_g_C_m2"]
    }
    
    for (i in DF2$Date) {
        myDF[myDF$Date == i, "DF2"] <- DF2[DF2$Date == i, "Total_g_C_m2"]
    }
    
    plotDF <- melt(myDF, id.var="Date")
    
    if (plotting == T) {
        pdf("plots_tables/understorey_pool_size_comparison.pdf", width=8, height=4)
        
        p <- ggplot(plotDF, aes(Date,value)) + 
            geom_point() + 
            stat_smooth() +
            facet_wrap(~variable)
        
        plot(p)
        
        dev.off()
    } else (
        return(plotDF)
    )

}