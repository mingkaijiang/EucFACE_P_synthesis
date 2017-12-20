#- Make the retranslocation flux
make_leaf_p_retranslocation_flux <- function(){
 
    df <- read.csv("download/FACE_P0020_RA_leafP-Eter_20130201-20151115_L1.csv")
    
    ### setting up the date
    df$Date <- paste0("1-", as.character(df$Campaign))
    df$Date <- as.Date(df$Date, "%d-%b-%y")
    
    ### per ring leaf P % - litter
    df.litter <- subset(df, Type == "Leaf litter")
    
    ### Leaf litter p, average across rings, ignore dates, unit = %
    df.litter.p <- summaryBy(PercP~Ring,
                             data=df.litter,FUN=mean,keep.names=T,na.rm=T)
    
    ### per ring leaf P % - green
    df.green <- subset(df, Type == "green leaf")
    
    ### Leaf green p, average across rings, ignore dates, unit = %
    df.green.p <- summaryBy(PercP~Ring,
                             data=df.green,FUN=mean,keep.names=T,na.rm=T)
    
    ### per ring leaf P % - dead
    df.dead <- subset(df, Type == "sceneced leaf")
    
    ### Leaf dead p, average across rings, ignore dates, unit = %
    df.dead.p <- summaryBy(PercP~Ring,
                             data=df.dead,FUN=mean,keep.names=T,na.rm=T)
    
    ### compare P% across green, dead and litter leaves
    pdf("plots_tables/Leaf_P_concentration.pdf")
    require(ggplot2)
    plotDF <- rbind(df.green.p, df.dead.p, df.litter.p)
    plotDF$Category <- rep(c("green", "dead", "litter"), each = 6)
    p <- ggplot(plotDF, aes(Ring, PercP)) +   
           geom_bar(aes(fill = Category), position = "dodge", stat="identity") +
           xlab("Ring") + ylab("P concentration (%)") + 
           ggtitle("P concentration comparison across leaf tissues")
    plot(p)
    dev.off()
}


