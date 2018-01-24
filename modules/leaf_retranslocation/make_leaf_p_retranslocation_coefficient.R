#- Make the retranslocation coefficient
make_leaf_p_retranslocation_coefficient <- function(){
 
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
    require(ggplot2)
    
    pdf("plots_tables/Leaf_P_concentration.pdf")
    plotDF <- rbind(df.green.p, df.dead.p, df.litter.p)
    plotDF$Category <- rep(c("green", "dead", "litter"), each = 6)
    p <- ggplot(plotDF, aes(Ring, PercP)) +   
           geom_bar(aes(fill = Category), position = "dodge", stat="identity") +
           xlab("Ring") + ylab("P concentration (%)") + 
           ggtitle("P concentration comparison across leaf tissues")
    plot(p)
    dev.off()
    
    ### calculate leaf P retranslocation rate based on dead and green leaf
    retransDF <- cbind(df.green.p, df.dead.p$PercP)
    colnames(retransDF) <- c("Ring", "green", "dead")
    retransDF$percent_diff <- retransDF$green - retransDF$dead
    retransDF$retrans_coef <- retransDF$percent_diff/retransDF$green
    
    ### Plot eCO2 effect on retranslocation coefficient
    retransDF$CO2 <- c("eCO2", "aCO2", "aCO2", "eCO2", "eCO2", "aCO2")
    
    pdf("plots_tables/CO2_effect_on_P_retranslocation_coefficient.pdf")
    p <- ggplot(retransDF, aes(CO2, retrans_coef)) +   
        geom_boxplot() +
        xlab("Ring") + ylab("Leaf P retranslocation coefficient (%)") + 
        ggtitle("CO2_effect_on_P_retranslocation_coefficient")
    plot(p)
    dev.off()
    
    outDF <- retransDF[,c("Ring", "retrans_coef", "CO2")]

    return(outDF)
    
}


