microbial_c_and_p_pool_comparison <- function (ppool) {
 
    
    # read in tmpDF
    # read in the Oct 2018 Johanna data at deeper depths
    tmpDF <- read.csv("temp_files/belowground_P_working_sheet.csv")
    tmpDF$Date <- as.Date(tmpDF$Date, format="%d/%m/%y")
    
    ### there are two NANs in the Cmic dataset, fill the gap
    ## multiple possible ways to fill the gap
    ## 1. taking the mean
    v <- mean(tmpDF$Cmic[tmpDF$Depth=="transition"], na.rm=T)
    #tmpDF$Cmic[tmpDF$Depth=="transition"&tmpDF$Ring%in%c(6,2)] <- v
    
    ## 2. calculating reduction coefficient
    v2 <- sum(tmpDF$Cmic[tmpDF$Ring%in%c(1,3,4,5)&tmpDF$Depth=="transition"])/sum(tmpDF$Cmic[tmpDF$Ring%in%c(1,3,4,5)&tmpDF$Depth=="10_30"])
    
    tmpDF$Cmic[tmpDF$Depth=="transition"&tmpDF$Ring%in%c(6,2)] <- v2 * tmpDF$Cmic[tmpDF$Depth=="10_30"&tmpDF$Ring%in%c(6,2)]
    
    
    p1 <- ggplot(tmpDF, aes(Cmic, Pmic)) +
        geom_point(aes(fill=Depth, col=Depth,pch=Trt), size=2)
    
    plot(p1)
    
    
    ### calculate microbial CP ratio
    tmpDF$CPratio <- with(tmpDF, Cmic/Pmic)
    
    ### summary
    test <- summaryBy(CPratio+Pmic+Cmic~Trt+Depth, data=tmpDF, FUN=c(mean,sd),
                      na.rm=T, keep.names=T)
       
    
    p1 <- ggplot(test, 
                 aes(Depth, Cmic.mean, group=Trt)) + 
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
        geom_errorbar(aes(ymax=Cmic.mean+Cmic.sd, ymin=Cmic.mean-Cmic.sd), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        xlab("") + #ylab(expression("Microbial C pool (g C " * m^-2 * ")"))+
        ylab(expression("Microbial C (mg C " * kg^-1 * ")"))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_x_discrete(limits=c("0_10", "10_30", "transition"),
                         labels=c("0_10"="0-10cm", 
                                  "10_30"="10-30cm",
                                  "transition"="30-60cm"))+
        scale_fill_manual(name="", 
                          values=c("Ambient"=set3Palette[1],
                                   "Elevated"=set3Palette[3]),
                          labels=c("Ambient"=expression(aCO[2]), 
                                   "Elevated"=expression(eCO[2])))
    
    
    p2 <- ggplot(test, 
                 aes(Depth, Pmic.mean, group=Trt)) + 
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
        geom_errorbar(aes(ymax=Pmic.mean+Pmic.sd, ymin=Pmic.mean-Pmic.sd), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        xlab("") + #ylab(expression("Microbial C pool (g C " * m^-2 * ")"))+
        ylab(expression("Microbial P (mg C " * kg^-1 * ")"))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_x_discrete(limits=c("0_10", "10_30", "transition"),
                         labels=c("0_10"="0-10cm", 
                                  "10_30"="10-30cm",
                                  "transition"="30-60cm"))+
        scale_fill_manual(name="", 
                          values=c("Ambient"=set3Palette[1],
                                   "Elevated"=set3Palette[3]),
                          labels=c("Ambient"=expression(aCO[2]), 
                                   "Elevated"=expression(eCO[2])))
    
    
    p3 <- ggplot(test, 
                 aes(Depth, CPratio.mean, group=Trt)) + 
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
        geom_errorbar(aes(ymax=CPratio.mean+CPratio.sd, ymin=CPratio.mean-CPratio.sd), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        xlab("") + #ylab(expression("Microbial C pool (g C " * m^-2 * ")"))+
        ylab("CP ratio")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_x_discrete(limits=c("0_10", "10_30", "transition"),
                         labels=c("0_10"="0-10cm", 
                                  "10_30"="10-30cm",
                                  "transition"="30-60cm"))+
        scale_fill_manual(name="", 
                          values=c("Ambient"=set3Palette[1],
                                   "Elevated"=set3Palette[3]),
                          labels=c("Ambient"=expression(aCO[2]), 
                                   "Elevated"=expression(eCO[2])))
    
    
    plot(p3)
    
    grid.labs <- c("(a)", "(b)", "(c)")
    
    legend_bot_row <- get_legend(p1 + theme(legend.position="bottom",
                                            legend.box = 'horizontal',
                                            legend.box.just = 'left'))
    
    pdf("plots_tables/output/unnormalized/soil microbial CP.pdf", width=10, height=6)
    top_row <- plot_grid(p1, p2, p3, ncol=3)
    plot_grid(top_row, 
              legend_bot_row,
              ncol = 1, 
              rel_heights=c(1, 0.1))
    grid.text(grid.labs,
              x = c(0.21, 0.47, 0.72), 
              y = c(0.98, 0.98, 0.98),
              gp=gpar(fontsize=16, col="black", fontface="bold"))    
    dev.off()
    
    
    
    
}