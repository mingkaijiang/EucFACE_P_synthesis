plot_cp_ratios <- function(inDF,
                           ppool) {
    
    
    ### calculate original CP ratio under aCO2
    ### calculate original CP ratio under eCO2
    ### calculate new CP ratio under aCO2, this is the 10% scenario
    
    
    ### reshape
    reDF1 <- melt(setDT(inDF), id.vars = c("Ring"), variable.name = "Variable")
    
    ### add treatment
    reDF1$Trt <- "amb"
    reDF1$Trt[reDF1$Ring%in%c(1,4,5)] <- "ele"
    

    myDF <- reDF1
    myDF2 <- summaryBy(value~Variable+Trt, FUN=c(mean,sd),
                       data=myDF, na.rm=T, keep.names=T)
    
    ### ignore soil, litter
    plotDF1 <- myDF2[myDF2$Variable%in%c("canopy", "leaflitter",
                                         "fineroot", "understorey",
                                        "understorey_litter", "frass")]
    
    plotDF2 <- myDF2[myDF2$Variable%in%c("wood", "sapwood",
                                         "heartwood")]
    
    
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
    
    
    ### calculate microbial CP ratio
    tmpDF$CPratio <- with(tmpDF, Cmic/Pmic)
    
    ### summary
    plotDF3 <- summaryBy(CPratio+Pmic+Cmic~Trt+Depth, data=tmpDF, FUN=c(mean,sd),
                      na.rm=T, keep.names=T)
    
    
    ###plot the three, check statistical significance
    p1 <- ggplot(plotDF1, aes(x=Variable, y=value.mean))+
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge")+
        geom_errorbar(aes(ymax=value.mean+value.sd, ymin=value.mean-value.sd, 
                          color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        labs(x="", y="CP ratio")+
        theme_linedraw() +
        #ylim(0,0.15)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_x_discrete(limits=c("canopy", "leaflitter", "fineroot",
                                  "understorey", "understorey_litter", "frass"),
                         labels=c("canopy", "canopy leaf litter", "fineroot",
                                  "understorey", "understorey litter", "frass"))+
        scale_fill_manual(name="", values = c("amb" = SpectralPalette[7], "ele" = SpectralPalette[3]),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("amb" = "black", "ele" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))
    
    
    p2 <- ggplot(plotDF2, aes(x=Variable, y=value.mean))+
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge")+
        geom_errorbar(aes(ymax=value.mean+value.sd, ymin=value.mean-value.sd, 
                          color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        labs(x="", y="CP ratio")+
        theme_linedraw() +
        #ylim(0,0.15)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_x_discrete(limits=c("wood", "sapwood", "heartwood"),
                         labels=c("wood", "sapwood", "heartwood"))+
        scale_fill_manual(name="", values = c("amb" = SpectralPalette[7], "ele" = SpectralPalette[3]),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("amb" = "black", "ele" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))
    
    
    p3 <- ggplot(plotDF3, aes(x=Depth, y=CPratio.mean))+
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge")+
        geom_errorbar(aes(ymax=CPratio.mean+CPratio.sd, ymin=CPratio.mean-CPratio.sd, 
                          color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        labs(x="", y="CP ratio")+
        theme_linedraw() +
        #ylim(0,0.15)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_x_discrete(limits=c("0_10", "10_30", "transition"),
                         labels=c("microbe 0-10cm", "microbe 10-30cm",
                                  "microbe 30-60cm"))+
        scale_fill_manual(name="", values = c("Ambient" = SpectralPalette[7], "Elevated" = SpectralPalette[3]),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("Ambient" = "black", "Elevated" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))
    
    
    
    require(grid)
    require(cowplot)
    
    grid.labs <- c("(a)", "(b)", "(c)")
    
    
    pdf(paste0("plots_tables/output/unnormalized/CP_ratio.pdf"),
        width=10,height=6)
    bot_row <- plot_grid(p2, p3, ncol=2, rel_widths = c(1., 1.))
    plot_grid(p1, bot_row,  ncol = 1,
              rel_heights=c(1, 1))
    grid.text(grid.labs,
              x = c(0.1, 0.1, 0.6), y = c(0.95, 0.45, 0.45),
              gp=gpar(fontsize=16, col="black", fontface="bold"))
    dev.off()
    
}