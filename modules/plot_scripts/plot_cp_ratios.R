plot_cp_ratios <- function(inDF) {
    
    
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
    
    plotDF3 <- myDF2[myDF2$Variable%in%c("soil", "microbe")]
    
    
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
    
    
    p3 <- ggplot(plotDF3, aes(x=Variable, y=value.mean))+
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
        scale_x_discrete(limits=c("microbe", "soil"),
                         labels=c("microbe", "soil"))+
        scale_fill_manual(name="", values = c("amb" = SpectralPalette[7], "ele" = SpectralPalette[3]),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("amb" = "black", "ele" = "black"),
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