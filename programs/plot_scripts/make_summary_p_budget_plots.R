make_summary_p_budget_plots <- function(inDF) {
    
    ################### Plot all P summary budget plots
    ### Plot 1df
    plotDF1 <- data.frame(c(inDF$aCO2[inDF$terms=="total standing p stock"], 
                            inDF$eCO2[inDF$terms=="total standing p stock"]), 
                          NA)
    colnames(plotDF1) <- c("mean", "sd")
    plotDF1$sd <- c(inDF$aCO2_sd[inDF$terms=="total standing p stock"], 
                    inDF$eCO2_sd[inDF$terms=="total standing p stock"])
    plotDF1$Trt <- c("aCO2", "eCO2")
    plotDF1$pos <- with(plotDF1, mean + sd)
    plotDF1$neg <- with(plotDF1, mean - sd)
    
    ### Plot 2
    plotDF2 <- data.frame(c(inDF$aCO2[inDF$terms=="Stand P OA"], 
                            inDF$eCO2[inDF$terms=="Stand P OA"],
                            inDF$aCO2[inDF$terms=="Stand P UA"], 
                            inDF$eCO2[inDF$terms=="Stand P UA"],
                            inDF$aCO2[inDF$terms=="Stand P Belowground"], 
                            inDF$eCO2[inDF$terms=="Stand P Belowground"]), 
                          NA)
    colnames(plotDF2) <- c("mean", "sd")
    plotDF2$sd <- c(inDF$aCO2_sd[inDF$terms=="Stand P OA"], 
                    inDF$eCO2_sd[inDF$terms=="Stand P OA"],
                    inDF$aCO2_sd[inDF$terms=="Stand P UA"], 
                    inDF$eCO2_sd[inDF$terms=="Stand P UA"],
                    inDF$aCO2_sd[inDF$terms=="Stand P Belowground"], 
                    inDF$eCO2_sd[inDF$terms=="Stand P Belowground"])
    plotDF2$Trt <- rep(c("aCO2", "eCO2"), 3)
    plotDF2$Variable <- rep(c("OA", "UA", "B"), each=2)
    plotDF2$pos <- with(plotDF2, mean + sd)
    plotDF2$neg <- with(plotDF2, mean - sd)
    aC <- sum(plotDF2$mean[plotDF2$Trt=="aCO2"])
    eC <- sum(plotDF2$mean[plotDF2$Trt=="eCO2"])
    
    plotDF2$prop[plotDF2$Trt=="aCO2"] <- plotDF2$mean[plotDF2$Trt=="aCO2"] / aC * 100
    plotDF2$prop[plotDF2$Trt=="eCO2"] <- plotDF2$mean[plotDF2$Trt=="eCO2"] / eC * 100
    
    
    ### Plot 3
    plotDF3 <- data.frame(c(inDF$aCO2[inDF$terms=="Total P requirement"], 
                            inDF$eCO2[inDF$terms=="Total P requirement"]), 
                          NA)
    colnames(plotDF3) <- c("mean", "sd")
    plotDF3$sd <- c(inDF$aCO2_sd[inDF$terms=="Total P requirement"], 
                    inDF$eCO2_sd[inDF$terms=="Total P requirement"])
    plotDF3$Trt <- c("aCO2", "eCO2")
    plotDF3$pos <- with(plotDF3, mean + sd)
    plotDF3$neg <- with(plotDF3, mean - sd)
    
    ### Plot 4
    plotDF4 <- data.frame(c(inDF$aCO2[inDF$terms=="Total P retranslocation"], 
                            inDF$eCO2[inDF$terms=="Total P retranslocation"]), 
                          NA)
    colnames(plotDF4) <- c("mean", "sd")
    plotDF4$sd <- c(inDF$aCO2_sd[inDF$terms=="Total P retranslocation"], 
                    inDF$eCO2_sd[inDF$terms=="Total P retranslocation"])
    plotDF4$Trt <- c("aCO2", "eCO2")
    plotDF4$pos <- with(plotDF4, mean + sd)
    plotDF4$neg <- with(plotDF4, mean - sd)
    
    ### Plot 5
    plotDF5 <- data.frame(c(inDF$aCO2[inDF$terms=="Total P uptake"], 
                            inDF$eCO2[inDF$terms=="Total P uptake"]), 
                          NA)
    colnames(plotDF5) <- c("mean", "sd")
    plotDF5$sd <- c(inDF$aCO2_sd[inDF$terms=="Total P uptake"], 
                    inDF$eCO2_sd[inDF$terms=="Total P uptake"])
    plotDF5$Trt <- c("aCO2", "eCO2")
    plotDF5$pos <- with(plotDF5, mean + sd)
    plotDF5$neg <- with(plotDF5, mean - sd)
    
    ### Plot 6
    plotDF6 <- data.frame(c(inDF$aCO2[inDF$terms=="Soil P mineralization"], 
                            inDF$eCO2[inDF$terms=="Soil P mineralization"]), 
                          NA)
    colnames(plotDF6) <- c("mean", "sd")
    plotDF6$sd <- c(inDF$aCO2_sd[inDF$terms=="Soil P mineralization"], 
                    inDF$eCO2_sd[inDF$terms=="Soil P mineralization"])
    plotDF6$Trt <- c("aCO2", "eCO2")
    plotDF6$pos <- with(plotDF6, mean + sd)
    plotDF6$neg <- with(plotDF6, mean - sd)
    
    ### Plotting
    
    p1 <- ggplot(plotDF1,
                 aes(Trt, mean)) + 
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        xlab("") + ylab(expression(paste("Standing P stock (g P ", m^-2, ")")))+
        theme_linedraw() +
        ylim(0, 2.0)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="bottom")+
        scale_fill_manual(name="", values = c("aCO2" = "blue2", "eCO2" = "red3"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_x_discrete(limits=c("aCO2","eCO2"),
                         labels=c(expression(aCO[2]),
                         expression(eCO[2])))
    
    p2 <- ggplot(plotDF2, aes(x="", y=prop, fill=Variable))+
        geom_bar(width = 1, stat = "identity") +
        coord_polar(theta="y") +
        facet_grid(facets=. ~ Trt) +
        theme_minimal()+
        theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(),
            panel.grid=element_blank(),
            axis.ticks = element_blank(),
            plot.title=element_text(size=14, face="bold"),
            legend.position="bottom")+
        scale_fill_manual(name="Component", values = c("UA" = "green", "OA" = "darkgreen", "B" = "orange"),
                          labels=c("B","OA", "UA"))
    
    p3 <- ggplot(plotDF3,
                 aes(Trt, mean)) + 
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        xlab("") + ylab(expression(paste("P requirement (g P ", m^-2, " ", yr^-1, ")")))+
        theme_linedraw() +
        ylim(0, 1.5)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", values = c("aCO2" = "blue2", "eCO2" = "red3"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_x_discrete(limits=c("aCO2","eCO2"),
                         labels=c(expression(aCO[2]),
                                  expression(eCO[2])))
    
    p4 <- ggplot(plotDF4,
                 aes(Trt, mean)) + 
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        xlab("") + ylab(expression(paste("P retranslocation (g P ", m^-2, " ", yr^-1, ")")))+
        theme_linedraw() +
        ylim(0, 1.5)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", values = c("aCO2" = "blue2", "eCO2" = "red3"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_x_discrete(limits=c("aCO2","eCO2"),
                         labels=c(expression(aCO[2]),
                                  expression(eCO[2])))
    
    p5 <- ggplot(plotDF5,
                 aes(Trt, mean)) + 
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        xlab("") + ylab(expression(paste("P uptake (g P ", m^-2, " ", yr^-1, ")")))+
        theme_linedraw() +
        ylim(0, 1.5)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", values = c("aCO2" = "blue2", "eCO2" = "red3"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_x_discrete(limits=c("aCO2","eCO2"),
                         labels=c(expression(aCO[2]),
                                  expression(eCO[2])))
    
  
    p6 <- ggplot(plotDF6,
                 aes(Trt, mean)) + 
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        xlab("") + ylab(expression(paste("Soil P mineralization (g P ", m^-2, " ", yr^-1, ")")))+
        theme_linedraw() +
        ylim(0, 1.5)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", values = c("aCO2" = "blue2", "eCO2" = "red3"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_x_discrete(limits=c("aCO2","eCO2"),
                         labels=c(expression(aCO[2]),
                                  expression(eCO[2])))
    

    grid.labs <- c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)")
    
    require(grid)
    require(cowplot)
    
    ## plot 
    pdf("plots_tables/Summary_P_Budget_Plots.pdf", width=8,height=8)
    plot_grid(p3, p4, p5, p6, p1, p2, labels="", ncol=2, align="v", axis = "l",
              rel_heights = c(1, 1, 1.2))
    grid.text(grid.labs, x = c(0.11, 0.60, 0.11, 0.60, 0.11, 0.60),
              y = c(0.95, 0.95, 0.65, 0.65, 0.34, 0.34), 
              gp=gpar(fontsize=14, col="black", fontface="bold"))
    dev.off()
    
    
    
    ### Plot 
    plotDF1 <- data.frame(c(inDF$aCO2[inDF$terms=="MRT"], 
                            inDF$eCO2[inDF$terms=="MRT"]), 
                          NA)
    colnames(plotDF1) <- c("mean", "sd")
    plotDF1$sd <- c(inDF$aCO2_sd[inDF$terms=="MRT"], 
                    inDF$eCO2_sd[inDF$terms=="MRT"])
    plotDF1$Trt <- c("aCO2", "eCO2")
    plotDF1$pos <- with(plotDF1, mean + sd)
    plotDF1$neg <- with(plotDF1, mean - sd)
    
    plotDF2 <- data.frame(c(inDF$aCO2[inDF$terms=="MRT_canopy"], 
                            inDF$eCO2[inDF$terms=="MRT_canopy"]), 
                          NA)
    colnames(plotDF2) <- c("mean", "sd")
    plotDF2$sd <- c(inDF$aCO2_sd[inDF$terms=="MRT_canopy"], 
                    inDF$eCO2_sd[inDF$terms=="MRT_canopy"])
    plotDF2$Trt <- c("aCO2", "eCO2")
    plotDF2$pos <- with(plotDF2, mean + sd)
    plotDF2$neg <- with(plotDF2, mean - sd)
    
    plotDF3 <- data.frame(c(inDF$aCO2[inDF$terms=="MRT_ua"], 
                            inDF$eCO2[inDF$terms=="MRT_ua"]), 
                          NA)
    colnames(plotDF3) <- c("mean", "sd")
    plotDF3$sd <- c(inDF$aCO2_sd[inDF$terms=="MRT_ua"], 
                    inDF$eCO2_sd[inDF$terms=="MRT_ua"])
    plotDF3$Trt <- c("aCO2", "eCO2")
    plotDF3$pos <- with(plotDF3, mean + sd)
    plotDF3$neg <- with(plotDF3, mean - sd)
    
    plotDF4 <- data.frame(c(inDF$aCO2[inDF$terms=="MRT_belowground"], 
                            inDF$eCO2[inDF$terms=="MRT_belowground"]), 
                          NA)
    colnames(plotDF4) <- c("mean", "sd")
    plotDF4$sd <- c(inDF$aCO2_sd[inDF$terms=="MRT_belowground"], 
                    inDF$eCO2_sd[inDF$terms=="MRT_belowground"])
    plotDF4$Trt <- c("aCO2", "eCO2")
    plotDF4$pos <- with(plotDF4, mean + sd)
    plotDF4$neg <- with(plotDF4, mean - sd)
    
    
    p1 <- ggplot(plotDF1,
                 aes(Trt, mean)) + 
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
        #geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
        #              position = position_dodge(0.9), width=0.2, size=0.4) +
        xlab("") + ylab("MRT (yr)")+
        theme_linedraw() +
        ylim(0,10)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", values = c("aCO2" = "blue2", "eCO2" = "red3"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_x_discrete(limits=c("aCO2","eCO2"),
                         labels=c(expression(aCO[2]),
                                  expression(eCO[2])))
    
    
    p2 <- ggplot(plotDF2,
                 aes(Trt, mean)) + 
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
        #geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
        #              position = position_dodge(0.9), width=0.2, size=0.4) +
        xlab("") + ylab("Canopy MRT (yr)")+
        theme_linedraw() +
        ylim(0,10)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", values = c("aCO2" = "blue2", "eCO2" = "red3"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_x_discrete(limits=c("aCO2","eCO2"),
                         labels=c(expression(aCO[2]),
                                  expression(eCO[2])))
    
    p3 <- ggplot(plotDF3,
                 aes(Trt, mean)) + 
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
        #geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
        #              position = position_dodge(0.9), width=0.2, size=0.4) +
        xlab("") + ylab("Understorey aboveground MRT (yr)")+
        theme_linedraw() +
        ylim(0,10)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", values = c("aCO2" = "blue2", "eCO2" = "red3"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_x_discrete(limits=c("aCO2","eCO2"),
                         labels=c(expression(aCO[2]),
                                  expression(eCO[2])))
    
    p4 <- ggplot(plotDF4,
                 aes(Trt, mean)) + 
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
        #geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
        #              position = position_dodge(0.9), width=0.2, size=0.4) +
        xlab("") + ylab("Belowground MRT (yr)")+
        theme_linedraw() +
        ylim(0,10)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", values = c("aCO2" = "blue2", "eCO2" = "red3"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_x_discrete(limits=c("aCO2","eCO2"),
                         labels=c(expression(aCO[2]),
                                  expression(eCO[2])))
    

    grid.labs <- c("(a)", "(b)", "(c)", "(d)")
    
    
    pdf("plots_tables/Summary_P_Budget_MRT_Plots.pdf", width=8,height=8)
    plot_grid(p1,p2,p3,p4, labels="", ncol=2, align="v", axis = "l")
    grid.text(grid.labs, x = c(0.11, 0.60, 0.11, 0.60),
              y = c(0.95, 0.95, 0.45, 0.45), 
              gp=gpar(fontsize=14, col="black", fontface="bold"))
    dev.off()
    
    
}


