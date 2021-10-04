make_p_budget_ring_plots <- function(inDF, norm) {
    
    ################### Plot all P summary budget plots
    
    
    ### Plot 1
    plotDF1 <- data.frame(c(inDF$R1[inDF$terms=="Total plant P stock"], 
                            inDF$R2[inDF$terms=="Total plant P stock"],
                            inDF$R3[inDF$terms=="Total plant P stock"], 
                            inDF$R4[inDF$terms=="Total plant P stock"],
                            inDF$R5[inDF$terms=="Total plant P stock"], 
                            inDF$R6[inDF$terms=="Total plant P stock"],
                            inDF$aCO2[inDF$terms=="Total plant P stock"], 
                            inDF$eCO2[inDF$terms=="Total plant P stock"]), 
                          NA)
    colnames(plotDF1) <- c("mean", "Ring")
    plotDF1$Ring <- c("R1", "R2", "R3", "R4", "R5", "R6", "aCO2", "eCO2")
    plotDF1$plot.order <- c("P4", "P1", "P2", "P5", "P6", "P3", "P7", "P8")
    plotDF1$Trt <- c("eCO2", "aCO2", "aCO2", "eCO2", "eCO2", "aCO2", "aCO2", "eCO2")

    ### Plot 2
    plotDF2 <- data.frame(c(inDF$R1[inDF$terms=="Total plant P requirement flux"], 
                            inDF$R2[inDF$terms=="Total plant P requirement flux"],
                            inDF$R3[inDF$terms=="Total plant P requirement flux"], 
                            inDF$R4[inDF$terms=="Total plant P requirement flux"],
                            inDF$R5[inDF$terms=="Total plant P requirement flux"], 
                            inDF$R6[inDF$terms=="Total plant P requirement flux"],
                            inDF$aCO2[inDF$terms=="Total plant P requirement flux"], 
                            inDF$eCO2[inDF$terms=="Total plant P requirement flux"]), 
                          NA)
    colnames(plotDF2) <- c("mean", "Ring")
    plotDF2$Ring <- c("R1", "R2", "R3", "R4", "R5", "R6", "aCO2", "eCO2")
    plotDF2$plot.order <- c("P4", "P1", "P2", "P5", "P6", "P3", "P7", "P8")
    plotDF2$Trt <- c("eCO2", "aCO2", "aCO2", "eCO2", "eCO2", "aCO2", "aCO2", "eCO2")
    
    
    ### Plot 3
    plotDF3 <- data.frame(c(inDF$R1[inDF$terms=="Total plant P retranslocation flux"], 
                            inDF$R2[inDF$terms=="Total plant P retranslocation flux"],
                            inDF$R3[inDF$terms=="Total plant P retranslocation flux"], 
                            inDF$R4[inDF$terms=="Total plant P retranslocation flux"],
                            inDF$R5[inDF$terms=="Total plant P retranslocation flux"], 
                            inDF$R6[inDF$terms=="Total plant P retranslocation flux"],
                            inDF$aCO2[inDF$terms=="Total plant P retranslocation flux"], 
                            inDF$eCO2[inDF$terms=="Total plant P retranslocation flux"]), 
                          NA)
    colnames(plotDF3) <- c("mean", "Ring")
    plotDF3$Ring <- c("R1", "R2", "R3", "R4", "R5", "R6", "aCO2", "eCO2")
    plotDF3$plot.order <- c("P4", "P1", "P2", "P5", "P6", "P3", "P7", "P8")
    plotDF3$Trt <- c("eCO2", "aCO2", "aCO2", "eCO2", "eCO2", "aCO2", "aCO2", "eCO2")
    
    ### Plot 4
    plotDF4 <- data.frame(c(inDF$R1[inDF$terms=="Plant P uptake flux"], 
                            inDF$R2[inDF$terms=="Plant P uptake flux"],
                            inDF$R3[inDF$terms=="Plant P uptake flux"], 
                            inDF$R4[inDF$terms=="Plant P uptake flux"],
                            inDF$R5[inDF$terms=="Plant P uptake flux"], 
                            inDF$R6[inDF$terms=="Plant P uptake flux"],
                            inDF$aCO2[inDF$terms=="Plant P uptake flux"], 
                            inDF$eCO2[inDF$terms=="Plant P uptake flux"]), 
                          NA)
    colnames(plotDF4) <- c("mean", "Ring")
    plotDF4$Ring <- c("R1", "R2", "R3", "R4", "R5", "R6", "aCO2", "eCO2")
    plotDF4$plot.order <- c("P4", "P1", "P2", "P5", "P6", "P3", "P7", "P8")
    plotDF4$Trt <- c("eCO2", "aCO2", "aCO2", "eCO2", "eCO2", "aCO2", "aCO2", "eCO2")

    
    ### Plot 5  - mineralization
    plotDF5 <- data.frame(c(inDF$R1[inDF$terms=="Soil P mineralization flux"], 
                            inDF$R2[inDF$terms=="Soil P mineralization flux"],
                            inDF$R3[inDF$terms=="Soil P mineralization flux"], 
                            inDF$R4[inDF$terms=="Soil P mineralization flux"],
                            inDF$R5[inDF$terms=="Soil P mineralization flux"], 
                            inDF$R6[inDF$terms=="Soil P mineralization flux"],
                            inDF$aCO2[inDF$terms=="Soil P mineralization flux"], 
                            inDF$eCO2[inDF$terms=="Soil P mineralization flux"]), 
                          NA)
    colnames(plotDF5) <- c("mean", "Ring")
    plotDF5$Ring <- c("R1", "R2", "R3", "R4", "R5", "R6", "aCO2", "eCO2")
    plotDF5$plot.order <- c("P4", "P1", "P2", "P5", "P6", "P3", "P7", "P8")
    plotDF5$Trt <- c("eCO2", "aCO2", "aCO2", "eCO2", "eCO2", "aCO2", "aCO2", "eCO2")
    
    ### Plot 6
    plotDF6 <- data.frame(c(inDF$R1[inDF$terms=="Plant P uptake over requirement"], 
                            inDF$R2[inDF$terms=="Plant P uptake over requirement"],
                            inDF$R3[inDF$terms=="Plant P uptake over requirement"], 
                            inDF$R4[inDF$terms=="Plant P uptake over requirement"],
                            inDF$R5[inDF$terms=="Plant P uptake over requirement"], 
                            inDF$R6[inDF$terms=="Plant P uptake over requirement"],
                            inDF$aCO2[inDF$terms=="Plant P uptake over requirement"], 
                            inDF$eCO2[inDF$terms=="Plant P uptake over requirement"]), 
                          NA)
    colnames(plotDF6) <- c("mean", "Ring")
    plotDF6$Ring <- c("R1", "R2", "R3", "R4", "R5", "R6", "aCO2", "eCO2")
    plotDF6$plot.order <- c("P4", "P1", "P2", "P5", "P6", "P3", "P7", "P8")
    plotDF6$Trt <- c("eCO2", "aCO2", "aCO2", "eCO2", "eCO2", "aCO2", "aCO2", "eCO2")
    
    
    ### Plot 8
    plotDF8 <- data.frame(c(inDF$R1[inDF$terms=="Plant P MRT"], 
                            inDF$R2[inDF$terms=="Plant P MRT"],
                            inDF$R3[inDF$terms=="Plant P MRT"], 
                            inDF$R4[inDF$terms=="Plant P MRT"],
                            inDF$R5[inDF$terms=="Plant P MRT"], 
                            inDF$R6[inDF$terms=="Plant P MRT"],
                            inDF$aCO2[inDF$terms=="Plant P MRT"], 
                            inDF$eCO2[inDF$terms=="Plant P MRT"]), 
                          NA)
    colnames(plotDF8) <- c("mean", "Ring")
    plotDF8$Ring <- c("R1", "R2", "R3", "R4", "R5", "R6", "aCO2", "eCO2")
    plotDF8$plot.order <- c("P4", "P1", "P2", "P5", "P6", "P3", "P7", "P8")
    plotDF8$Trt <- c("eCO2", "aCO2", "aCO2", "eCO2", "eCO2", "aCO2", "aCO2", "eCO2")
    
    
    ### Plot 9
    plotDF9 <- data.frame(c(inDF$R1[inDF$terms=="Plant PUE"], 
                            inDF$R2[inDF$terms=="Plant PUE"],
                            inDF$R3[inDF$terms=="Plant PUE"], 
                            inDF$R4[inDF$terms=="Plant PUE"],
                            inDF$R5[inDF$terms=="Plant PUE"], 
                            inDF$R6[inDF$terms=="Plant PUE"],
                            inDF$aCO2[inDF$terms=="Plant PUE"], 
                            inDF$eCO2[inDF$terms=="Plant PUE"]), 
                          NA)
    colnames(plotDF9) <- c("mean", "Ring")
    plotDF9$Ring <- c("R1", "R2", "R3", "R4", "R5", "R6", "aCO2", "eCO2")
    plotDF9$plot.order <- c("P4", "P1", "P2", "P5", "P6", "P3", "P7", "P8")
    plotDF9$Trt <- c("eCO2", "aCO2", "aCO2", "eCO2", "eCO2", "aCO2", "aCO2", "eCO2")

    ### Plotting
    
    p1 <- ggplot(plotDF1,
                 aes(plot.order, mean)) + 
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
        xlab("") + ylab(expression(paste("Standing P stock (g P ", m^-2, ")")))+
        theme_linedraw() +
        ylim(0, 2.0)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
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
        scale_x_discrete(limits=c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8"),
                         labels=c("R2", "R3", "R6", "R1", "R4", "R5", expression(aCO[2]),
                         expression(eCO[2])))+
        geom_vline(xintercept = 3.5, linetype="dotted", 
                   color = "black", size=1)+
        geom_vline(xintercept = 6.5, linetype="dotted", 
                   color = "black", size=1)
    
    p2 <- ggplot(plotDF2,
                 aes(plot.order, mean)) + 
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
        xlab("") + ylab(expression(paste("Plant P requirement (g P ", m^-2, " ", yr^-1, ")")))+
        theme_linedraw() +
        #ylim(0, 1.5)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
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
        scale_x_discrete(limits=c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8"),
                         labels=c("R2", "R3", "R6", "R1", "R4", "R5", expression(aCO[2]),
                                  expression(eCO[2])))+
        geom_vline(xintercept = 3.5, linetype="dotted", 
                   color = "black", size=1)+
        geom_vline(xintercept = 6.5, linetype="dotted", 
                   color = "black", size=1)
    
    p3 <- ggplot(plotDF3,
                 aes(plot.order, mean)) + 
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
        xlab("") + ylab(expression(paste("Plant P retranslocation (g P ", m^-2, " ", yr^-1, ")")))+
        theme_linedraw() +
        ylim(0, 1.0)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
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
        scale_x_discrete(limits=c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8"),
                         labels=c("R2", "R3", "R6", "R1", "R4", "R5", expression(aCO[2]),
                                  expression(eCO[2])))+
        geom_vline(xintercept = 3.5, linetype="dotted", 
                   color = "black", size=1)+
        geom_vline(xintercept = 6.5, linetype="dotted", 
                   color = "black", size=1)
    
    p4 <- ggplot(plotDF4,
                 aes(plot.order, mean)) + 
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
        xlab("") + ylab(expression(paste("Plant P uptake (g P ", m^-2, " ", yr^-1, ")")))+
        theme_linedraw() +
        ylim(0, 0.6)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
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
        scale_x_discrete(limits=c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8"),
                         labels=c("R2", "R3", "R6", "R1", "R4", "R5", expression(aCO[2]),
                                  expression(eCO[2])))+
        geom_vline(xintercept = 3.5, linetype="dotted", 
                   color = "black", size=1)+
        geom_vline(xintercept = 6.5, linetype="dotted", 
                   color = "black", size=1)
    
    p5 <- ggplot(plotDF5,
                 aes(plot.order, mean)) + 
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
        xlab("") + ylab(expression(paste("P mineralization rate (g P ", m^-2, " ", yr^-1, ")")))+
        theme_linedraw() +
        ylim(0, 0.6)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
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
        scale_x_discrete(limits=c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8"),
                         labels=c("R2", "R3", "R6", "R1", "R4", "R5", expression(aCO[2]),
                                  expression(eCO[2])))+
        geom_vline(xintercept = 3.5, linetype="dotted", 
                   color = "black", size=1)+
        geom_vline(xintercept = 6.5, linetype="dotted", 
                   color = "black", size=1)
    
    
    p6 <- ggplot(plotDF6,
                 aes(plot.order, mean)) + 
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
        xlab("") + ylab("Uptake over requirement (%)")+
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
        scale_fill_manual(name="", values = c("aCO2" = "blue2", "eCO2" = "red3"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_x_discrete(limits=c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8"),
                         labels=c("R2", "R3", "R6", "R1", "R4", "R5", expression(aCO[2]),
                                  expression(eCO[2])))+
        geom_vline(xintercept = 3.5, linetype="dotted", 
                   color = "black", size=1)+
        geom_vline(xintercept = 6.5, linetype="dotted", 
                   color = "black", size=1)
    
    p8 <- ggplot(plotDF8,
                 aes(plot.order, mean)) + 
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
        xlab("") + ylab(expression(paste("P MRT (", yr, ")")))+
        theme_linedraw() +
        ylim(0, 12)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
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
        scale_x_discrete(limits=c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8"),
                         labels=c("R2", "R3", "R6", "R1", "R4", "R5", expression(aCO[2]),
                                  expression(eCO[2])))+
        geom_vline(xintercept = 3.5, linetype="dotted", 
                   color = "black", size=1)+
        geom_vline(xintercept = 6.5, linetype="dotted", 
                   color = "black", size=1)
    
    p9 <- ggplot(plotDF9,
                 aes(plot.order, mean)) + 
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
        xlab("") + ylab(expression(paste("Plant PUE (g C g ", P^-1, ")")))+
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
        scale_fill_manual(name="", values = c("aCO2" = "blue2", "eCO2" = "red3"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_x_discrete(limits=c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8"),
                         labels=c("R2", "R3", "R6", "R1", "R4", "R5", expression(aCO[2]),
                                  expression(eCO[2])))+
        geom_vline(xintercept = 3.5, linetype="dotted", 
                   color = "black", size=1)+
        geom_vline(xintercept = 6.5, linetype="dotted", 
                   color = "black", size=1)
    

    grid.labs <- c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)", "(g)", "(h)")
    
    require(grid)
    require(cowplot)
    
    ## plot 
    pdf(paste0("plots_tables/output/P_Budget_Ring_Plots_", norm, ".pdf"),
        width=8,height=12)
    plot_grid(p2, p1, p3, p4, p8, p5, p9, p6, labels="", ncol=2, align="v", axis = "l")
    grid.text(grid.labs, x = c(0.12, 0.62, 0.12, 0.62, 0.12, 0.62, 0.12, 0.62),
              y = c(0.97, 0.97, 0.72, 0.72, 0.47, 0.47, 0.23, 0.23), 
              gp=gpar(fontsize=14, col="black", fontface="bold"))
    dev.off()
    
}


