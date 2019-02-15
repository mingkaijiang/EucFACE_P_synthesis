make_summary_p_budget_ring_plots <- function() {
    
    ################### Plot all P summary budget plots
    
    ## Total plant standing P, with components of OA, UA, and belowground
    inDF <- summary_table_total_p_budgets
    
    ### Plot 1
    plotDF1 <- data.frame(c(inDF$R1[inDF$terms=="total standing p stock"], 
                            inDF$R2[inDF$terms=="total standing p stock"],
                            inDF$R3[inDF$terms=="total standing p stock"], 
                            inDF$R4[inDF$terms=="total standing p stock"],
                            inDF$R5[inDF$terms=="total standing p stock"], 
                            inDF$R6[inDF$terms=="total standing p stock"],
                            inDF$aCO2[inDF$terms=="total standing p stock"], 
                            inDF$eCO2[inDF$terms=="total standing p stock"]), 
                          NA)
    colnames(plotDF1) <- c("mean", "Ring")
    plotDF1$Ring <- c("R1", "R2", "R3", "R4", "R5", "R6", "aCO2", "eCO2")
    plotDF1$plot.order <- c("P4", "P1", "P2", "P5", "P6", "P3", "P7", "P8")
    plotDF1$Trt <- c("eCO2", "aCO2", "aCO2", "eCO2", "eCO2", "aCO2", "aCO2", "eCO2")

    ### Plot 2
    plotDF2 <- data.frame(c(inDF$R1[inDF$terms=="total p requirement"], 
                            inDF$R2[inDF$terms=="total p requirement"],
                            inDF$R3[inDF$terms=="total p requirement"], 
                            inDF$R4[inDF$terms=="total p requirement"],
                            inDF$R5[inDF$terms=="total p requirement"], 
                            inDF$R6[inDF$terms=="total p requirement"],
                            inDF$aCO2[inDF$terms=="total p requirement"], 
                            inDF$eCO2[inDF$terms=="total p requirement"]), 
                          NA)
    colnames(plotDF2) <- c("mean", "Ring")
    plotDF2$Ring <- c("R1", "R2", "R3", "R4", "R5", "R6", "aCO2", "eCO2")
    plotDF2$plot.order <- c("P4", "P1", "P2", "P5", "P6", "P3", "P7", "P8")
    plotDF2$Trt <- c("eCO2", "aCO2", "aCO2", "eCO2", "eCO2", "aCO2", "aCO2", "eCO2")
    
    
    ### Plot 3
    plotDF3 <- data.frame(c(inDF$R1[inDF$terms=="total p retranslocated"], 
                            inDF$R2[inDF$terms=="total p retranslocated"],
                            inDF$R3[inDF$terms=="total p retranslocated"], 
                            inDF$R4[inDF$terms=="total p retranslocated"],
                            inDF$R5[inDF$terms=="total p retranslocated"], 
                            inDF$R6[inDF$terms=="total p retranslocated"],
                            inDF$aCO2[inDF$terms=="total p retranslocated"], 
                            inDF$eCO2[inDF$terms=="total p retranslocated"]), 
                          NA)
    colnames(plotDF3) <- c("mean", "Ring")
    plotDF3$Ring <- c("R1", "R2", "R3", "R4", "R5", "R6", "aCO2", "eCO2")
    plotDF3$plot.order <- c("P4", "P1", "P2", "P5", "P6", "P3", "P7", "P8")
    plotDF3$Trt <- c("eCO2", "aCO2", "aCO2", "eCO2", "eCO2", "aCO2", "aCO2", "eCO2")
    
    ### Plot 4
    plotDF4 <- data.frame(c(inDF$R1[inDF$terms=="total p uptake from soil"], 
                            inDF$R2[inDF$terms=="total p uptake from soil"],
                            inDF$R3[inDF$terms=="total p uptake from soil"], 
                            inDF$R4[inDF$terms=="total p uptake from soil"],
                            inDF$R5[inDF$terms=="total p uptake from soil"], 
                            inDF$R6[inDF$terms=="total p uptake from soil"],
                            inDF$aCO2[inDF$terms=="total p uptake from soil"], 
                            inDF$eCO2[inDF$terms=="total p uptake from soil"]), 
                          NA)
    colnames(plotDF4) <- c("mean", "Ring")
    plotDF4$Ring <- c("R1", "R2", "R3", "R4", "R5", "R6", "aCO2", "eCO2")
    plotDF4$plot.order <- c("P4", "P1", "P2", "P5", "P6", "P3", "P7", "P8")
    plotDF4$Trt <- c("eCO2", "aCO2", "aCO2", "eCO2", "eCO2", "aCO2", "aCO2", "eCO2")

    
    ### Plot 5  - mineralization
    plotDF5 <- data.frame(c(inDF$R1[inDF$terms=="soil p mineralization"], 
                            inDF$R2[inDF$terms=="soil p mineralization"],
                            inDF$R3[inDF$terms=="soil p mineralization"], 
                            inDF$R4[inDF$terms=="soil p mineralization"],
                            inDF$R5[inDF$terms=="soil p mineralization"], 
                            inDF$R6[inDF$terms=="soil p mineralization"],
                            inDF$aCO2[inDF$terms=="soil p mineralization"], 
                            inDF$eCO2[inDF$terms=="soil p mineralization"]), 
                          NA)
    colnames(plotDF5) <- c("mean", "Ring")
    plotDF5$Ring <- c("R1", "R2", "R3", "R4", "R5", "R6", "aCO2", "eCO2")
    plotDF5$plot.order <- c("P4", "P1", "P2", "P5", "P6", "P3", "P7", "P8")
    plotDF5$Trt <- c("eCO2", "aCO2", "aCO2", "eCO2", "eCO2", "aCO2", "aCO2", "eCO2")
    
    ### Plot 6
    plotDF6 <- data.frame(c(inDF$R1[inDF$terms=="total uptake over requirement"], 
                            inDF$R2[inDF$terms=="total uptake over requirement"],
                            inDF$R3[inDF$terms=="total uptake over requirement"], 
                            inDF$R4[inDF$terms=="total uptake over requirement"],
                            inDF$R5[inDF$terms=="total uptake over requirement"], 
                            inDF$R6[inDF$terms=="total uptake over requirement"],
                            inDF$aCO2[inDF$terms=="total uptake over requirement"], 
                            inDF$eCO2[inDF$terms=="total uptake over requirement"]), 
                          NA)
    colnames(plotDF6) <- c("mean", "Ring")
    plotDF6$Ring <- c("R1", "R2", "R3", "R4", "R5", "R6", "aCO2", "eCO2")
    plotDF6$plot.order <- c("P4", "P1", "P2", "P5", "P6", "P3", "P7", "P8")
    plotDF6$Trt <- c("eCO2", "aCO2", "aCO2", "eCO2", "eCO2", "aCO2", "aCO2", "eCO2")
    
    
    ### Plot 7  - gap in demand and supply
    plotDF7 <- data.frame(c(inDF$R1[inDF$terms=="p supply and uptake gap"], 
                            inDF$R2[inDF$terms=="p supply and uptake gap"],
                            inDF$R3[inDF$terms=="p supply and uptake gap"], 
                            inDF$R4[inDF$terms=="p supply and uptake gap"],
                            inDF$R5[inDF$terms=="p supply and uptake gap"], 
                            inDF$R6[inDF$terms=="p supply and uptake gap"],
                            inDF$aCO2[inDF$terms=="p supply and uptake gap"], 
                            inDF$eCO2[inDF$terms=="p supply and uptake gap"]), 
                          NA)
    colnames(plotDF7) <- c("mean", "Ring")
    plotDF7$Ring <- c("R1", "R2", "R3", "R4", "R5", "R6", "aCO2", "eCO2")
    plotDF7$plot.order <- c("P4", "P1", "P2", "P5", "P6", "P3", "P7", "P8")
    plotDF7$Trt <- c("eCO2", "aCO2", "aCO2", "eCO2", "eCO2", "aCO2", "aCO2", "eCO2")
    
    ### Plot 8
    plotDF8 <- data.frame(c(inDF$R1[inDF$terms=="total P MRT in plant"], 
                            inDF$R2[inDF$terms=="total P MRT in plant"],
                            inDF$R3[inDF$terms=="total P MRT in plant"], 
                            inDF$R4[inDF$terms=="total P MRT in plant"],
                            inDF$R5[inDF$terms=="total P MRT in plant"], 
                            inDF$R6[inDF$terms=="total P MRT in plant"],
                            inDF$aCO2[inDF$terms=="total P MRT in plant"], 
                            inDF$eCO2[inDF$terms=="total P MRT in plant"]), 
                          NA)
    colnames(plotDF8) <- c("mean", "Ring")
    plotDF8$Ring <- c("R1", "R2", "R3", "R4", "R5", "R6", "aCO2", "eCO2")
    plotDF8$plot.order <- c("P4", "P1", "P2", "P5", "P6", "P3", "P7", "P8")
    plotDF8$Trt <- c("eCO2", "aCO2", "aCO2", "eCO2", "eCO2", "aCO2", "aCO2", "eCO2")
    
    
    ### Plot 9
    plotDF9 <- data.frame(c(inDF$R1[inDF$terms=="total standing PUE"], 
                            inDF$R2[inDF$terms=="total standing PUE"],
                            inDF$R3[inDF$terms=="total standing PUE"], 
                            inDF$R4[inDF$terms=="total standing PUE"],
                            inDF$R5[inDF$terms=="total standing PUE"], 
                            inDF$R6[inDF$terms=="total standing PUE"],
                            inDF$aCO2[inDF$terms=="total standing PUE"], 
                            inDF$eCO2[inDF$terms=="total standing PUE"]), 
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
        ylim(0, 0.5)+
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
        ylim(0, 0.5)+
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
    
    p6 <- ggplot(plotDF7,
                 aes(plot.order, mean)) + 
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
        xlab("") + ylab(expression(paste("P supply-demand gap (g P ", m^-2, " ", yr^-1, ")")))+
        theme_linedraw() +
        ylim(-0.5, 0.4)+
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
    
    p7 <- ggplot(plotDF6,
                 aes(plot.order, mean)) + 
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
        xlab("") + ylab("Uptake over requirement (%)")+
        theme_linedraw() +
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
    

    grid.labs <- c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)", "(g)", "(h)", "(i)")
    
    require(grid)
    require(cowplot)
    
    ## plot 
    pdf("plots_tables/Summary_P_Budget_Ring_Plots.pdf", width=8,height=12)
    plot_grid(p2, p1, p3, p7, p4, p8, p5, p9, p6, labels="", ncol=2, align="v", axis = "l")
    grid.text(grid.labs, x = c(0.12, 0.62, 0.12, 0.62, 0.12, 0.62, 0.12, 0.62, 0.12),
              y = c(0.97, 0.97, 0.78, 0.78, 0.58, 0.58, 0.38, 0.38, 0.18), 
              gp=gpar(fontsize=14, col="black", fontface="bold"))
    dev.off()
    
}


