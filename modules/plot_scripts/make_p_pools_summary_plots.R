make_p_pools_summary_plots <- function(inDF,norm) {
    
    ### Plot 1
    plotDF1 <- data.frame(c(inDF$aCO2[inDF$terms=="Total Wood P Pool"], 
                            inDF$eCO2[inDF$terms=="Total Wood P Pool"],
                            inDF$aCO2[inDF$terms=="Sapwood P Pool"], 
                            inDF$eCO2[inDF$terms=="Sapwood P Pool"],
                            inDF$aCO2[inDF$terms=="Heartwood P Pool"], 
                            inDF$eCO2[inDF$terms=="Heartwood P Pool"],
                            inDF$aCO2[inDF$terms=="Canopy P Pool"], 
                            inDF$eCO2[inDF$terms=="Canopy P Pool"],
                            inDF$aCO2[inDF$terms=="Fine Root P Pool"], 
                            inDF$eCO2[inDF$terms=="Fine Root P Pool"],
                            inDF$aCO2[inDF$terms=="Coarse Root P Pool"], 
                            inDF$eCO2[inDF$terms=="Coarse Root P Pool"],
                            inDF$aCO2[inDF$terms=="Understorey P Pool"], 
                            inDF$eCO2[inDF$terms=="Understorey P Pool"],
                            inDF$aCO2[inDF$terms=="Forestfloor Leaf Litter P Pool"], 
                            inDF$eCO2[inDF$terms=="Forestfloor Leaf Litter P Pool"],
                            inDF$aCO2[inDF$terms=="Understorey Litter P Pool"], 
                            inDF$eCO2[inDF$terms=="Understorey Litter P Pool"],
                            inDF$aCO2[inDF$terms=="Standing Dead Wood P Pool"], 
                            inDF$eCO2[inDF$terms=="Standing Dead Wood P Pool"]), 
                          NA, NA)
    colnames(plotDF1) <- c("mean", "sd", "Variable")
    plotDF1$sd <- c(inDF$aCO2_sd[inDF$terms=="Total Wood P Pool"], 
                    inDF$eCO2_sd[inDF$terms=="Total Wood P Pool"],
                    inDF$aCO2_sd[inDF$terms=="Sapwood P Pool"], 
                    inDF$eCO2_sd[inDF$terms=="Sapwood P Pool"],
                    inDF$aCO2_sd[inDF$terms=="Heartwood P Pool"], 
                    inDF$eCO2_sd[inDF$terms=="Heartwood P Pool"],
                    inDF$aCO2_sd[inDF$terms=="Canopy P Pool"], 
                    inDF$eCO2_sd[inDF$terms=="Canopy P Pool"],
                    inDF$aCO2_sd[inDF$terms=="Fine Root P Pool"], 
                    inDF$eCO2_sd[inDF$terms=="Fine Root P Pool"],
                    inDF$aCO2_sd[inDF$terms=="Coarse Root P Pool"], 
                    inDF$eCO2_sd[inDF$terms=="Coarse Root P Pool"],
                    inDF$aCO2_sd[inDF$terms=="Understorey P Pool"], 
                    inDF$eCO2_sd[inDF$terms=="Understorey P Pool"],
                    inDF$aCO2_sd[inDF$terms=="Forestfloor Leaf Litter P Pool"], 
                    inDF$eCO2_sd[inDF$terms=="Forestfloor Leaf Litter P Pool"],
                    inDF$aCO2_sd[inDF$terms=="Understorey Litter P Pool"], 
                    inDF$eCO2_sd[inDF$terms=="Understorey Litter P Pool"],
                    inDF$aCO2_sd[inDF$terms=="Standing Dead Wood P Pool"], 
                    inDF$eCO2_sd[inDF$terms=="Standing Dead Wood P Pool"])
    plotDF1$Variable <- rep(c("Wood", "Sapwood", "Heartwood", 
                              "Canopy", "Fine Root", 
                              "Coarse Root", "Understorey",
                              "Forestfloor Litter", "Understorey Litter",
                              "Standing Dead"), each=2)
    plotDF1$Trt <- rep(c("aCO2", "eCO2"), 10)
    plotDF1$pos <- with(plotDF1, mean + sd)
    plotDF1$neg <- with(plotDF1, mean - sd)
    
    ### Plot 2
    plotDF2 <- data.frame(c(inDF$aCO2[inDF$terms=="Microbial P Pool 0-10cm"], 
                            inDF$eCO2[inDF$terms=="Microbial P Pool 0-10cm"],
                            inDF$aCO2[inDF$terms=="Microbial P Pool 10-30cm"], 
                            inDF$eCO2[inDF$terms=="Microbial P Pool 10-30cm"],
                            inDF$aCO2[inDF$terms=="Microbial P Pool 30-60cm"], 
                            inDF$eCO2[inDF$terms=="Microbial P Pool 30-60cm"]), 
                          NA, NA)
    colnames(plotDF2) <- c("mean", "sd", "Variable")
    plotDF2$sd <- c(inDF$aCO2_sd[inDF$terms=="Microbial P Pool 0-10cm"], 
                    inDF$eCO2_sd[inDF$terms=="Microbial P Pool 0-10cm"],
                    inDF$aCO2_sd[inDF$terms=="Microbial P Pool 10-30cm"], 
                    inDF$eCO2_sd[inDF$terms=="Microbial P Pool 10-30cm"],
                    inDF$aCO2_sd[inDF$terms=="Microbial P Pool 30-60cm"], 
                    inDF$eCO2_sd[inDF$terms=="Microbial P Pool 30-60cm"])
    plotDF2$Variable <- rep(c("Microbe 0-10cm","Microbe 10-30cm","Microbe 30-60cm"), each=2)
    plotDF2$Trt <- rep(c("aCO2", "eCO2"), 3)
    plotDF2$pos <- with(plotDF2, mean + sd)
    plotDF2$neg <- with(plotDF2, mean - sd)
    
    ### Plot 3
    plotDF3 <- data.frame(c(inDF$aCO2[inDF$terms=="Soil Phosphate P Pool 0-10cm"], 
                            inDF$eCO2[inDF$terms=="Soil Phosphate P Pool 0-10cm"],
                            inDF$aCO2[inDF$terms=="Soil Phosphate P Pool 10-30cm"], 
                            inDF$eCO2[inDF$terms=="Soil Phosphate P Pool 10-30cm"],
                            inDF$aCO2[inDF$terms=="Soil Phosphate P Pool 30-60cm"], 
                            inDF$eCO2[inDF$terms=="Soil Phosphate P Pool 30-60cm"]), 
                          NA, NA)
    colnames(plotDF3) <- c("mean", "sd", "Variable")
    plotDF3$sd <- c(inDF$aCO2_sd[inDF$terms=="Soil Phosphate P Pool 0-10cm"], 
                    inDF$eCO2_sd[inDF$terms=="Soil Phosphate P Pool 0-10cm"],
                    inDF$aCO2_sd[inDF$terms=="Soil Phosphate P Pool 10-30cm"], 
                    inDF$eCO2_sd[inDF$terms=="Soil Phosphate P Pool 10-30cm"],
                    inDF$aCO2_sd[inDF$terms=="Soil Phosphate P Pool 30-60cm"], 
                    inDF$eCO2_sd[inDF$terms=="Soil Phosphate P Pool 30-60cm"])
    plotDF3$Variable <- rep(c("Soil Phosphate  0-10cm","Soil Phosphate  10-30cm","Soil Phosphate  30-60cm"), each=2)
    plotDF3$Trt <- rep(c("aCO2", "eCO2"), 3)
    plotDF3$pos <- with(plotDF3, mean + sd)
    plotDF3$neg <- with(plotDF3, mean - sd)
    
    ### Plot 4
    plotDF4 <- data.frame(c(inDF$aCO2[inDF$terms=="Soil P Pool 0-10cm"], 
                            inDF$eCO2[inDF$terms=="Soil P Pool 0-10cm"],
                            inDF$aCO2[inDF$terms=="Soil P Pool 10-30cm"], 
                            inDF$eCO2[inDF$terms=="Soil P Pool 10-30cm"],
                            inDF$aCO2[inDF$terms=="Soil P Pool 30-60cm"], 
                            inDF$eCO2[inDF$terms=="Soil P Pool 30-60cm"]), 
                          NA, NA)
    colnames(plotDF4) <- c("mean", "sd", "Variable")
    plotDF4$sd <- c(inDF$aCO2_sd[inDF$terms=="Soil P Pool 0-10cm"], 
                    inDF$eCO2_sd[inDF$terms=="Soil P Pool 0-10cm"],
                    inDF$aCO2_sd[inDF$terms=="Soil P Pool 10-30cm"], 
                    inDF$eCO2_sd[inDF$terms=="Soil P Pool 10-30cm"],
                    inDF$aCO2_sd[inDF$terms=="Soil P Pool 30-60cm"], 
                    inDF$eCO2_sd[inDF$terms=="Soil P Pool 30-60cm"])
    plotDF4$Variable <- rep(c("Soil 0-10cm","Soil 10-30cm","Soil 30-60cm"), each=2)
    plotDF4$Trt <- rep(c("aCO2", "eCO2"), 3)
    plotDF4$pos <- with(plotDF4, mean + sd)
    plotDF4$neg <- with(plotDF4, mean - sd)
    
    ### Plotting
    p1 <- ggplot(plotDF1, aes(x=Variable, y=mean))+
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge")+
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        labs(x="", y=expression(paste("P pool (g P ", m^-2, ")")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="top")+
        scale_fill_manual(name="", values = c("aCO2" = "blue2", "eCO2" = "red3"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))
    

    p2 <- ggplot(plotDF2, aes(x=Variable, y=mean))+
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge")+
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        labs(x="", y=expression(paste("P pool (g P ", m^-2, ")")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", values = c("aCO2" = "blue2", "eCO2" = "red3"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))
    
    p3 <- ggplot(plotDF3, aes(x=Variable, y=mean))+
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge")+
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        labs(x="", y=expression(paste("P pool (g P ", m^-2, ")")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", values = c("aCO2" = "blue2", "eCO2" = "red3"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_x_discrete(labels=c("Soil Phosphate  0-10cm"=expression("Soil " * PO[4]-P * " (0-10cm)"),
                                "Soil Phosphate  10-30cm"=expression("Soil " * PO[4]-P * " (10-30cm)"),
                                "Soil Phosphate  30-60cm"=expression("Soil " * PO[4]-P * " (30-60cm)")))
    
    p4 <- ggplot(plotDF4, aes(x=Variable, y=mean))+
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge")+
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        labs(x="", y=expression(paste("P pool (g P ", m^-2, ")")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", values = c("aCO2" = "blue2", "eCO2" = "red3"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))
    
    
    
    grid.labs <- c("(a)", "(b)", "(c)", "(d)")
    
    require(grid)
    require(cowplot)
    
    require(gridExtra)
    
    ## plot 
    pdf(paste0("plots_tables/output/", norm, "/P_Pools_Summary_Plots_", norm, ".pdf"),
        width=16,height=8)
    bot_row <- plot_grid(p2, p3, p4, ncol=3)
    plot_grid(p1, bot_row,  ncol = 1, rel_widths = c(1, 0.6,0.6,0.6),
              rel_heights=c(1.2, 1, 1, 1))
    grid.text(grid.labs,x = c(0.06, 0.05, 0.38, 0.72), y = c(0.9, 0.42, 0.42, 0.42),
              gp=gpar(fontsize=16, col="black", fontface="bold"))
    dev.off()
    
    
    
    

    
    
    vegDF <- inDF[inDF$terms%in%c("Canopy P Pool", "Total Wood P Pool", "Forestfloor Leaf Litter P Pool",
                                  "Fine Root P Pool", "Coarse Root P Pool", "Understorey P Pool",
                                  "Understorey Litter P Pool", "Standing Dead Wood P Pool"),]
    
    soilDF1 <- inDF[inDF$terms%in%c("Soil P Pool 0-10cm", "Soil P Pool 10-30cm", 
                                   "Soil P Pool 30-60cm"),]
    
    
    totDF <- inDF[inDF$terms%in%c("Canopy P Pool", 
                                  "Total Wood P Pool", 
                                  "Sapwood P Pool", 
                                  "Heartwood P Pool", 
                                  "Forestfloor Leaf Litter P Pool",
                                  "Fine Root P Pool", 
                                  "Coarse Root P Pool", 
                                  "Understorey P Pool",
                                  "Understorey Litter P Pool", 
                                  "Standing Dead Wood P Pool",
                                  "Soil P Pool 0-10cm", 
                                  "Soil P Pool 10-30cm", 
                                  "Soil P Pool 30-60cm",
                                  "Soil Inorg P Pool 0-10cm", 
                                  "Soil Inorg P Pool 10-30cm", 
                                  "Soil Inorg P Pool 30-60cm",
                                  "Soil Org P Pool 0-10cm", 
                                  "Soil Org P Pool 10-30cm", 
                                  "Soil Org P Pool 30-60cm",
                                  "Soil Phosphate P Pool 0-10cm", 
                                  "Soil Phosphate P Pool 10-30cm", 
                                  "Soil Phosphate P Pool 30-60cm",
                                  "Microbial P Pool 0-10cm",
                                  "Microbial P Pool 10-30cm",
                                  "Microbial P Pool 30-60cm"),]
    
    ### prepare plotDF
    #plotDF1 <- data.frame(c(totDF$aCO2[totDF$terms=="Canopy P Pool"], 
    #                        totDF$eCO2[totDF$terms=="Canopy P Pool"],
    #                        totDF$aCO2[totDF$terms=="Total Wood P Pool"], 
    #                        totDF$eCO2[totDF$terms=="Total Wood P Pool"],
    #                        totDF$aCO2[totDF$terms=="Forestfloor Leaf Litter P Pool"], 
    #                        totDF$eCO2[totDF$terms=="Forestfloor Leaf Litter P Pool"],
    #                        totDF$aCO2[totDF$terms=="Fine Root P Pool"], 
    #                        totDF$eCO2[totDF$terms=="Fine Root P Pool"],
    #                        totDF$aCO2[totDF$terms=="Coarse Root P Pool"], 
    #                        totDF$eCO2[totDF$terms=="Coarse Root P Pool"],
    #                        totDF$aCO2[totDF$terms=="Understorey P Pool"], 
    #                        totDF$eCO2[totDF$terms=="Understorey P Pool"],
    #                        totDF$aCO2[totDF$terms=="Understorey Litter P Pool"], 
    #                        totDF$eCO2[totDF$terms=="Understorey Litter P Pool"],
    #                        totDF$aCO2[totDF$terms=="Standing Dead Wood P Pool"], 
    #                        totDF$eCO2[totDF$terms=="Standing Dead Wood P Pool"]), 
    #                      NA, NA)
    #colnames(plotDF1) <- c("mean", "sd", "Variable")
    #plotDF1$sd <- c(totDF$aCO2_sd[totDF$terms=="Canopy P Pool"], 
    #                totDF$eCO2_sd[totDF$terms=="Canopy P Pool"],
    #                totDF$aCO2_sd[totDF$terms=="Total Wood P Pool"], 
    #                totDF$eCO2_sd[totDF$terms=="Total Wood P Pool"],
    #                totDF$aCO2_sd[totDF$terms=="Forestfloor Leaf Litter P Pool"], 
    #                totDF$eCO2_sd[totDF$terms=="Forestfloor Leaf Litter P Pool"],
    #                totDF$aCO2_sd[totDF$terms=="Fine Root P Pool"], 
    #                totDF$eCO2_sd[totDF$terms=="Fine Root P Pool"],
    #                totDF$aCO2_sd[totDF$terms=="Coarse Root P Pool"], 
    #                totDF$eCO2_sd[totDF$terms=="Coarse Root P Pool"],
    #                totDF$aCO2_sd[totDF$terms=="Understorey P Pool"], 
    #                totDF$eCO2_sd[totDF$terms=="Understorey P Pool"],
    #                totDF$aCO2_sd[totDF$terms=="Understorey Litter P Pool"], 
    #                totDF$eCO2_sd[totDF$terms=="Understorey Litter P Pool"],
    #                totDF$aCO2_sd[totDF$terms=="Standing Dead Wood P Pool"], 
    #                totDF$eCO2_sd[totDF$terms=="Standing Dead Wood P Pool"])
    #plotDF1$Variable <- rep(c("Canopy P Pool",
    #                          "Total Wood P Pool",
    #                          "Forestfloor Leaf Litter P Pool",
    #                          "Fine Root P Pool",
    #                          "Coarse Root P Pool",
    #                          "Understorey P Pool",
    #                          "Understorey Litter P Pool",
    #                          "Standing Dead Wood P Pool"), 
    #                        each=2)
    #plotDF1$Trt <- rep(c("aCO2", "eCO2"), 8)
    #plotDF1$pos <- with(plotDF1, mean + sd)
    #plotDF1$neg <- with(plotDF1, mean - sd)
    
    
    
    
    
    plotDF <- data.frame(c(totDF$aCO2[totDF$terms=="Canopy P Pool"], 
                           totDF$eCO2[totDF$terms=="Canopy P Pool"],
                           totDF$aCO2[totDF$terms=="Total Wood P Pool"], 
                           totDF$eCO2[totDF$terms=="Total Wood P Pool"],
                           totDF$aCO2[totDF$terms=="Sapwood P Pool"], 
                           totDF$eCO2[totDF$terms=="Sapwood P Pool"],
                           totDF$aCO2[totDF$terms=="Heartwood P Pool"], 
                           totDF$eCO2[totDF$terms=="Heartwood P Pool"],
                           totDF$aCO2[totDF$terms=="Forestfloor Leaf Litter P Pool"], 
                           totDF$eCO2[totDF$terms=="Forestfloor Leaf Litter P Pool"],
                           totDF$aCO2[totDF$terms=="Fine Root P Pool"], 
                           totDF$eCO2[totDF$terms=="Fine Root P Pool"],
                           totDF$aCO2[totDF$terms=="Coarse Root P Pool"], 
                           totDF$eCO2[totDF$terms=="Coarse Root P Pool"],
                           totDF$aCO2[totDF$terms=="Understorey P Pool"], 
                           totDF$eCO2[totDF$terms=="Understorey P Pool"],
                           totDF$aCO2[totDF$terms=="Understorey Litter P Pool"], 
                           totDF$eCO2[totDF$terms=="Understorey Litter P Pool"],
                           totDF$aCO2[totDF$terms=="Standing Dead Wood P Pool"], 
                           totDF$eCO2[totDF$terms=="Standing Dead Wood P Pool"],
                           totDF$aCO2[totDF$terms=="Soil P Pool 0-10cm"], 
                           totDF$eCO2[totDF$terms=="Soil P Pool 0-10cm"],
                           totDF$aCO2[totDF$terms=="Soil P Pool 10-30cm"], 
                           totDF$eCO2[totDF$terms=="Soil P Pool 10-30cm"],
                           totDF$aCO2[totDF$terms=="Soil P Pool 30-60cm"], 
                           totDF$eCO2[totDF$terms=="Soil P Pool 30-60cm"],
                           totDF$aCO2[totDF$terms=="Soil Inorg P Pool 0-10cm"], 
                           totDF$eCO2[totDF$terms=="Soil Inorg P Pool 0-10cm"],
                           totDF$aCO2[totDF$terms=="Soil Inorg P Pool 10-30cm"], 
                           totDF$eCO2[totDF$terms=="Soil Inorg P Pool 10-30cm"],
                           totDF$aCO2[totDF$terms=="Soil Inorg P Pool 30-60cm"], 
                           totDF$eCO2[totDF$terms=="Soil Inorg P Pool 30-60cm"],
                           totDF$aCO2[totDF$terms=="Soil Org P Pool 0-10cm"], 
                           totDF$eCO2[totDF$terms=="Soil Org P Pool 0-10cm"],
                           totDF$aCO2[totDF$terms=="Soil Org P Pool 10-30cm"], 
                           totDF$eCO2[totDF$terms=="Soil Org P Pool 10-30cm"],
                           totDF$aCO2[totDF$terms=="Soil Org P Pool 30-60cm"], 
                           totDF$eCO2[totDF$terms=="Soil Org P Pool 30-60cm"],
                           totDF$aCO2[totDF$terms=="Soil Phosphate P Pool 0-10cm"], 
                           totDF$eCO2[totDF$terms=="Soil Phosphate P Pool 0-10cm"],
                           totDF$aCO2[totDF$terms=="Soil Phosphate P Pool 10-30cm"], 
                           totDF$eCO2[totDF$terms=="Soil Phosphate P Pool 10-30cm"],
                           totDF$aCO2[totDF$terms=="Soil Phosphate P Pool 30-60cm"], 
                           totDF$eCO2[totDF$terms=="Soil Phosphate P Pool 30-60cm"],
                           totDF$aCO2[totDF$terms=="Microbial P Pool 0-10cm"], 
                           totDF$eCO2[totDF$terms=="Microbial P Pool 0-10cm"],
                           totDF$aCO2[totDF$terms=="Microbial P Pool 10-30cm"], 
                           totDF$eCO2[totDF$terms=="Microbial P Pool 10-30cm"],
                           totDF$aCO2[totDF$terms=="Microbial P Pool 30-60cm"], 
                           totDF$eCO2[totDF$terms=="Microbial P Pool 30-60cm"]), 
                         NA, NA)
    colnames(plotDF) <- c("mean", "sd", "Variable")
    plotDF$sd <- c(totDF$aCO2_sd[totDF$terms=="Canopy P Pool"], 
                   totDF$eCO2_sd[totDF$terms=="Canopy P Pool"],
                   totDF$aCO2_sd[totDF$terms=="Total Wood P Pool"], 
                   totDF$eCO2_sd[totDF$terms=="Total Wood P Pool"],
                   totDF$aCO2_sd[totDF$terms=="Sapwood P Pool"], 
                   totDF$eCO2_sd[totDF$terms=="Sapwood P Pool"],
                   totDF$aCO2_sd[totDF$terms=="Heartwood P Pool"], 
                   totDF$eCO2_sd[totDF$terms=="Heartwood P Pool"],
                   totDF$aCO2_sd[totDF$terms=="Forestfloor Leaf Litter P Pool"], 
                   totDF$eCO2_sd[totDF$terms=="Forestfloor Leaf Litter P Pool"],
                   totDF$aCO2_sd[totDF$terms=="Fine Root P Pool"], 
                   totDF$eCO2_sd[totDF$terms=="Fine Root P Pool"],
                   totDF$aCO2_sd[totDF$terms=="Coarse Root P Pool"], 
                   totDF$eCO2_sd[totDF$terms=="Coarse Root P Pool"],
                   totDF$aCO2_sd[totDF$terms=="Understorey P Pool"], 
                   totDF$eCO2_sd[totDF$terms=="Understorey P Pool"],
                   totDF$aCO2_sd[totDF$terms=="Understorey Litter P Pool"], 
                   totDF$eCO2_sd[totDF$terms=="Understorey Litter P Pool"],
                   totDF$aCO2_sd[totDF$terms=="Standing Dead Wood P Pool"], 
                   totDF$eCO2_sd[totDF$terms=="Standing Dead Wood P Pool"],
                   totDF$aCO2_sd[totDF$terms=="Soil P Pool 0-10cm"], 
                   totDF$eCO2_sd[totDF$terms=="Soil P Pool 0-10cm"],
                   totDF$aCO2_sd[totDF$terms=="Soil P Pool 10-30cm"], 
                   totDF$eCO2_sd[totDF$terms=="Soil P Pool 10-30cm"],
                   totDF$aCO2_sd[totDF$terms=="Soil P Pool 30-60cm"], 
                   totDF$eCO2_sd[totDF$terms=="Soil P Pool 30-60cm"],
                   totDF$aCO2_sd[totDF$terms=="Soil Inorg P Pool 0-10cm"], 
                   totDF$eCO2_sd[totDF$terms=="Soil Inorg P Pool 0-10cm"],
                   totDF$aCO2_sd[totDF$terms=="Soil Inorg P Pool 10-30cm"], 
                   totDF$eCO2_sd[totDF$terms=="Soil Inorg P Pool 10-30cm"],
                   totDF$aCO2_sd[totDF$terms=="Soil Inorg P Pool 30-60cm"], 
                   totDF$eCO2_sd[totDF$terms=="Soil Inorg P Pool 30-60cm"],
                   totDF$aCO2_sd[totDF$terms=="Soil Org P Pool 0-10cm"], 
                   totDF$eCO2_sd[totDF$terms=="Soil Org P Pool 0-10cm"],
                   totDF$aCO2_sd[totDF$terms=="Soil Org P Pool 10-30cm"], 
                   totDF$eCO2_sd[totDF$terms=="Soil Org P Pool 10-30cm"],
                   totDF$aCO2_sd[totDF$terms=="Soil Org P Pool 30-60cm"], 
                   totDF$eCO2_sd[totDF$terms=="Soil Org P Pool 30-60cm"],
                   totDF$aCO2_sd[totDF$terms=="Soil Phosphate P Pool 0-10cm"], 
                   totDF$eCO2_sd[totDF$terms=="Soil Phosphate P Pool 0-10cm"],
                   totDF$aCO2_sd[totDF$terms=="Soil Phosphate P Pool 10-30cm"], 
                   totDF$eCO2_sd[totDF$terms=="Soil Phosphate P Pool 10-30cm"],
                   totDF$aCO2_sd[totDF$terms=="Soil Phosphate P Pool 30-60cm"], 
                   totDF$eCO2_sd[totDF$terms=="Soil Phosphate P Pool 30-60cm"],
                   totDF$aCO2_sd[totDF$terms=="Microbial P Pool 0-10cm"], 
                   totDF$eCO2_sd[totDF$terms=="Microbial P Pool 0-10cm"],
                   totDF$aCO2_sd[totDF$terms=="Microbial P Pool 10-30cm"], 
                   totDF$eCO2_sd[totDF$terms=="Microbial P Pool 10-30cm"],
                   totDF$aCO2_sd[totDF$terms=="Microbial P Pool 30-60cm"], 
                   totDF$eCO2_sd[totDF$terms=="Microbial P Pool 30-60cm"])
    plotDF$Variable <- rep(c("Canopy P Pool",
                             "Total Wood P Pool",
                             "Sapwood P Pool",
                             "Heartwood P Pool",
                             "Forestfloor Leaf Litter P Pool",
                             "Fine Root P Pool",
                             "Coarse Root P Pool",
                             "Understorey P Pool",
                             "Understorey Litter P Pool",
                             "Standing Dead Wood P Pool",
                             "Soil P Pool 0-10cm",
                             "Soil P Pool 10-30cm",
                             "Soil P Pool 30-60cm",
                             "Soil Inorg P Pool 0-10cm",
                             "Soil Inorg P Pool 10-30cm",
                             "Soil Inorg P Pool 30-60cm",
                             "Soil Org P Pool 0-10cm",
                             "Soil Org P Pool 10-30cm",
                             "Soil Org P Pool 30-60cm",
                             "Soil Phosphate P Pool 0-10cm",
                             "Soil Phosphate P Pool 10-30cm",
                             "Soil Phosphate P Pool 30-60cm",
                             "Microbial P Pool 0-10cm",
                             "Microbial P Pool 10-30cm",
                             "Microbial P Pool 30-60cm"), 
                           each=2)
    plotDF$Trt <- rep(c("aCO2", "eCO2"), 25)
    plotDF$pos <- with(plotDF, mean + sd)
    plotDF$neg <- with(plotDF, mean - sd)
    plotDF$sd2 <- plotDF$sd^2
    
    plotDF$Cat1 <- c(rep("Veg", 4), rep("NA", 4), 
                     rep("Lit", 2), rep("Veg", 6),
                     rep("Lit", 4), rep("Soil_0_10", 2),
                     rep("Soil_10_30", 2), rep("Soil_30_60", 2),
                     rep("NA", 24))
    
    
    plotDF$Cat2 <- c(rep("Veg", 2), rep("NA", 2), 
                     rep("Veg", 4), 
                     rep("Lit", 2), rep("Veg", 6),
                     rep("Lit", 4), rep("Soil_0_10", 2),
                     rep("Soil_10_30", 2), rep("Soil_30_60", 2),
                     rep("NA", 24))
    
    
    ### total ecosystem P pool
    subDF1 <- plotDF[plotDF$Cat1!="NA",]
    plotDF1 <- summaryBy(mean~Cat1+Trt, FUN=sum, data=subDF1, keep.names=T, na.rm=T)
    totDF1 <- summaryBy(mean~Trt, data=plotDF1, FUN=sum, keep.names=T, na.rm=T)
    totDF1$sd[totDF1$Trt=="aCO2"] <- sqrt(sum(subDF1$sd2[subDF1$Trt=="aCO2"])/(dim(subDF1)[1]/2))
    totDF1$sd[totDF1$Trt=="eCO2"] <- sqrt(sum(subDF1$sd2[subDF1$Trt=="eCO2"])/(dim(subDF1)[1]/2))
    
    
    ### vegetation + litter P pool
    subDF2 <- plotDF[plotDF$Cat2%in%c("Veg", "Lit"),]
    plotDF2 <- summaryBy(mean~Variable+Trt, FUN=sum, data=subDF2, keep.names=T, na.rm=T)
    totDF2 <- summaryBy(mean~Trt, data=plotDF2, FUN=sum, keep.names=T, na.rm=T)
    totDF2$sd[totDF2$Trt=="aCO2"] <- sqrt(sum(subDF2$sd2[subDF2$Trt=="aCO2"])/(dim(subDF2)[1]/2))
    totDF2$sd[totDF2$Trt=="eCO2"] <- sqrt(sum(subDF2$sd2[subDF2$Trt=="eCO2"])/(dim(subDF2)[1]/2))
    
    
    ### 0-10 cm soil
    subDF3 <- plotDF[plotDF$Variable%in%c("Soil Inorg P Pool 0-10cm",
                                          "Soil Org P Pool 0-10cm",
                                          "Microbial P Pool 0-10cm",
                                          "Soil Phosphate P Pool 0-10cm"),]
    
    tmpDF <- subDF3[1:2,]
    tmpDF$Variable <- "Soil Org Residual P Pool 0-10cm"
    subDF3 <- rbind(subDF3, tmpDF)
    
    for (i in c("aCO2", "eCO2")) {
        subDF3$mean[subDF3$Variable=="Soil Org Residual P Pool 0-10cm"&subDF3$Trt==i] <- (subDF3$mean[subDF3$Variable=="Soil Org P Pool 0-10cm"&subDF3$Trt==i] -
            subDF3$mean[subDF3$Variable=="Microbial P Pool 0-10cm"&subDF3$Trt==i] - subDF3$mean[subDF3$Variable=="Soil Phosphate P Pool 0-10cm"&subDF3$Trt==i])
        
        subDF3$sd[subDF3$Variable=="Soil Org Residual P Pool 0-10cm"] <- sqrt((subDF3$sd2[subDF3$Variable=="Soil Org P Pool 0-10cm"&subDF3$Trt==i]+
                                                                                   subDF3$sd2[subDF3$Variable=="Microbial P Pool 0-10cm"&subDF3$Trt==i] + subDF3$sd2[subDF3$Variable=="Soil Phosphate P Pool 0-10cm"&subDF3$Trt==i])/3)
    }
    
    subDF3$pos[subDF3$Variable=="Soil Org Residual P Pool 0-10cm"] <- subDF3$mean[subDF3$Variable=="Soil Org Residual P Pool 0-10cm"] +
        subDF3$sd[subDF3$Variable=="Soil Org Residual P Pool 0-10cm"]
    
    subDF3$neg[subDF3$Variable=="Soil Org Residual P Pool 0-10cm"] <- subDF3$mean[subDF3$Variable=="Soil Org Residual P Pool 0-10cm"] -
        subDF3$neg[subDF3$Variable=="Soil Org Residual P Pool 0-10cm"]
    
    
    subDF3$sd2[subDF3$Variable=="Soil Org Residual P Pool 0-10cm"] <- NA
    
    subDF3 <- subDF3[subDF3$Variable!="Soil Org P Pool 0-10cm",]
    totDF3 <- plotDF[plotDF$Variable=="Soil P Pool 0-10cm",]
    
    
    
    
    ### 10-30 cm soil
    subDF4 <- plotDF[plotDF$Variable%in%c("Soil Inorg P Pool 10-30cm",
                                          "Soil Org P Pool 10-30cm",
                                          "Microbial P Pool 10-30cm",
                                          "Soil Phosphate P Pool 10-30cm"),]
    
    tmpDF <- subDF4[1:2,]
    tmpDF$Variable <- "Soil Org Residual P Pool 10-30cm"
    subDF4 <- rbind(subDF4, tmpDF)
    
    for (i in c("aCO2", "eCO2")) {
        subDF4$mean[subDF4$Variable=="Soil Org Residual P Pool 10-30cm"&subDF4$Trt==i] <- (subDF4$mean[subDF4$Variable=="Soil Org P Pool 10-30cm"&subDF4$Trt==i] -
                                                                                              subDF4$mean[subDF4$Variable=="Microbial P Pool 10-30cm"&subDF4$Trt==i] - subDF4$mean[subDF4$Variable=="Soil Phosphate P Pool 10-30cm"&subDF4$Trt==i])
        
        subDF4$sd[subDF4$Variable=="Soil Org Residual P Pool 10-30cm"] <- sqrt((subDF4$sd2[subDF4$Variable=="Soil Org P Pool 10-30cm"&subDF4$Trt==i]+
                                                                                   subDF4$sd2[subDF4$Variable=="Microbial P Pool 10-30cm"&subDF4$Trt==i] + subDF4$sd2[subDF4$Variable=="Soil Phosphate P Pool 10-30cm"&subDF4$Trt==i])/3)
    }
    
    subDF4$pos[subDF4$Variable=="Soil Org Residual P Pool 10-30cm"] <- subDF4$mean[subDF4$Variable=="Soil Org Residual P Pool 10-30cm"] +
        subDF4$sd[subDF4$Variable=="Soil Org Residual P Pool 10-30cm"]
    
    subDF4$neg[subDF4$Variable=="Soil Org Residual P Pool 10-30cm"] <- subDF4$mean[subDF4$Variable=="Soil Org Residual P Pool 10-30cm"] -
        subDF4$neg[subDF4$Variable=="Soil Org Residual P Pool 10-30cm"]
    
    
    subDF4$sd2[subDF4$Variable=="Soil Org Residual P Pool 10-30cm"] <- NA
    
    subDF4 <- subDF4[subDF4$Variable!="Soil Org P Pool 10-30cm",]
    totDF4 <- plotDF[plotDF$Variable=="Soil P Pool 10-30cm",]
    
    
    
    
    ### 30-60 cm soil
    subDF5 <- plotDF[plotDF$Variable%in%c("Soil Inorg P Pool 30-60cm",
                                          "Soil Org P Pool 30-60cm",
                                          "Microbial P Pool 30-60cm",
                                          "Soil Phosphate P Pool 30-60cm"),]
    
    tmpDF <- subDF3[1:2,]
    tmpDF$Variable <- "Soil Org Residual P Pool 30-60cm"
    subDF5 <- rbind(subDF5, tmpDF)
    
    for (i in c("aCO2", "eCO2")) {
        subDF5$mean[subDF5$Variable=="Soil Org Residual P Pool 30-60cm"&subDF5$Trt==i] <- (subDF5$mean[subDF5$Variable=="Soil Org P Pool 30-60cm"&subDF5$Trt==i] -
                                                                                              subDF5$mean[subDF5$Variable=="Microbial P Pool 30-60cm"&subDF5$Trt==i] - subDF5$mean[subDF5$Variable=="Soil Phosphate P Pool 30-60cm"&subDF5$Trt==i])
        
        subDF5$sd[subDF5$Variable=="Soil Org Residual P Pool 30-60cm"] <- sqrt((subDF5$sd2[subDF5$Variable=="Soil Org P Pool 30-60cm"&subDF5$Trt==i]+
                                                                                   subDF5$sd2[subDF5$Variable=="Microbial P Pool 30-60cm"&subDF5$Trt==i] + subDF5$sd2[subDF5$Variable=="Soil Phosphate P Pool 30-60cm"&subDF5$Trt==i])/3)
    }
    
    subDF5$pos[subDF5$Variable=="Soil Org Residual P Pool 30-60cm"] <- subDF5$mean[subDF5$Variable=="Soil Org Residual P Pool 30-60cm"] +
        subDF5$sd[subDF5$Variable=="Soil Org Residual P Pool 30-60cm"]
    
    subDF5$neg[subDF5$Variable=="Soil Org Residual P Pool 30-60cm"] <- subDF5$mean[subDF5$Variable=="Soil Org Residual P Pool 30-60cm"] -
        subDF5$neg[subDF5$Variable=="Soil Org Residual P Pool 30-60cm"]
    
    
    subDF5$sd2[subDF5$Variable=="Soil Org Residual P Pool 30-60cm"] <- NA
    
    subDF5 <- subDF5[subDF5$Variable!="Soil Org P Pool 30-60cm",]
    totDF5 <- plotDF[plotDF$Variable=="Soil P Pool 30-60cm",]
    
    
    ### plot total ecosystem P pool
    p1 <- ggplot(plotDF1, aes(x=Trt, y=mean))+
        geom_bar(aes(fill=Cat1), 
                 stat = "identity", position="stack")+
        geom_errorbar(data=totDF1, aes(x=Trt, ymax=mean+sd, ymin=mean-sd), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        geom_point(data=totDF1, aes(x=Trt, y=mean), pch=19, col="black", size=2)+
        labs(x="", y=expression(paste("Ecosystem P pool (g P ", m^-2, ")")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right")+
        scale_fill_manual(name="", 
                          values = c("Veg" = cbPalette[2],
                                     "Lit" = cbPalette[7],
                                     "Soil_0_10" = cbPalette[4],
                                     "Soil_10_30" = cbPalette[5],
                                     "Soil_30_60" = cbPalette[6]),
                          labels=c("Veg"="Vegetation",
                                   "Lit"="Litter",
                                   "Soil_0_10"=expression(Soil['0-10cm']),
                                   "Soil_10_30"=expression(Soil['10-30cm']),
                                   "Soil_30_60"=expression(Soil['30-60cm'])))+
        scale_x_discrete(limits=c("aCO2", "eCO2"),
                         labels=c("aCO2"=expression(aCO[2]),
                                  "eCO2"=expression(eCO[2])))
        
    
    ### plot vegetation P pool
    p2 <- ggplot(plotDF2, aes(x=Trt, y=mean))+
        geom_bar(aes(fill=Variable), 
                 stat = "identity", position="stack")+
        geom_errorbar(data=totDF2, aes(x=Trt, ymax=mean+sd, ymin=mean-sd), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        geom_point(data=totDF2, aes(x=Trt, y=mean), pch=19, col="black", size=2)+
        labs(x="", y=expression(paste("Plant + Litter P pool (g P ", m^-2, ")")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right")+
        scale_fill_manual(name="", 
                          values = c("Canopy P Pool" = SpectralPalette[1],
                                     "Coarse Root P Pool" = SpectralPalette[2],
                                     "Fine Root P Pool" = SpectralPalette[3],
                                     "Forestfloor Leaf Litter P Pool" = SpectralPalette[4],
                                     "Heartwood P Pool" = SpectralPalette[5],
                                     "Sapwood P Pool" = SpectralPalette[6],
                                     "Standing Dead Wood P Pool" = SpectralPalette[7],
                                     "Understorey Litter P Pool" = SpectralPalette[8],
                                     "Understorey P Pool" = SpectralPalette[9]),
                          labels=c("Canopy P Pool"="Canopy",
                                   "Coarse Root P Pool"="Coarseroot",
                                   "Fine Root P Pool"="Fineroot",
                                   "Forestfloor Leaf Litter P Pool"="Forestfloor leaf litter",
                                   "Heartwood P Pool"="Heartwood",
                                   "Sapwood P Pool"="Sapwood",
                                   "Standing Dead Wood P Pool"="Standing dead wood",
                                   "Understorey Litter P Pool"="Understorey litter",
                                   "Understorey P Pool"="Understorey"))+
        scale_x_discrete(limits=c("aCO2", "eCO2"),
                         labels=c("aCO2"=expression(aCO[2]),
                                  "eCO2"=expression(eCO[2])))
    
    
    
    ### plot soil P pool 0 - 10 cm
    p3 <- ggplot(subDF3, aes(x=Trt, y=mean))+
        geom_bar(aes(fill=Variable), 
                 stat = "identity", position="stack")+
        geom_errorbar(data=totDF3, aes(x=Trt, ymax=mean+sd, ymin=mean-sd), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        geom_point(data=totDF3, aes(x=Trt, y=mean), pch=19, col="black", size=2)+
        labs(x="0 - 10 cm", y=expression(paste("Soil P pool (g P ", m^-2, ")")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", 
                          values = c("Soil Inorg P Pool 0-10cm" = Diverge_hsv_Palette[3],
                                     "Soil Phosphate P Pool 0-10cm" = Diverge_hsv_Palette[7],
                                     "Microbial P Pool 0-10cm" = Diverge_hsv_Palette[1],
                                     "Soil Org Residual P Pool 0-10cm" = Diverge_hsv_Palette[5]),
                          labels=c("Soil Inorg P Pool 0-10cm"=expression(P[inorg]),
                                   "Soil Phosphate P Pool 0-10cm"=expression(PO[4]-P),
                                   "Microbial P Pool 0-10cm"="Microbial P",
                                   "Soil Org Residual P Pool 0-10cm"=expression(P[org] * " residual")))+
        scale_x_discrete(limits=c("aCO2", "eCO2"),
                         labels=c("aCO2"=expression(aCO[2]),
                                  "eCO2"=expression(eCO[2])))


    
    
    ### plot soil P pool 10 - 30 cm
    p4 <- ggplot(subDF4, aes(x=Trt, y=mean))+
        geom_bar(aes(fill=Variable), 
                 stat = "identity", position="stack")+
        geom_errorbar(data=totDF4, aes(x=Trt, ymax=mean+sd, ymin=mean-sd), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        geom_point(data=totDF4, aes(x=Trt, y=mean), pch=19, col="black", size=2)+
        labs(x="10 - 30 cm", y=expression(paste("Soil P pool (g P ", m^-2, ")")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", 
                          values = c("Soil Inorg P Pool 10-30cm" = Diverge_hsv_Palette[3],
                                     "Soil Phosphate P Pool 10-30cm" = Diverge_hsv_Palette[7],
                                     "Microbial P Pool 10-30cm" = Diverge_hsv_Palette[1],
                                     "Soil Org Residual P Pool 10-30cm" = Diverge_hsv_Palette[5]),
                          labels=c("Soil Inorg P Pool 10-30cm"=expression(P[inorg]),
                                   "Soil Phosphate P Pool 10-30cm"=expression(PO[4]-P),
                                   "Microbial P Pool 10-30cm"="Microbial P",
                                   "Soil Org Residual P Pool 10-30cm"=expression(P[org] * " residual")))+
        scale_x_discrete(limits=c("aCO2", "eCO2"),
                         labels=c("aCO2"=expression(aCO[2]),
                                  "eCO2"=expression(eCO[2])))
    
    
    
    ### plot soil P pool 30 - 60 cm
    p5 <- ggplot(subDF5, aes(x=Trt, y=mean))+
        geom_bar(aes(fill=Variable), 
                 stat = "identity", position="stack")+
        geom_errorbar(data=totDF5, aes(x=Trt, ymax=mean+sd, ymin=mean-sd), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        geom_point(data=totDF5, aes(x=Trt, y=mean), pch=19, col="black", size=2)+
        labs(x="30 - 60 cm", y=expression(paste("Soil P pool (g P ", m^-2, ")")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", 
                          values = c("Soil Inorg P Pool 30-60cm" = Diverge_hsv_Palette[3],
                                     "Soil Phosphate P Pool 30-60cm" = Diverge_hsv_Palette[7],
                                     "Microbial P Pool 30-60cm" = Diverge_hsv_Palette[1],
                                     "Soil Org Residual P Pool 30-60cm" = Diverge_hsv_Palette[5]),
                          labels=c("Soil Inorg P Pool 30-60cm"=expression(P[inorg]),
                                   "Soil Phosphate P Pool 30-60cm"=expression(PO[4]-P),
                                   "Microbial P Pool 30-60cm"="Microbial P",
                                   "Soil Org Residual P Pool 30-60cm"=expression(P[org] * " residual")))+
        scale_x_discrete(limits=c("aCO2", "eCO2"),
                         labels=c("aCO2"=expression(aCO[2]),
                                  "eCO2"=expression(eCO[2])))
    
    
    
    
    legend_bot_row <- get_legend(p3 + theme(legend.position="bottom",
                                            legend.box = 'horizontal',
                                            legend.box.just = 'left'))
    
    
    
    grid.labs <- c("(a)", "(b)", "(c)", "(d)", "(e)")
    
    
    ## plot 
    pdf(paste0("plots_tables/output/", norm, "/P_Pools_Summary_Plots_", norm, "_2.pdf"),
        width=10,height=8)
    top_row <- plot_grid(p1, p2, ncol=2)
    bot_row <- plot_grid(p3, p4, p5, ncol=3)
    plot_grid(top_row, bot_row, legend_bot_row,
              ncol = 1, rel_widths = c(1, 1, 1),
              rel_heights=c(1, 1, 0.2))
    grid.text(grid.labs,x = c(0.08, 0.76, 0.1, 0.42, 0.75), y = c(0.97, 0.97, 0.5, 0.5, 0.5),
              gp=gpar(fontsize=16, col="black", fontface="bold"))
    dev.off()
    
    
    
    
}


