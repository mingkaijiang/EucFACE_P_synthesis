make_p_concentration_summary_plots <- function(inDF,norm) {
    
    
    ### convert unit
    inDF$aCO2 <- inDF$aCO2 * 10
    inDF$eCO2 <- inDF$eCO2 * 10
    inDF$aCO2_sd <- inDF$aCO2_sd * 10
    inDF$eCO2_sd <- inDF$eCO2_sd * 10
    
    
    
    ### Plot 1
    plotDF1 <- data.frame(c(inDF$aCO2[inDF$conc.terms=="Canopy P Conc"], 
                            inDF$eCO2[inDF$conc.terms=="Canopy P Conc"],
                            inDF$aCO2[inDF$conc.terms=="Leaflitter P Conc"], 
                            inDF$eCO2[inDF$conc.terms=="Leaflitter P Conc"]), 
                          NA, NA)
    colnames(plotDF1) <- c("mean", "sd", "Variable")
    plotDF1$sd <- c(inDF$aCO2_sd[inDF$conc.terms=="Canopy P Conc"], 
                    inDF$eCO2_sd[inDF$conc.terms=="Canopy P Conc"],
                    inDF$aCO2_sd[inDF$conc.terms=="Leaflitter P Conc"], 
                    inDF$eCO2_sd[inDF$conc.terms=="Leaflitter P Conc"])
    plotDF1$Variable <- rep(c("Canopy", "Canopy leaflitter"), each=2)
    plotDF1$Trt <- rep(c("aCO2", "eCO2"), 2)
    plotDF1$pos <- with(plotDF1, mean + sd)
    plotDF1$neg <- with(plotDF1, mean - sd)
    
    ### Plot 2
    plotDF2 <- data.frame(c(inDF$aCO2[inDF$conc.terms=="Understorey P Conc"], 
                            inDF$eCO2[inDF$conc.terms=="Understorey P Conc"],
                            inDF$aCO2[inDF$conc.terms=="Understorey Litter P Conc"], 
                            inDF$eCO2[inDF$conc.terms=="Understorey Litter P Conc"]), 
                          NA, NA)
    colnames(plotDF2) <- c("mean", "sd", "Variable")
    plotDF2$sd <- c(inDF$aCO2_sd[inDF$conc.terms=="Understorey P Conc"], 
                    inDF$eCO2_sd[inDF$conc.terms=="Understorey P Conc"],
                    inDF$aCO2_sd[inDF$conc.terms=="Understorey Litter P Conc"], 
                    inDF$eCO2_sd[inDF$conc.terms=="Understorey Litter P Conc"])
    plotDF2$Variable <- rep(c("Understorey leaf", "Understorey leaflitter"), each=2)
    plotDF2$Trt <- rep(c("aCO2", "eCO2"), 2)
    plotDF2$pos <- with(plotDF2, mean + sd)
    plotDF2$neg <- with(plotDF2, mean - sd)
    
    
    ### Plot 3
    plotDF3 <- data.frame(c(inDF$aCO2[inDF$conc.terms=="Fine Root P Conc"], 
                            inDF$eCO2[inDF$conc.terms=="Fine Root P Conc"],
                            inDF$aCO2[inDF$conc.terms=="Sapwood P Conc"], 
                            inDF$eCO2[inDF$conc.terms=="Sapwood P Conc"]), 
                          NA, NA)
    colnames(plotDF3) <- c("mean", "sd", "Variable")
    plotDF3$sd <- c(inDF$aCO2_sd[inDF$conc.terms=="Fine Root P Conc"], 
                    inDF$eCO2_sd[inDF$conc.terms=="Fine Root P Conc"],
                    inDF$aCO2_sd[inDF$conc.terms=="Sapwood P Conc"], 
                    inDF$eCO2_sd[inDF$conc.terms=="Sapwood P Conc"])
    plotDF3$Variable <- rep(c("Fine Root", "Sapwood"), each=2)
    plotDF3$Trt <- rep(c("aCO2", "eCO2"), 2)
    plotDF3$pos <- with(plotDF3, mean + sd)
    plotDF3$neg <- with(plotDF3, mean - sd)
    
    ### Plot 4
    plotDF4 <- data.frame(c(inDF$aCO2[inDF$conc.terms=="Frass P Conc"], 
                            inDF$eCO2[inDF$conc.terms=="Frass P Conc"]), 
                          NA, NA)
    colnames(plotDF4) <- c("mean", "sd", "Variable")
    plotDF4$sd <- c(inDF$aCO2_sd[inDF$conc.terms=="Frass P Conc"], 
                    inDF$eCO2_sd[inDF$conc.terms=="Frass P Conc"])
    plotDF4$Variable <- rep(c("Frass"), each=2)
    plotDF4$Trt <- rep(c("aCO2", "eCO2"), 1)
    plotDF4$pos <- with(plotDF4, mean + sd)
    plotDF4$neg <- with(plotDF4, mean - sd)
    
    
    plotDF1 <- rbind(plotDF1, plotDF4)
    
    ### Plot 5
    plotDF5 <- data.frame(c(inDF$aCO2[inDF$conc.terms=="Soil P Conc 0-10cm"], 
                            inDF$eCO2[inDF$conc.terms=="Soil P Conc 0-10cm"],
                            inDF$aCO2[inDF$conc.terms=="Microbial P Conc 0-10cm"], 
                            inDF$eCO2[inDF$conc.terms=="Microbial P Conc 0-10cm"]), 
                          NA, NA)
    colnames(plotDF5) <- c("mean", "sd", "Variable")
    plotDF5$sd <- c(inDF$aCO2_sd[inDF$conc.terms=="Soil P Conc 0-10cm"], 
                    inDF$eCO2_sd[inDF$conc.terms=="Soil P Conc 0-10cm"],
                    inDF$aCO2_sd[inDF$conc.terms=="Microbial P Conc 0-10cm"], 
                    inDF$eCO2_sd[inDF$conc.terms=="Microbial P Conc 0-10cm"])
    plotDF5$Variable <- rep(c("Soil 0-10cm", "Microbe 0-10cm"), each=2)
    plotDF5$Trt <- rep(c("aCO2", "eCO2"), 2)
    plotDF5$pos <- with(plotDF5, mean + sd)
    plotDF5$neg <- with(plotDF5, mean - sd)
    
    ### Plot 6
    plotDF6 <- data.frame(c(inDF$aCO2[inDF$conc.terms=="Soil Phosphate P Conc 0-10cm"], 
                            inDF$eCO2[inDF$conc.terms=="Soil Phosphate P Conc 0-10cm"]), 
                          NA, NA)
    colnames(plotDF6) <- c("mean", "sd", "Variable")
    plotDF6$sd <- c(inDF$aCO2_sd[inDF$conc.terms=="Soil Phosphate P Conc 0-10cm"], 
                    inDF$eCO2_sd[inDF$conc.terms=="Soil Phosphate P Conc 0-10cm"])
    plotDF6$Variable <- rep(c("Soil Phosphate 0-10cm"), each=2)
    plotDF6$Trt <- rep(c("aCO2", "eCO2"), 1)
    plotDF6$pos <- with(plotDF6, mean + sd)
    plotDF6$neg <- with(plotDF6, mean - sd)

    
    ### plotDF7
    plotDF7 <- data.frame(c(inDF$aCO2[inDF$conc.terms=="Soil P Conc 10-30cm"], 
                            inDF$eCO2[inDF$conc.terms=="Soil P Conc 10-30cm"],
                            inDF$aCO2[inDF$conc.terms=="Microbial P Conc 10-30cm"], 
                            inDF$eCO2[inDF$conc.terms=="Microbial P Conc 10-30cm"]), 
                          NA, NA)
    colnames(plotDF7) <- c("mean", "sd", "Variable")
    plotDF7$sd <- c(inDF$aCO2_sd[inDF$conc.terms=="Soil P Conc 10-30cm"], 
                    inDF$eCO2_sd[inDF$conc.terms=="Soil P Conc 10-30cm"],
                    inDF$aCO2_sd[inDF$conc.terms=="Microbial P Conc 10-30cm"], 
                    inDF$eCO2_sd[inDF$conc.terms=="Microbial P Conc 10-30cm"])
    plotDF7$Variable <- rep(c("Soil 10-30cm", "Microbe 10-30cm"), each=2)
    plotDF7$Trt <- rep(c("aCO2", "eCO2"), 2)
    plotDF7$pos <- with(plotDF7, mean + sd)
    plotDF7$neg <- with(plotDF7, mean - sd)
    
    
    ### plotDF8
    plotDF8 <- data.frame(c(inDF$aCO2[inDF$conc.terms=="Soil P Conc 30-60cm"], 
                            inDF$eCO2[inDF$conc.terms=="Soil P Conc 30-60cm"],
                            inDF$aCO2[inDF$conc.terms=="Microbial P Conc 30-60cm"], 
                            inDF$eCO2[inDF$conc.terms=="Microbial P Conc 30-60cm"]), 
                          NA, NA)
    colnames(plotDF8) <- c("mean", "sd", "Variable")
    plotDF8$sd <- c(inDF$aCO2_sd[inDF$conc.terms=="Soil P Conc 30-60cm"], 
                    inDF$eCO2_sd[inDF$conc.terms=="Soil P Conc 30-60cm"],
                    inDF$aCO2_sd[inDF$conc.terms=="Microbial P Conc 30-60cm"], 
                    inDF$eCO2_sd[inDF$conc.terms=="Microbial P Conc 30-60cm"])
    plotDF8$Variable <- rep(c("Soil 30-60cm", "Microbe 30-60cm"), each=2)
    plotDF8$Trt <- rep(c("aCO2", "eCO2"), 2)
    plotDF8$pos <- with(plotDF8, mean + sd)
    plotDF8$neg <- with(plotDF8, mean - sd)
    
    
    tmpDF <- rbind(plotDF5, plotDF7, plotDF8)
    
    plotDF9 <- tmpDF[tmpDF$Variable%in%c("Soil 0-10cm", "Soil 10-30cm", "Soil 30-60cm"),]
    plotDF10 <- tmpDF[tmpDF$Variable%in%c("Microbe 0-10cm", "Microbe 10-30cm", "Microbe 30-60cm"),]
    plotDF11 <- rbind(plotDF1, plotDF2, plotDF3)
    
    
    ### Plotting
    p1 <- ggplot(plotDF1, aes(x=Variable, y=mean))+
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge")+
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        labs(x="", y=expression("P concentration (mg " * g^-1 * ")"))+
        theme_linedraw() +
        ylim(0,1.5)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", values = c("aCO2" = SpectralPalette[7], "eCO2" = SpectralPalette[3]),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))
    

    p2 <- ggplot(plotDF2, aes(x=Variable, y=mean))+
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge")+
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        labs(x="", y=expression("P concentration (mg " * g^-1 * ")"))+
        theme_linedraw() +
        ylim(0,1.0)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", values = c("aCO2" = SpectralPalette[7], "eCO2" = SpectralPalette[3]),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))

    p3 <- ggplot(plotDF3, aes(x=Variable, y=mean))+
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge")+
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        labs(x="", y=expression("P concentration (mg " * g^-1 * ")"))+
        theme_linedraw() +
        ylim(0,0.5)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", values = c("aCO2" = SpectralPalette[7], "eCO2" = SpectralPalette[3]),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))
    
    p4 <- ggplot(plotDF4, aes(x=Variable, y=mean))+
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge")+
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        labs(x="", y=expression("P concentration (mg " * g^-1 * ")"))+
        theme_linedraw() +
        ylim(0,1.5)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", values = c("aCO2" = SpectralPalette[7], "eCO2" = SpectralPalette[3]),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))
    
    p5 <- ggplot(plotDF5, aes(x=Variable, y=mean))+
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge")+
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        labs(x="", y=expression("P concentration (mg " * g^-1 * ")"))+
        theme_linedraw() +
        ylim(0,0.1)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", values = c("aCO2" = SpectralPalette[7], "eCO2" = SpectralPalette[3]),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))
    
    p6 <- ggplot(plotDF6, aes(x=Variable, y=mean))+
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge")+
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        labs(x="", y=expression("P concentration (mg " * g^-1 * ")"))+
        theme_linedraw() +
        ylim(0,0.003)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", values = c("aCO2" = SpectralPalette[7], "eCO2" = SpectralPalette[3]),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))
    
    
    p7 <- ggplot(plotDF7, aes(x=Variable, y=mean))+
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge")+
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        labs(x="", y=expression("P concentration (mg " * g^-1 * ")"))+
        theme_linedraw() +
        ylim(0,0.1)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", values = c("aCO2" = SpectralPalette[7], "eCO2" = SpectralPalette[3]),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))
    
    
    p8 <- ggplot(plotDF8, aes(x=Variable, y=mean))+
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge")+
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        labs(x="", y=expression("P concentration (mg " * g^-1 * ")"))+
        theme_linedraw() +
        ylim(0,0.1)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", values = c("aCO2" = SpectralPalette[7], "eCO2" = SpectralPalette[3]),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))
    
    
    p9 <- ggplot(plotDF9, aes(x=Variable, y=mean))+
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge")+
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        labs(x="", y=expression("P concentration (mg " * g^-1 * ")"))+
        theme_linedraw() +
        ylim(0,0.1)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", values = c("aCO2" = SpectralPalette[7], "eCO2" = SpectralPalette[3]),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))
    
    
    p10 <- ggplot(plotDF10, aes(x=Variable, y=mean))+
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge")+
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        labs(x="", y=expression("P concentration (mg " * g^-1 * ")"))+
        theme_linedraw() +
        ylim(0,0.04)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", values = c("aCO2" = SpectralPalette[7], "eCO2" = SpectralPalette[3]),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))
    
    
    p11 <- ggplot(plotDF11, aes(x=Variable, y=mean))+
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge")+
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        labs(x="", y=expression("P concentration (mg " * g^-1 * ")"))+
        theme_linedraw() +
        ylim(0,1.5)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", values = c("aCO2" = SpectralPalette[7], "eCO2" = SpectralPalette[3]),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))
    
    
    #grid.labs <- c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)")
    
    require(grid)
    require(cowplot)
    
    ## plot 
    pdf(paste0("plots_tables/output/", norm, "/P_Concentration_Summary_Plots_", norm, ".pdf"),
        width=12,height=8)
    bot_row <- plot_grid(p10, p9, p6, ncol=3, rel_widths = c(1., 1., 0.5))
    plot_grid(p11, bot_row,  ncol = 1,
              rel_heights=c(1, 1))
    grid.text(grid.labs,x = c(0.1, 0.1, 0.48, 0.88), y = c(0.95, 0.45, 0.45, 0.45),
              gp=gpar(fontsize=16, col="black", fontface="bold"))
    dev.off()
    
 }


