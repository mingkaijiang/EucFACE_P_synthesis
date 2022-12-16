make_p_pools_summary_plots <- function(inDF,norm) {
    
    
    
    ### extract hedley data
    tmpDF <- inDF[inDF$terms%in%c("Exchangeable Pi Pool", "Exchangeable Po Pool",
                                  "Moderately labile Po Pool", "Secondary Fe bound Pi Pool",
                                  "Primary Ca bound Pi Pool", 
                                  "Occluded P Pool"),]
    
    subDF1 <- data.frame(tmpDF$terms, tmpDF$aCO2, tmpDF$aCO2_sd)
    subDF2 <- data.frame(tmpDF$terms, tmpDF$eCO2, tmpDF$eCO2_sd)
    colnames(subDF1) <- colnames(subDF2) <- c("terms", "mean", "sd")
    hedDF <- rbind(subDF1, subDF2)
    
    hedDF$Pool <- rep(c("1_Exchangeable_Pi", "2_Exchangeable_Po", "3_Moderately_labile_Po",
                          "4_Secondary_Pi", "5_Primary_Pi", "6_Occluded_P"), 2)
    hedDF$Trt <- rep(c("amb", "ele"), each=6)
    
    ### calculate totals
    hedsumDF <- summaryBy(mean~Trt, FUN=sum, data=hedDF, na.rm=T, keep.names=T)
    
    for (i in c("amb", "ele")) {
        hedDF$frac[hedDF$Trt==i] <- hedDF$mean[hedDF$Trt==i] / hedsumDF$mean[hedsumDF$Trt==i]
    }
    
    
    
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
              legend.position="none")+
        scale_fill_manual(name="", values = c("aCO2" = SpectralPalette[7], "eCO2" = SpectralPalette[3]),
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
        scale_fill_manual(name="", values = c("aCO2" = SpectralPalette[7], "eCO2" = SpectralPalette[3]),
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
        scale_fill_manual(name="", values = c("aCO2" = SpectralPalette[7], "eCO2" = SpectralPalette[3]),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_x_discrete(labels=c("Soil Phosphate  0-10cm"=expression("Soil Phosphate (0-10cm)"),
                                "Soil Phosphate  10-30cm"=expression("Soil Phosphate (10-30cm)"),
                                "Soil Phosphate  30-60cm"=expression("Soil Phosphate (30-60cm)")))
    
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
        scale_fill_manual(name="", values = c("aCO2" = SpectralPalette[7], "eCO2" = SpectralPalette[3]),
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
                                  #"Understorey Litter P Pool", 
                                  "Standing Dead Wood P Pool"),]
    
    soilDF1 <- inDF[inDF$terms%in%c("Soil P Pool 0-10cm", "Soil P Pool 10-30cm", 
                                   "Soil P Pool 30-60cm"),]
    
    #soilDF2 <- inDF[inDF$terms%in%c("Soil Inorg P Pool 0-10cm", "Soil Inorg P Pool 10-30cm", 
    #                                "Soil Inorg P Pool 30-60cm"),]
    
    #soilDF3 <- inDF[inDF$terms%in%c("Soil Inorg P Pool 0-10cm", "Soil Inorg P Pool 10-30cm", 
    #                                "Soil Inorg P Pool 30-60cm"),]
    
    
    
    
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
    
    #plotDF$Cat1 <- c(rep("Veg", 4), rep("NA", 4), 
    #                 rep("Lit", 2), rep("Veg", 6),
    #                 rep("NA", 2), 
    #                 rep("Lit", 2), rep("Soil_0_10", 2),
    #                 rep("Soil_10_30", 2), rep("Soil_30_60", 2),
    #                 rep("NA", 24))
    
    plotDF$Cat1 <- c(rep("Veg", 4), rep("NA", 4), 
                     rep("Veg", 2), rep("Veg", 6),
                     rep("NA", 2), 
                     rep("Veg", 2), rep("NA", 6),
                     rep("Soil_Inorg", 6), rep("Soil_Org", 6),
                     rep("NA", 12))
    
    
    #plotDF$Cat2 <- c(rep("Veg", 2), rep("NA", 2), 
    #                 rep("Veg", 4), 
    #                 rep("Lit", 2), rep("Veg", 6),
    #                 rep("NA", 2), 
    #                 rep("Lit", 2), rep("Soil_0_10", 2),
    #                 rep("Soil_10_30", 2), rep("Soil_30_60", 2),
    #                 rep("NA", 24))
    
    plotDF$Cat2 <- c(rep("Veg", 2), rep("NA", 2), 
                     rep("Veg", 4), 
                     rep("Veg", 2), rep("Veg", 6),
                     rep("NA", 2), 
                     rep("Veg", 2), rep("NA", 6),
                     rep("Soil_Inorg", 6), rep("Soil_Org", 6),
                     rep("NA", 12))
    
    
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
    
    tmpDF <- subDF3[1:2,]
    tmpDF$Variable <- "Soil Inorg Residual P Pool 0-10cm"
    subDF3 <- rbind(subDF3, tmpDF)
    
    
    for (i in c("aCO2", "eCO2")) {
        subDF3$mean[subDF3$Variable=="Soil Org Residual P Pool 0-10cm"&subDF3$Trt==i] <- (subDF3$mean[subDF3$Variable=="Soil Org P Pool 0-10cm"&subDF3$Trt==i] -
            subDF3$mean[subDF3$Variable=="Microbial P Pool 0-10cm"&subDF3$Trt==i])
        
        subDF3$mean[subDF3$Variable=="Soil Inorg Residual P Pool 0-10cm"&subDF3$Trt==i] <- (subDF3$mean[subDF3$Variable=="Soil Inorg P Pool 0-10cm"&subDF3$Trt==i] -
                                                                                              subDF3$mean[subDF3$Variable=="Soil Phosphate P Pool 0-10cm"&subDF3$Trt==i])
        
        
        subDF3$sd[subDF3$Variable=="Soil Org Residual P Pool 0-10cm"] <- sqrt((subDF3$sd2[subDF3$Variable=="Soil Org P Pool 0-10cm"&subDF3$Trt==i]+
                                                                                   subDF3$sd2[subDF3$Variable=="Microbial P Pool 0-10cm"&subDF3$Trt==i])/2)
        
        subDF3$sd[subDF3$Variable=="Soil inorg Residual P Pool 0-10cm"] <- sqrt((subDF3$sd2[subDF3$Variable=="Soil Inorg P Pool 0-10cm"&subDF3$Trt==i]+
                                                                                   subDF3$sd2[subDF3$Variable=="Soil Phosphate P Pool 0-10cm"&subDF3$Trt==i])/2)
    }
    
    subDF3$pos[subDF3$Variable=="Soil Org Residual P Pool 0-10cm"] <- subDF3$mean[subDF3$Variable=="Soil Org Residual P Pool 0-10cm"] +
        subDF3$sd[subDF3$Variable=="Soil Org Residual P Pool 0-10cm"]
    
    subDF3$neg[subDF3$Variable=="Soil Org Residual P Pool 0-10cm"] <- subDF3$mean[subDF3$Variable=="Soil Org Residual P Pool 0-10cm"] -
        subDF3$neg[subDF3$Variable=="Soil Org Residual P Pool 0-10cm"]
    
    
    subDF3$pos[subDF3$Variable=="Soil Inorg Residual P Pool 0-10cm"] <- subDF3$mean[subDF3$Variable=="Soil Inorg Residual P Pool 0-10cm"] +
        subDF3$sd[subDF3$Variable=="Soil Inorg Residual P Pool 0-10cm"]
    
    subDF3$neg[subDF3$Variable=="Soil Inorg Residual P Pool 0-10cm"] <- subDF3$mean[subDF3$Variable=="Soil Inorg Residual P Pool 0-10cm"] -
        subDF3$neg[subDF3$Variable=="Soil Inorg Residual P Pool 0-10cm"]
    
    
    subDF3$sd2[subDF3$Variable=="Soil Org Residual P Pool 0-10cm"] <- NA
    subDF3$sd2[subDF3$Variable=="Soil Inorg Residual P Pool 0-10cm"] <- NA
    
    subDF3 <- subDF3[subDF3$Variable!="Soil Org P Pool 0-10cm",]
    subDF3 <- subDF3[subDF3$Variable!="Soil Inorg P Pool 0-10cm",]
    
    totDF3 <- plotDF[plotDF$Variable=="Soil P Pool 0-10cm",]
    
    
    
    
    ### 10-30 cm soil
    subDF4 <- plotDF[plotDF$Variable%in%c("Soil Inorg P Pool 10-30cm",
                                          "Soil Org P Pool 10-30cm",
                                          "Microbial P Pool 10-30cm",
                                          "Soil Phosphate P Pool 10-30cm"),]
    
    tmpDF <- subDF4[1:2,]
    tmpDF$Variable <- "Soil Org Residual P Pool 10-30cm"
    subDF4 <- rbind(subDF4, tmpDF)
    
    tmpDF <- subDF4[1:2,]
    tmpDF$Variable <- "Soil Inorg Residual P Pool 10-30cm"
    subDF4 <- rbind(subDF4, tmpDF)
    
    
    for (i in c("aCO2", "eCO2")) {
        subDF4$mean[subDF4$Variable=="Soil Org Residual P Pool 10-30cm"&subDF4$Trt==i] <- (subDF4$mean[subDF4$Variable=="Soil Org P Pool 10-30cm"&subDF4$Trt==i] -
                                                                                              subDF4$mean[subDF4$Variable=="Microbial P Pool 10-30cm"&subDF4$Trt==i])
        
        subDF4$mean[subDF4$Variable=="Soil Inorg Residual P Pool 10-30cm"&subDF4$Trt==i] <- (subDF4$mean[subDF4$Variable=="Soil Inorg P Pool 10-30cm"&subDF4$Trt==i] -
                                                                                                subDF4$mean[subDF4$Variable=="Soil Phosphate P Pool 10-30cm"&subDF4$Trt==i])
        
        
        subDF4$sd[subDF4$Variable=="Soil Org Residual P Pool 10-30cm"] <- sqrt((subDF4$sd2[subDF4$Variable=="Soil Org P Pool 10-30cm"&subDF4$Trt==i]+
                                                                                   subDF4$sd2[subDF4$Variable=="Microbial P Pool 10-30cm"&subDF4$Trt==i])/2)
        
        subDF4$sd[subDF4$Variable=="Soil Inorg Residual P Pool 10-30cm"] <- sqrt((subDF4$sd2[subDF4$Variable=="Soil Inorg P Pool 10-30cm"&subDF4$Trt==i]+
                                                                                    subDF4$sd2[subDF4$Variable=="Soil Phosphate P Pool 10-30cm"&subDF4$Trt==i])/2)
    }
    
    subDF4$pos[subDF4$Variable=="Soil Org Residual P Pool 10-30cm"] <- subDF4$mean[subDF4$Variable=="Soil Org Residual P Pool 10-30cm"] +
        subDF4$sd[subDF4$Variable=="Soil Org Residual P Pool 10-30cm"]
    
    subDF4$neg[subDF4$Variable=="Soil Org Residual P Pool 10-30cm"] <- subDF4$mean[subDF4$Variable=="Soil Org Residual P Pool 10-30cm"] -
        subDF4$neg[subDF4$Variable=="Soil Org Residual P Pool 10-30cm"]
    
    subDF4$pos[subDF4$Variable=="Soil Inorg Residual P Pool 10-30cm"] <- subDF4$mean[subDF4$Variable=="Soil Inorg Residual P Pool 10-30cm"] +
        subDF4$sd[subDF4$Variable=="Soil Inorg Residual P Pool 10-30cm"]
    
    subDF4$neg[subDF4$Variable=="Soil Inorg Residual P Pool 10-30cm"] <- subDF4$mean[subDF4$Variable=="Soil Inorg Residual P Pool 10-30cm"] -
        subDF4$neg[subDF4$Variable=="Soil Inorg Residual P Pool 10-30cm"]
    
    
    subDF4$sd2[subDF4$Variable=="Soil Org Residual P Pool 10-30cm"] <- NA
    subDF4$sd2[subDF4$Variable=="Soil Inorg Residual P Pool 10-30cm"] <- NA
    
    subDF4 <- subDF4[subDF4$Variable!="Soil Org P Pool 10-30cm",]
    subDF4 <- subDF4[subDF4$Variable!="Soil Inorg P Pool 10-30cm",]
    
    totDF4 <- plotDF[plotDF$Variable=="Soil P Pool 10-30cm",]
    
    
    
    
    ### 30-60 cm soil
    subDF5 <- plotDF[plotDF$Variable%in%c("Soil Inorg P Pool 30-60cm",
                                          "Soil Org P Pool 30-60cm",
                                          "Microbial P Pool 30-60cm",
                                          "Soil Phosphate P Pool 30-60cm"),]
    
    tmpDF <- subDF3[1:2,]
    tmpDF$Variable <- "Soil Org Residual P Pool 30-60cm"
    subDF5 <- rbind(subDF5, tmpDF)
    
    tmpDF <- subDF3[1:2,]
    tmpDF$Variable <- "Soil Inorg Residual P Pool 30-60cm"
    subDF5 <- rbind(subDF5, tmpDF)
    
    for (i in c("aCO2", "eCO2")) {
        subDF5$mean[subDF5$Variable=="Soil Org Residual P Pool 30-60cm"&subDF5$Trt==i] <- (subDF5$mean[subDF5$Variable=="Soil Org P Pool 30-60cm"&subDF5$Trt==i] -
                                                                                              subDF5$mean[subDF5$Variable=="Microbial P Pool 30-60cm"&subDF5$Trt==i])
        
        subDF5$mean[subDF5$Variable=="Soil Inorg Residual P Pool 30-60cm"&subDF5$Trt==i] <- (subDF5$mean[subDF5$Variable=="Soil Inorg P Pool 30-60cm"&subDF5$Trt==i] - subDF5$mean[subDF5$Variable=="Soil Phosphate P Pool 30-60cm"&subDF5$Trt==i])
        
        
        subDF5$sd[subDF5$Variable=="Soil Org Residual P Pool 30-60cm"] <- sqrt((subDF5$sd2[subDF5$Variable=="Soil Org P Pool 30-60cm"&subDF5$Trt==i]+
                                                                                   subDF5$sd2[subDF5$Variable=="Microbial P Pool 30-60cm"&subDF5$Trt==i])/2)
        
        subDF5$sd[subDF5$Variable=="Soil Inorg Residual P Pool 30-60cm"] <- sqrt((subDF5$sd2[subDF5$Variable=="Soil Inorg P Pool 30-60cm"&subDF5$Trt==i]+
                                                                                    subDF5$sd2[subDF5$Variable=="Soil Phosphate P Pool 30-60cm"&subDF5$Trt==i])/2)
    }
    
    subDF5$pos[subDF5$Variable=="Soil Org Residual P Pool 30-60cm"] <- subDF5$mean[subDF5$Variable=="Soil Org Residual P Pool 30-60cm"] +
        subDF5$sd[subDF5$Variable=="Soil Org Residual P Pool 30-60cm"]
    
    subDF5$neg[subDF5$Variable=="Soil Org Residual P Pool 30-60cm"] <- subDF5$mean[subDF5$Variable=="Soil Org Residual P Pool 30-60cm"] -
        subDF5$neg[subDF5$Variable=="Soil Org Residual P Pool 30-60cm"]
    
    
    subDF5$pos[subDF5$Variable=="Soil Inorg Residual P Pool 30-60cm"] <- subDF5$mean[subDF5$Variable=="Soil Inorg Residual P Pool 30-60cm"] +
        subDF5$sd[subDF5$Variable=="Soil Inorg Residual P Pool 30-60cm"]
    
    subDF5$neg[subDF5$Variable=="Soil Inorg Residual P Pool 30-60cm"] <- subDF5$mean[subDF5$Variable=="Soil Inorg Residual P Pool 30-60cm"] -
        subDF5$neg[subDF5$Variable=="Soil Inorg Residual P Pool 30-60cm"]
    
    subDF5$sd2[subDF5$Variable=="Soil Org Residual P Pool 30-60cm"] <- NA
    subDF5$sd2[subDF5$Variable=="Soil Inorg Residual P Pool 30-60cm"] <- NA
    
    subDF5 <- subDF5[subDF5$Variable!="Soil Org P Pool 30-60cm",]
    subDF5 <- subDF5[subDF5$Variable!="Soil Inorg P Pool 30-60cm",]
    
    totDF5 <- plotDF[plotDF$Variable=="Soil P Pool 30-60cm",]
    
    
    #tot1 <- sum(plotDF1$mean[plotDF1$Trt=="aCO2"])
    #tot2 <- sum(plotDF1$mean[plotDF1$Trt=="eCO2"])
    #
    #plotDF1$frac[plotDF1$Trt=="aCO2"] <- round(plotDF1$mean[plotDF1$Trt=="aCO2"]/tot1 * 100,0)
    #plotDF1$frac[plotDF1$Trt=="eCO2"] <- round(plotDF1$mean[plotDF1$Trt=="eCO2"]/tot2 * 100,0)
    #
    #plotDF1 <- plotDF1[order(plotDF1$Trt, plotDF1$frac),]
    #
    #plotDF1$label_y[plotDF1$Trt=="aCO2"] <- cumsum(plotDF1$mean[plotDF1$Trt=="aCO2"])
    #plotDF1$label_y[plotDF1$Trt=="eCO2"] <- cumsum(plotDF1$mean[plotDF1$Trt=="eCO2"])
    
    
    
    ### order labels
    plotDF1$Cat1 <- gsub("Veg", "1_Veg", plotDF1$Cat1)
    
    plotDF2$Variable <- gsub("Canopy P Pool", "1_Canopy P Pool", plotDF2$Variable)
    plotDF2$Variable <- gsub("Understorey P Pool", "2_Understorey P Pool", plotDF2$Variable)
    plotDF2$Variable <- gsub("Fine Root P Pool", "3_Fine Root P Pool", plotDF2$Variable)
    plotDF2$Variable <- gsub("Coarse Root P Pool", "4_Coarse Root P Pool", plotDF2$Variable)
    plotDF2$Variable <- gsub("Heartwood P Pool", "5_Heartwood P Pool", plotDF2$Variable)
    plotDF2$Variable <- gsub("Sapwood P Pool", "6_Sapwood P Pool", plotDF2$Variable)
    plotDF2$Variable <- gsub("Standing Dead Wood P Pool", "7_Standing Dead Wood P Pool", plotDF2$Variable)
    plotDF2$Variable <- gsub("Forestfloor Leaf Litter P Pool", "8_Forestfloor Leaf Litter P Pool", plotDF2$Variable)

    
    
    subDF3$Variable <- gsub("Microbial P Pool 0-10cm", "1_Microbial P Pool 0-10cm", subDF3$Variable)
    subDF3$Variable <- gsub("Soil Org Residual P Pool 0-10cm", "2_Soil Org Residual P Pool 0-10cm", subDF3$Variable)
    subDF3$Variable <- gsub("Soil Inorg Residual P Pool 0-10cm", "4_Soil Inorg P Pool 0-10cm", subDF3$Variable)
    subDF3$Variable <- gsub("Soil Phosphate P Pool 0-10cm", "3_Soil Phosphate P Pool 0-10cm", subDF3$Variable)
    
    
    subDF4$Variable <- gsub("Microbial P Pool 10-30cm", "1_Microbial P Pool 10-30cm", subDF4$Variable)
    subDF4$Variable <- gsub("Soil Org Residual P Pool 10-30cm", "2_Soil Org Residual P Pool 10-30cm", subDF4$Variable)
    subDF4$Variable <- gsub("Soil Inorg Residual P Pool 10-30cm", "4_Soil Inorg P Pool 10-30cm", subDF4$Variable)
    subDF4$Variable <- gsub("Soil Phosphate P Pool 10-30cm", "3_Soil Phosphate P Pool 10-30cm", subDF4$Variable)
    
    
    subDF5$Variable <- gsub("Microbial P Pool 30-60cm", "1_Microbial P Pool 30-60cm", subDF5$Variable)
    subDF5$Variable <- gsub("Soil Org Residual P Pool 30-60cm", "2_Soil Org Residual P Pool 30-60cm", subDF5$Variable)
    subDF5$Variable <- gsub("Soil Inorg Residual P Pool 30-60cm", "4_Soil Inorg P Pool 30-60cm", subDF5$Variable)
    subDF5$Variable <- gsub("Soil Phosphate P Pool 30-60cm", "3_Soil Phosphate P Pool 30-60cm", subDF5$Variable)
    
    
    
    ### Hedley
    #hedDFa <- subset(hedDF, Trt=="amb")
    #hedDFe <- subset(hedDF, Trt=="ele")
    
    
    
    ### plot total ecosystem P pool
    p1 <- ggplot(plotDF1, aes(x=Trt, y=mean))+
        geom_bar(aes(fill=Cat1), col="black",
                 stat = "identity", position="stack")+
        geom_errorbar(data=totDF1, aes(x=Trt, ymax=mean+sd, ymin=mean-sd), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        geom_point(data=totDF1, aes(x=Trt, y=mean), pch=19, col="black", size=2)+
        labs(x="", y=expression(paste("Ecosystem P pool (g P ", m^-2, ")")))+
        theme_linedraw() +
        #geom_text(aes(x=Trt, y=label_y, label = frac), stat = "identity", 
        #          position = position_dodge(0.9), vjust=2.0,
        #          colour = "black")+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position = c(0.8, 0.2),
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        #scale_fill_manual(name="", 
        #                  values = c("Veg" = cbPalette[2],
        #                             "Lit" = cbPalette[7],
        #                             "Soil_0_10" = cbPalette[4],
        #                             "Soil_10_30" = cbPalette[5],
        #                             "Soil_30_60" = cbPalette[6]),
        #                  labels=c("Veg"="Vegetation",
        #                           "Lit"="Litter",
        #                           "Soil_0_10"=expression(Soil['0-10cm']),
        #                           "Soil_10_30"=expression(Soil['10-30cm']),
        #                           "Soil_30_60"=expression(Soil['30-60cm'])))+
        scale_fill_manual(name="P Pool", 
                          values = c("1_Veg" = cbPalette[4],
                                     "Soil_Inorg" = cbPalette[2],
                                     "Soil_Org" = cbPalette[5]),
                          labels=c("1_Veg"="Plant+Litter",
                                   "Soil_Inorg"="Inorganic soil",
                                   "Soil_Org"="Organic soil"))+
        scale_x_discrete(limits=c("aCO2", "eCO2"),
                         labels=c("aCO2"=expression(aCO[2]),
                                  "eCO2"=expression(eCO[2])))
        
    
    ### plot vegetation P pool
    p2 <- ggplot(plotDF2, aes(x=Trt, y=mean))+
        geom_bar(aes(fill=Variable), col="black",
                 stat = "identity", position="stack")+
        geom_errorbar(data=totDF2, aes(x=Trt, ymax=mean+sd, ymin=mean-sd), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        geom_point(data=totDF2, aes(x=Trt, y=mean), pch=19, col="black", size=2)+
        labs(x="", y=expression(paste("Plant + Litter P pool (g P ", m^-2, ")")))+
        #labs(x="", y="")+
        theme_linedraw() +
        #scale_y_continuous(position = "right")+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right")+
        scale_fill_manual(name="", 
                          values = c("1_Canopy P Pool" = SpectralPalette[1],
                                     "4_Coarse Root P Pool" = SpectralPalette[4],
                                     "3_Fine Root P Pool" = SpectralPalette[3],
                                     "8_Forestfloor Leaf Litter P Pool" = SpectralPalette[9],
                                     "5_Heartwood P Pool" = SpectralPalette[5],
                                     "6_Sapwood P Pool" = SpectralPalette[6],
                                     "7_Standing Dead Wood P Pool" = SpectralPalette[8],
                                     
                                     "2_Understorey P Pool" = SpectralPalette[2]),
                          labels=c("1_Canopy P Pool"="Canopy",
                                   "4_Coarse Root P Pool"="Coarseroot",
                                   "3_Fine Root P Pool"="Fineroot",
                                   "8_Forestfloor Leaf Litter P Pool"="Forestfloor leaf litter",
                                   "5_Heartwood P Pool"="Heartwood",
                                   "6_Sapwood P Pool"="Sapwood",
                                   "7_Standing Dead Wood P Pool"="Standing dead wood",
                                   
                                   "2_Understorey P Pool"="Understorey aboveground"))+
        scale_x_discrete(limits=c("aCO2", "eCO2"),
                         labels=c("aCO2"=expression(aCO[2]),
                                  "eCO2"=expression(eCO[2])))
    
    
    
    ### plot soil P pool 0 - 10 cm
    p3 <- ggplot(subDF3, aes(x=Trt, y=mean))+
        geom_bar(aes(fill=Variable), col="black",
                 stat = "identity", position="stack")+
        geom_errorbar(data=totDF3, aes(x=Trt, ymax=mean+sd, ymin=mean-sd), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        geom_point(data=totDF3, aes(x=Trt, y=mean), pch=19, col="black", size=2)+
        labs(x="0 - 10 cm", y=expression(paste("Soil P pool (g P ", m^-2, ")")))+
        theme_linedraw() +
        coord_flip()+
        ylim(0, 18)+
        #ggtitle("0-10 cm")+
        annotate("text", y=16, x=0.7, label="0-10 cm", size=5)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              plot.title = element_text(hjust = 1),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", 
                          values = c("4_Soil Inorg P Pool 0-10cm" = Diverge_hsv_Palette[5],
                                     "3_Soil Phosphate P Pool 0-10cm" = Diverge_hsv_Palette[7],
                                     "1_Microbial P Pool 0-10cm" = Diverge_hsv_Palette[1],
                                     "2_Soil Org Residual P Pool 0-10cm" = Diverge_hsv_Palette[3]),
                          labels=c("4_Soil Inorg P Pool 0-10cm"="Inorganic residual",
                                   "3_Soil Phosphate P Pool 0-10cm"="Phosphate",
                                   "1_Microbial P Pool 0-10cm"="Microbe",
                                   "2_Soil Org Residual P Pool 0-10cm"="Organic residual"))+
        scale_x_discrete(limits=c("aCO2", "eCO2"),
                         labels=c("aCO2"=expression(aCO[2]),
                                  "eCO2"=expression(eCO[2])))


    
    
    ### plot soil P pool 10 - 30 cm
    p4 <- ggplot(subDF4, aes(x=Trt, y=mean))+
        geom_bar(aes(fill=Variable), col="black",
                 stat = "identity", position="stack")+
        geom_errorbar(data=totDF4, aes(x=Trt, ymax=mean+sd, ymin=mean-sd), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        geom_point(data=totDF4, aes(x=Trt, y=mean), pch=19, col="black", size=2)+
        labs(x="10 - 30 cm", y=expression(paste("Soil P pool (g P ", m^-2, ")")))+
        theme_linedraw() +
        ylim(0, 18)+
        coord_flip()+
        #ggtitle("10-30 cm")+
        annotate("text", y=16, x=0.7, label="10-30 cm", size=5)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              plot.title = element_text(hjust = 1),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", 
                          values = c("4_Soil Inorg P Pool 10-30cm" = Diverge_hsv_Palette[5],
                                     "3_Soil Phosphate P Pool 10-30cm" = Diverge_hsv_Palette[7],
                                     "1_Microbial P Pool 10-30cm" = Diverge_hsv_Palette[1],
                                     "2_Soil Org Residual P Pool 10-30cm" = Diverge_hsv_Palette[3]),
                          labels=c("4_Soil Inorg P Pool 10-30cm"="Inorganic residual",
                                   "3_Soil Phosphate P Pool 10-30cm"="Phosphate",
                                   "1_Microbial P Pool 10-30cm"="Microbe",
                                   "2_Soil Org Residual P Pool 10-30cm"="Organic residual"))+
        scale_x_discrete(limits=c("aCO2", "eCO2"),
                         labels=c("aCO2"=expression(aCO[2]),
                                  "eCO2"=expression(eCO[2])))
    
    
    
    ### plot soil P pool 30 - 60 cm
    p5 <- ggplot(subDF5, aes(x=Trt, y=mean))+
        geom_bar(aes(fill=Variable), col="black",
                 stat = "identity", position="stack")+
        geom_errorbar(data=totDF5, aes(x=Trt, ymax=mean+sd, ymin=mean-sd), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        geom_point(data=totDF5, aes(x=Trt, y=mean), pch=19, col="black", size=2)+
        labs(x="30 - 60 cm", y=expression(paste("Soil P pool (g P ", m^-2, ")")))+
        theme_linedraw() +
        ylim(0, 18)+
        coord_flip()+
        annotate("text", y=16, x=0.7, label="30-60 cm", size=5)+
        #ggtitle("30-60 cm")+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              plot.title = element_text(hjust = 1),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", 
                          values = c("4_Soil Inorg P Pool 30-60cm" = Diverge_hsv_Palette[5],
                                     "3_Soil Phosphate P Pool 30-60cm" = Diverge_hsv_Palette[7],
                                     "1_Microbial P Pool 30-60cm" = Diverge_hsv_Palette[1],
                                     "2_Soil Org Residual P Pool 30-60cm" = Diverge_hsv_Palette[3]),
                          labels=c("4_Soil Inorg P Pool 30-60cm"="Inorganic residual",
                                   "3_Soil Phosphate P Pool 30-60cm"="Phosphate",
                                   "1_Microbial P Pool 30-60cm"="Microbe",
                                   "2_Soil Org Residual P Pool 30-60cm"="Organic residual"))+
        scale_x_discrete(limits=c("aCO2", "eCO2"),
                         labels=c("aCO2"=expression(aCO[2]),
                                  "eCO2"=expression(eCO[2])))
    
    
    p6 <- ggplot(hedDF,
                 aes(Trt, frac)) + 
        geom_bar(stat = "identity", aes(fill=Pool), col="black",position="stack")+
        xlab("")+ 
        ylab("Soil P bio-availability (fraction)")+
        theme_linedraw() +
        ylim(0, 1)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right")+
        scale_fill_manual(name="Hedley P fractionation", 
                          values = c("1_Exchangeable_Pi" = set3Palette[6], 
                                     "2_Exchangeable_Po" = set3Palette[8],
                                     "3_Moderately_labile_Po" = set3Palette[3],
                                     "4_Secondary_Pi" = set3Palette[4],
                                     "5_Primary_Pi" = set3Palette[5], 
                                     "6_Occluded_P" = set3Palette[7]),
                          labels=c(expression("Exchangeable " * P[i]), 
                                   expression("Exchangeable " * P[o]), 
                                   expression("Moderately labile " * P[o]), 
                                   expression("Secondary " * P[i] * " (" * F[e] * "-bound)"),
                                   expression("Primary " * P[i] * " (" * C[a] *"-bound)"),
                                   "Occluded P"))+
        scale_x_discrete(limits=c("amb","ele"),
                         labels=c(expression(aCO[2]),
                                  expression(eCO[2])))
    
    #plot(p6)
    
    
    legend_bot_row <- get_legend(p3 + theme(legend.position="bottom",
                                            legend.box = 'horizotal',
                                            legend.box.just = 'left')
                                 +guides(fill=guide_legend(nrow=2,byrow=TRUE)))
    
    
    
    grid.labs <- c("(a)", "(b)", "(c)", "(d)")#, "(e)")
    
    
    ## plot 
    pdf(paste0("plots_tables/output/", norm, "/P_Pools_Summary_Plots_", norm, "_2.pdf"),
        width=10,height=8)
    top_row <- plot_grid(p1, NULL, p2, ncol=3, rel_widths=c(1, 0.1, 1))
    bot_row_left <- plot_grid(p3, p4, p5, legend_bot_row, ncol=1, rel_heights=c(0.7, 0.7, 1, 0.4))
    bot_row <- plot_grid(bot_row_left, NULL, p6, ncol=3, rel_widths=c(1, 0.1, 1))
    plot_grid(top_row, bot_row, #scale = 0.9,
              ncol = 1, rel_widths = c(1, 1),
              rel_heights=c(1, 1))
    #grid.text(grid.labs,x = c(0.08, 0.48, 0.1, 0.42, 0.75), y = c(0.97, 0.97, 0.5, 0.5, 0.5),
    #          gp=gpar(fontsize=16, col="black", fontface="bold"))
    grid.text(grid.labs,x = c(0.08, 0.54, 0.08, 0.54), 
              y = c(0.97, 0.97, 0.52, 0.52),
              gp=gpar(fontsize=16, col="black", fontface="bold"))
    dev.off()
    
    
    
    
     ### Plot treemap
    
    require(treemapify)
    
    
    ### process the dataframe
    ## remove all " P Pool" to save space
    plotDF$Variable <- gsub(" P Pool", "", plotDF$Variable)
    
    ## remove totals
    plotDF <- plotDF[plotDF$Variable!="Total Wood",]
    plotDF <- plotDF[plotDF$Variable!="Soil 0-10cm",]
    plotDF <- plotDF[plotDF$Variable!="Soil 10-30cm",]
    plotDF <- plotDF[plotDF$Variable!="Soil 30-60cm",]
    
    
    tmpDF2 <- tmpDF1 <- plotDF[1:2,]
    tmpDF1$Variable <- "Soil Org Residual 0-10cm"
    tmpDF2$Variable <- "Soil Inorg Residual 0-10cm"
    
    for (i in c("aCO2", "eCO2")) {
        tmpDF1$mean[tmpDF1$Trt==i] <- plotDF$mean[plotDF$Variable=="Soil Org 0-10cm"&plotDF$Trt==i] -
                                          plotDF$mean[plotDF$Variable=="Microbial 0-10cm"&plotDF$Trt==i]
        
        tmpDF2$mean[tmpDF2$Trt==i] <- plotDF$mean[plotDF$Variable=="Soil Inorg 0-10cm"&plotDF$Trt==i] -
            plotDF$mean[plotDF$Variable=="Soil Phosphate 0-10cm"&plotDF$Trt==i]
    }
    
    tmpDF <- rbind(tmpDF1, tmpDF2)
    plotDF <- rbind(plotDF, tmpDF)
    
    
    tmpDF2 <- tmpDF1 <- plotDF[1:2,]
    tmpDF1$Variable <- "Soil Org Residual 10-30cm"
    tmpDF2$Variable <- "Soil Inorg Residual 10-30cm"
    
    for (i in c("aCO2", "eCO2")) {
        tmpDF1$mean[tmpDF1$Trt==i] <- plotDF$mean[plotDF$Variable=="Soil Org 10-30cm"&plotDF$Trt==i] -
            plotDF$mean[plotDF$Variable=="Microbial 10-30cm"&plotDF$Trt==i]
        
        tmpDF2$mean[tmpDF2$Trt==i] <- plotDF$mean[plotDF$Variable=="Soil Inorg 10-30cm"&plotDF$Trt==i] -
            plotDF$mean[plotDF$Variable=="Soil Phosphate 10-30cm"&plotDF$Trt==i]
    }
    
    tmpDF <- rbind(tmpDF1, tmpDF2)
    plotDF <- rbind(plotDF, tmpDF)
    
    
    
    tmpDF2 <- tmpDF1 <- plotDF[1:2,]
    tmpDF1$Variable <- "Soil Org Residual 30-60cm"
    tmpDF2$Variable <- "Soil Inorg Residual 30-60cm"
    
    for (i in c("aCO2", "eCO2")) {
        tmpDF1$mean[tmpDF1$Trt==i] <- plotDF$mean[plotDF$Variable=="Soil Org 30-60cm"&plotDF$Trt==i] -
            plotDF$mean[plotDF$Variable=="Microbial 30-60cm"&plotDF$Trt==i]
        
        tmpDF2$mean[tmpDF2$Trt==i] <- plotDF$mean[plotDF$Variable=="Soil Inorg 30-60cm"&plotDF$Trt==i] -
            plotDF$mean[plotDF$Variable=="Soil Phosphate 30-60cm"&plotDF$Trt==i]
    }
    
    tmpDF <- rbind(tmpDF1, tmpDF2)
    plotDF <- rbind(plotDF, tmpDF)
    
    
    plotDF <- plotDF[plotDF$Variable!="Soil Org 0-10cm",]
    plotDF <- plotDF[plotDF$Variable!="Soil Org 10-30cm",]
    plotDF <- plotDF[plotDF$Variable!="Soil Org 30-60cm",]
    
    plotDF <- plotDF[plotDF$Variable!="Soil Inorg 0-10cm",]
    plotDF <- plotDF[plotDF$Variable!="Soil Inorg 10-30cm",]
    plotDF <- plotDF[plotDF$Variable!="Soil Inorg 30-60cm",]
    plotDF <- plotDF[plotDF$Variable!="Understorey Litter",]
    
    ### remove excess columns
    
    
    ### coloring
    plotDF$Cat3 <- NA
    plotDF$Cat3[plotDF$Variable%in%c("Canopy", "Fine Root", "Understorey")] <- "Labile Veg"
    plotDF$Cat3[plotDF$Variable%in%c("Sapwood", "Heartwood", "Coarse Root")] <- "Slow Veg"
    plotDF$Cat3[plotDF$Variable%in%c("Forestfloor Leaf Litter", "Standing Dead Wood")] <- "Litter"
    plotDF$Cat3[plotDF$Variable%in%c("Soil Phosphate 0-10cm", "Soil Phosphate 10-30cm", "Soil Phosphate 30-60cm")] <- "Phosphate"
    plotDF$Cat3[plotDF$Variable%in%c("Soil Org Residual 0-10cm", "Soil Org Residual 10-30cm", "Soil Org Residual 30-60cm")] <- "Soil Org"
    plotDF$Cat3[plotDF$Variable%in%c("Soil Inorg Residual 0-10cm", "Soil Inorg Residual 10-30cm", "Soil Inorg Residual 30-60cm")] <- "Soil Inorg"
    plotDF$Cat3[plotDF$Variable%in%c("Microbial 0-10cm", "Microbial 10-30cm", "Microbial 30-60cm")] <- "Microbe"
    
    
    ### split into two dataframes to plot
    aDF <- plotDF[plotDF$Trt=="aCO2",]
    eDF <- plotDF[plotDF$Trt=="eCO2",]
    
    library(viridis) 
    palette1 <- viridis(8)
    
    p6 <- ggplot(aDF, aes(area = mean, fill = Cat3, label=Variable)) +
        geom_treemap()+
        geom_treemap_text(colour = "white",
                          place = "centre",
                          size = 15)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", 
                          values = c("Labile Veg" = palette1[5],
                                     "Slow Veg" = palette1[3],
                                     "Litter" = palette1[4],
                                     "Phosphate" = palette1[6],
                                     "Soil Org" = palette1[1],
                                     "Soil Inorg" = palette1[2],
                                     "Microbe" = palette1[7]),
                          labels=c("Labile Veg" = "Labile Veg" ,
                                   "Slow veg" = "Slow veg",
                                   "Litter" = "Litter",
                                   "Phosphate" = "Phosphate",
                                   "Soil Org" = "Soil Org",
                                   "Soil Inorg" = "Soil Inorg",
                                   "Microbe" = "Microbe"))
    
    
    p7 <- ggplot(eDF, aes(area = mean, fill = Cat3, label=Variable)) +
        geom_treemap()+
        geom_treemap_text(colour = "white",
                          place = "centre",
                          size = 15)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", 
                          values = c("Labile Veg" = palette1[5],
                                     "Slow Veg" = palette1[3],
                                     "Litter" = palette1[4],
                                     "Phosphate" = palette1[6],
                                     "Soil Org" = palette1[1],
                                     "Soil Inorg" = palette1[2],
                                     "Microbe" = palette1[7]),
                          labels=c("Labile Veg" = "Labile Veg" ,
                                   "Slow veg" = "Slow veg",
                                   "Litter" = "Litter",
                                   "Phosphate" = "Phosphate",
                                   "Soil Org" = "Soil Org",
                                   "Soil Inorg" = "Soil Inorg",
                                   "Microbe" = "Microbe"))
    
    
    
    legend_bot_row <- get_legend(p1 + theme(legend.position="bottom",
                                            legend.box = 'horizontal',
                                            legend.box.just = 'left'))
    
    
    
    grid.labs <- c("(a)", "(b)")
    
    
    ## plot 
    pdf(paste0("plots_tables/output/", norm, "/P_Pools_treemaps_", norm, ".pdf"),
        width=16,height=6)
    top_row <- plot_grid(p1, p2, ncol=2)
    plot_grid(top_row, legend_bot_row,
              ncol = 1, rel_widths = c(1, 1),
              rel_heights=c(1, 0.2))
    #grid.text(grid.labs, x = c(0.45, 0.95), y = c(0.97, 0.97),
    #          gp=gpar(fontsize=16, col="black", fontface="bold"))
    dev.off()
    
}


