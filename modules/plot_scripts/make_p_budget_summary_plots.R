make_p_budget_summary_plots <- function(inDF, 
                                        inDF2,
                                        inDF3,
                                        norm) {
    
    ################### Plot all P summary budget plots
    ### Plot 1df
    plotDF1 <- data.frame(c(inDF$aCO2[inDF$terms=="Total plant P stock"], 
                            inDF$eCO2[inDF$terms=="Total plant P stock"]), 
                          NA)
    colnames(plotDF1) <- c("mean", "sd")
    plotDF1$sd <- c(inDF$aCO2_sd[inDF$terms=="Total plant P stock"], 
                    inDF$eCO2_sd[inDF$terms=="Total plant P stock"])
    plotDF1$Trt <- c("aCO2", "eCO2")
    plotDF1$pos <- with(plotDF1, mean + sd)
    plotDF1$neg <- with(plotDF1, mean - sd)
    
    ### Plot 2
    plotDF2 <- data.frame(c(inDF$aCO2[inDF$terms=="Overstorey aboveground P stock"], 
                            inDF$eCO2[inDF$terms=="Overstorey aboveground P stock"],
                            inDF$aCO2[inDF$terms=="Understorey aboveground P stock"], 
                            inDF$eCO2[inDF$terms=="Understorey aboveground P stock"],
                            inDF$aCO2[inDF$terms=="Belowground P stock"], 
                            inDF$eCO2[inDF$terms=="Belowground P stock"],
                            inDF$aCO2[inDF$terms=="Dead P stock"], 
                            inDF$eCO2[inDF$terms=="Dead P stock"]), 
                          NA)
    colnames(plotDF2) <- c("mean", "sd")
    plotDF2$sd <- c(inDF$aCO2_sd[inDF$terms=="Overstorey aboveground P stock"], 
                    inDF$eCO2_sd[inDF$terms=="Overstorey aboveground P stock"],
                    inDF$aCO2_sd[inDF$terms=="Understorey aboveground P stock"], 
                    inDF$eCO2_sd[inDF$terms=="Understorey aboveground P stock"],
                    inDF$aCO2_sd[inDF$terms=="Belowground P stock"], 
                    inDF$eCO2_sd[inDF$terms=="Belowground P stock"],
                    inDF$aCO2_sd[inDF$terms=="Dead P stock"], 
                    inDF$eCO2_sd[inDF$terms=="Dead P stock"])
    plotDF2$Trt <- rep(c("aCO2", "eCO2"), 4)
    plotDF2$Variable <- rep(c("OA", "UA", "B", "D"), each=2)
    plotDF2$pos <- with(plotDF2, mean + sd)
    plotDF2$neg <- with(plotDF2, mean - sd)
    aC <- sum(plotDF2$mean[plotDF2$Trt=="aCO2"])
    eC <- sum(plotDF2$mean[plotDF2$Trt=="eCO2"])
    
    plotDF2$prop[plotDF2$Trt=="aCO2"] <- plotDF2$mean[plotDF2$Trt=="aCO2"] / aC * 100
    plotDF2$prop[plotDF2$Trt=="eCO2"] <- plotDF2$mean[plotDF2$Trt=="eCO2"] / eC * 100
    
    
    ### Plot 3
    plotDF3 <- data.frame(c(inDF$aCO2[inDF$terms=="Total plant P requirement flux"], 
                            inDF$eCO2[inDF$terms=="Total plant P requirement flux"]), 
                          NA)
    colnames(plotDF3) <- c("mean", "sd")
    plotDF3$sd <- c(inDF$aCO2_sd[inDF$terms=="Total plant P requirement flux"], 
                    inDF$eCO2_sd[inDF$terms=="Total plant P requirement flux"])
    plotDF3$Trt <- c("aCO2", "eCO2")
    plotDF3$pos <- with(plotDF3, mean + sd)
    plotDF3$neg <- with(plotDF3, mean - sd)
    
    ### Plot 4
    plotDF4 <- data.frame(c(inDF$aCO2[inDF$terms=="Total plant P retranslocation flux"], 
                            inDF$eCO2[inDF$terms=="Total plant P retranslocation flux"]), 
                          NA)
    colnames(plotDF4) <- c("mean", "sd")
    plotDF4$sd <- c(inDF$aCO2_sd[inDF$terms=="Total plant P retranslocation flux"], 
                    inDF$eCO2_sd[inDF$terms=="Total plant P retranslocation flux"])
    plotDF4$Trt <- c("aCO2", "eCO2")
    plotDF4$pos <- with(plotDF4, mean + sd)
    plotDF4$neg <- with(plotDF4, mean - sd)
    
    ### Plot 5
    plotDF5 <- data.frame(c(inDF$aCO2[inDF$terms=="Plant P uptake flux"], 
                            inDF$eCO2[inDF$terms=="Plant P uptake flux"]), 
                          NA)
    colnames(plotDF5) <- c("mean", "sd")
    plotDF5$sd <- c(inDF$aCO2_sd[inDF$terms=="Plant P uptake flux"], 
                    inDF$eCO2_sd[inDF$terms=="Plant P uptake flux"])
    plotDF5$Trt <- c("aCO2", "eCO2")
    plotDF5$pos <- with(plotDF5, mean + sd)
    plotDF5$neg <- with(plotDF5, mean - sd)
    
    ### Plot 6
    plotDF6 <- data.frame(c(inDF$aCO2[inDF$terms=="Soil P mineralization flux"], 
                            inDF$eCO2[inDF$terms=="Soil P mineralization flux"]), 
                          NA)
    colnames(plotDF6) <- c("mean", "sd")
    plotDF6$sd <- c(inDF$aCO2_sd[inDF$terms=="Soil P mineralization flux"], 
                    inDF$eCO2_sd[inDF$terms=="Soil P mineralization flux"])
    plotDF6$Trt <- c("aCO2", "eCO2")
    plotDF6$pos <- with(plotDF6, mean + sd)
    plotDF6$neg <- with(plotDF6, mean - sd)
    
    ### plot 7
    plotDF7 <- data.frame(c(inDF$aCO2[inDF$terms=="Plant P MRT"], 
                            inDF$eCO2[inDF$terms=="Plant P MRT"]), 
                          NA)
    colnames(plotDF7) <- c("mean", "sd")
    plotDF7$sd <- c(inDF$aCO2_sd[inDF$terms=="Plant P MRT"], 
                    inDF$eCO2_sd[inDF$terms=="Plant P MRT"])
    plotDF7$Trt <- c("aCO2", "eCO2")
    plotDF7$pos <- with(plotDF7, mean + sd)
    plotDF7$neg <- with(plotDF7, mean - sd)
    
    
    ### plot 8 PUE
    plotDF8 <- data.frame(c(inDF$aCO2[inDF$terms=="Plant PUE"], 
                            inDF$eCO2[inDF$terms=="Plant PUE"]), 
                          NA)
    colnames(plotDF8) <- c("mean", "sd")
    plotDF8$sd <- c(inDF$aCO2_sd[inDF$terms=="Plant PUE"], 
                    inDF$eCO2_sd[inDF$terms=="Plant PUE"])
    plotDF8$Trt <- c("aCO2", "eCO2")
    plotDF8$pos <- with(plotDF8, mean + sd)
    plotDF8$neg <- with(plotDF8, mean - sd)
    
    
    
    ### P uptake / P mineralization
    plotDF9 <- data.frame(c(inDF$aCO2[inDF$terms=="Plant P uptake over P mineralization"], 
                            inDF$eCO2[inDF$terms=="Plant P uptake over P mineralization"]), 
                          NA)
    colnames(plotDF9) <- c("mean", "sd")
    plotDF9$sd <- c(inDF$aCO2_sd[inDF$terms=="Plant P uptake over P mineralization"], 
                    inDF$eCO2_sd[inDF$terms=="Plant P uptake over P mineralization"])
    plotDF9$Trt <- c("aCO2", "eCO2")
    plotDF9$pos <- with(plotDF9, mean + sd)
    plotDF9$neg <- with(plotDF9, mean - sd)
    
    
    plotDF10 <- data.frame(c(inDF$aCO2[inDF$terms=="Plant P MRT"], 
                             inDF$eCO2[inDF$terms=="Plant P MRT"],
                             inDF$aCO2[inDF$terms=="Microbe P MRT"], 
                             inDF$eCO2[inDF$terms=="Microbe P MRT"]), 
                           NA, NA)
    colnames(plotDF10) <- c("mean", "sd", "Component")
    plotDF10$Component <- c("Plant", "Plant", "Microbe", "Microbe")
    plotDF10$sd <- c(inDF$aCO2_sd[inDF$terms=="Plant P MRT"], 
                     inDF$eCO2_sd[inDF$terms=="Plant P MRT"],
                     inDF$aCO2_sd[inDF$terms=="Microbe P MRT"], 
                    inDF$eCO2_sd[inDF$terms=="Microbe P MRT"])
    plotDF10$Trt <- c("aCO2", "eCO2")
    plotDF10$pos <- with(plotDF10, mean + sd)
    plotDF10$neg <- with(plotDF10, mean - sd)
    

    
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
        #geom_text(aes(label=round(prop, 0), y=prop-10))+
        theme_minimal(10)+
        theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(),
            panel.grid=element_blank(),
            axis.ticks = element_blank(),
            plot.title=element_text(size=14, face="bold"),
            legend.position="bottom")+
        scale_fill_manual(name="Component", values = c("UA" = "green", 
                                                       "OA" = "darkgreen", 
                                                       "B" = "orange",
                                                       "D" = "brown"),
                          labels=c("B", "D", "OA", "UA"))
    
    p3 <- ggplot(plotDF3,
                 aes(Trt, mean)) + 
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        xlab("") + ylab(expression(paste("P requirement (g P ", m^-2, " ", yr^-1, ")")))+
        theme_linedraw() +
        ylim(0, 1.0)+
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
        ylim(0, 1.0)+
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
        ylim(0, 1.0)+
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
        ylim(0, 1.0)+
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
    
    
    p7 <- ggplot(plotDF7,
                 aes(Trt, mean)) + 
      geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
      geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                    position = position_dodge(0.9), width=0.2, size=0.4) +
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
    

    p8 <- ggplot(plotDF8,
                 aes(Trt, mean)) + 
      geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
      geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                    position = position_dodge(0.9), width=0.2, size=0.4) +
      xlab("") + ylab(expression(paste("Plant PUE ( gC" * " " *gP^-1 * " )")))+
      theme_linedraw() +
      ylim(0,3000)+
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
    
    
    
    grid.labs <- c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)", "(g)", "(h)")
    
    require(grid)
    require(cowplot)
    
    ## plot 
    pdf(paste0("plots_tables/output/", norm, "/P_Budget_Summary_Plots_", norm, ".pdf"), 
        width=10,height=14)
    plot_grid(p3, p4, p5, p6, 
              p1, p2, p7, p8, 
              labels="", ncol=2, align="v", axis = "l",
              rel_heights = c(1, 1, 1.2, 1))
    grid.text(grid.labs, x = c(0.1, 0.6, 0.1, 0.6, 0.1, 0.6, 0.1, 0.6),
              y = c(0.97, 0.97, 0.73, 0.73, 0.5, 0.5, 0.21, 0.21), 
              gp=gpar(fontsize=14, col="black", fontface="bold"))
    dev.off()
    
    
    
    
    ### calculate proportions of demand flux
    demDF <- inDF2[inDF2$terms%in%c("Canopy P flux", "Wood P flux", "Fine Root P flux", 
                                    "Coarse Root P flux", "Twig litter P flux", "Bark litter P flux",
                                    "Seed litter P flux", "Frass P flux", "Understorey P flux"),]
    
    demDF$aCO2_p <- round(demDF$aCO2 / inDF2$aCO2[inDF2$terms=="Total vegetation production P flux"],3) * 100
    demDF$eCO2_p <- round(demDF$eCO2 / inDF2$eCO2[inDF2$terms=="Total vegetation production P flux"],3) * 100
    
    demDF <- demDF[,c("terms", "aCO2_p", "eCO2_p")]
    
    
    
    demDF2 <- demDF
    demDF2$terms2 <- demDF2$terms
    demDF2$terms2[demDF2$terms%in%c("Wood P flux", 
                                    "Coarse Root P flux", "Twig litter P flux",
                                    "Bark litter P flux", "Seed litter P flux",
                                    "Frass P flux")] <- "Other"
    demDF2 <- summaryBy(aCO2_p+eCO2_p~terms2, FUN=sum, data=demDF2, na.rm=T, keep.names=T)
    
    
    ### calculate proportions of retranslocation flux
    retDF <- inDF2[inDF2$terms%in%c("Canopy retrans P flux", "Sapwood retrans P flux", "Fineroot retrans P flux", 
                                    "Coarseroot retrans P flux", "Understorey retrans P flux"),]
    
    retDF$aCO2_p <- round(retDF$aCO2 / inDF2$aCO2[inDF2$terms=="Total vegetation retranslocation P flux"],3) * 100
    retDF$eCO2_p <- round(retDF$eCO2 / inDF2$eCO2[inDF2$terms=="Total vegetation retranslocation P flux"],3) * 100
    
    retDF <- retDF[,c("terms", "aCO2_p", "eCO2_p")]
    
    
    ### calculate proportions of litter flux (i.e. uptake) - litter to scale
    litDF <- inDF2[inDF2$terms%in%c("Leaflitter P flux", "Fineroot Litter P flux", 
                                    "Twig litter P flux", "Bark litter P flux",
                                    "Seed litter P flux", "Frass P flux", "Understorey Litter P flux"),]
    
    litDF$aCO2_p <- round(litDF$aCO2 / sum(litDF$aCO2),3) * 100
    litDF$eCO2_p <- round(litDF$eCO2 / sum(litDF$eCO2),3) * 100
    
    litDF <- litDF[,c("terms", "aCO2_p", "eCO2_p")]
    
    
    ### calculate proportions of net P min flux (i.e. uptake) 
    minDF <- inDF2[inDF2$terms%in%c("Mineralization P flux 0-10cm", "Mineralization P flux 10-30cm", 
                                    "Mineralization P flux 30-60cm"),]
    
    minDF$aCO2_p <- round(minDF$aCO2 / sum(minDF$aCO2),3) * 100
    minDF$eCO2_p <- round(minDF$eCO2 / sum(minDF$eCO2),3) * 100
    
    minDF <- minDF[,c("terms", "aCO2", "eCO2", "aCO2_p", "eCO2_p")]
    
    minDF2 <- reshape2::melt(minDF[,1:3], id.var=c("terms"))
    minDF2$terms <- gsub("Mineralization P flux ", "", minDF2$terms)
    colnames(minDF2) <- c("Depth", "Trt", "mean")
    
    
    ### add information
    plotDF4$Variable <- "retrans"
    plotDF5$Variable <- "uptake"
    
    plotDF9 <- rbind(plotDF4, plotDF5)
    
    
    ### prepare demDF for plotting
    demDF.plot <- reshape2::melt(demDF, id.vars=c("terms"))
    
    
    
    demDF$ymax1 <- cumsum(demDF$aCO2_p)
    demDF$ymax2 <- cumsum(demDF$eCO2_p)
    demDF$ymin1 <- c(0, head(demDF$ymax1, n=-1))
    demDF$ymin2 <- c(0, head(demDF$ymax2, n=-1))
    
    demDF$labelPosition1 <- (demDF$ymax1 + demDF$ymin1) / 2
    demDF$labelPosition2 <- (demDF$ymax2 + demDF$ymin2) / 2
    
    demDF$label1 <- gsub(" P flux", "", demDF$terms)
    demDF$label2 <- gsub(" P flux", "", demDF$terms)
    demDF$label1 <- gsub(" litter", "", demDF$label1)
    demDF$label2 <- gsub(" litter", "", demDF$label2)
    
    
    demDF2$ymax1 <- cumsum(demDF2$aCO2_p)
    demDF2$ymax2 <- cumsum(demDF2$eCO2_p)
    demDF2$ymin1 <- c(0, head(demDF2$ymax1, n=-1))
    demDF2$ymin2 <- c(0, head(demDF2$ymax2, n=-1))
    
    demDF2$labelPosition1 <- (demDF2$ymax1 + demDF2$ymin1) / 2
    demDF2$labelPosition2 <- (demDF2$ymax2 + demDF2$ymin2) / 2
    
    demDF2$label1 <- gsub(" P flux", "", demDF2$terms2)
    demDF2$label2 <- gsub(" P flux", "", demDF2$terms2)
    demDF2$label1 <- gsub(" litter", "", demDF2$label1)
    demDF2$label2 <- gsub(" litter", "", demDF2$label2)
    
    
    
    retDF$ymax1 <- cumsum(retDF$aCO2_p)
    retDF$ymax2 <- cumsum(retDF$eCO2_p)
    retDF$ymin1 <- c(0, head(retDF$ymax1, n=-1))
    retDF$ymin2 <- c(0, head(retDF$ymax2, n=-1))
    
    retDF$labelPosition1 <- (retDF$ymax1 + retDF$ymin1) / 2
    retDF$labelPosition2 <- (demDF$ymax2 + retDF$ymin2) / 2
    
    retDF$label1 <- gsub(" retrans P flux", "", retDF$terms)
    retDF$label2 <- gsub(" retrans P flux", "", retDF$terms)
    
    
    
    litDF$ymax1 <- cumsum(litDF$aCO2_p)
    litDF$ymax2 <- cumsum(litDF$eCO2_p)
    litDF$ymin1 <- c(0, head(litDF$ymax1, n=-1))
    litDF$ymin2 <- c(0, head(litDF$ymax2, n=-1))
    
    litDF$labelPosition1 <- (litDF$ymax1 + litDF$ymin1) / 2
    litDF$labelPosition2 <- (litDF$ymax2 + litDF$ymin2) / 2
    
    litDF$label1 <- gsub(" P flux", "", litDF$terms)
    litDF$label2 <- gsub(" P flux", "", litDF$terms)
    
    litDF$label1 <- gsub(" litter", "", litDF$label1)
    litDF$label2 <- gsub(" litter", "", litDF$label2)
    
    litDF$label1 <- gsub("litter", "", litDF$label1)
    litDF$label2 <- gsub("litter", "", litDF$label2)
    
    litDF$label1 <- gsub(" Litter", "", litDF$label1)
    litDF$label2 <- gsub(" Litter", "", litDF$label2)
    
    
    
    ### plotting
    p1 <- ggplot(plotDF9,
                 aes(Trt, mean)) + 
      geom_bar(stat = "identity", aes(fill=Variable), position="stack") +
      geom_errorbar(data=plotDF3, aes(ymax=pos, ymin=neg), 
                    position = position_dodge(0.9), width=0.2, size=0.4,
                    color="black") +
      geom_point(data=plotDF3, aes(x=Trt, y=mean), size=2, pch=19, color="black")+
      xlab("") + ylab(expression(paste("P demand (g P ", m^-2, " ", yr^-1, ")")))+
      theme_linedraw() +
      ylim(0, 1.0)+
      theme(panel.grid.minor=element_blank(),
            axis.title.x = element_text(size=10), 
            axis.text.x = element_text(size=10),
            axis.text.y=element_text(size=10),
            axis.title.y=element_text(size=10),
            legend.text=element_text(size=14),
            legend.title=element_text(size=16),
            panel.grid.major=element_blank(),
            legend.position="right")+
      scale_fill_manual(name="Flux", 
                        values = c("retrans" = Diverge_hsv_Palette[6],
                                   "uptake" = Diverge_hsv_Palette[8]),
                        labels=c("retrans"="Resorption",
                                 "uptake"="Uptake"))+
      scale_x_discrete(limits=c("aCO2","eCO2"),
                       labels=c(expression(aCO[2]),
                                expression(eCO[2])))
    
    
    p2 <- ggplot(minDF2, aes(x=Trt, y=mean)) + 
      geom_bar(aes(fill=Depth), stat = "identity", position="stack") +
      geom_errorbar(data=plotDF6,
                    aes(x=Trt, ymax=mean+sd, ymin=mean-sd), 
                    position = position_dodge(0.9), width=0.2, 
                    size=0.4) +
      geom_point(data=plotDF6, aes(x=Trt, y=mean), size=2, color="black")+
      xlab("") + ylab(expression(paste("Soil P mineralization (g P ", m^-2, " ", yr^-1, ")")))+
      theme_linedraw() +
      ylim(0, 1.0)+
      theme(panel.grid.minor=element_blank(),
            axis.title.x = element_text(size=10), 
            axis.text.x = element_text(size=10),
            axis.text.y=element_text(size=10),
            axis.title.y=element_text(size=10),
            legend.text=element_text(size=14),
            legend.title=element_text(size=16),
            panel.grid.major=element_blank(),
            legend.position="right")+
      scale_fill_manual(name="Depth", 
                        values = c("0-10cm" = Diverge_hsv_Palette[7],
                                   "10-30cm" = Diverge_hsv_Palette[4],
                                   "30-60cm" = Diverge_hsv_Palette[3]),
                        labels=c("0-10cm"="0-10cm",
                                 "10-30cm"="10-30cm",
                                 "30-60cm"="30-60cm"))+
      scale_x_discrete(limits=c("aCO2","eCO2"),
                       labels=c(expression(aCO[2]),
                                expression(eCO[2])))
    
    
    
    
    
    p3 <- ggplot(demDF, aes(ymax=ymax1, ymin=ymin1, xmax=4, xmin=3, fill=label1)) +
      geom_rect() +
      coord_polar(theta="y")+
      xlim(c(2, 4)) +
      theme_void() +
      #theme_minimal(10)+
      #geom_label(x=3.5, aes(y=labelPosition1, label=aCO2_p), size=6)+
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(),
            panel.grid=element_blank(),
            axis.ticks = element_blank(),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            plot.title=element_text(size=14, face="bold"),
            legend.position="none")+
      #scale_fill_manual(name="P demand flux",
      #                  values = c("Canopy" = Diverge_hsv_Palette[1],
      #                             "Wood" = Diverge_hsv_Palette[2],
      #                             "Fine Root" = Diverge_hsv_Palette[3],
      #                             "Coarse Root" = Diverge_hsv_Palette[4],
      #                             "Twig" = Diverge_hsv_Palette[5],
      #                             "Bark" = Diverge_hsv_Palette[6],
      #                             "Seed" = Diverge_hsv_Palette[7],
      #                             "Frass" = Diverge_hsv_Palette[8],
      #                             "Understorey" = Diverge_hsv_Palette[9]),
      #                  labels=c("Canopy" = "Canopy",
      #                           "Wood" = "Wood",
      #                           "Fine Root" = "Fineroot",
      #                           "Coarse Root" = "Coarseroot",
      #                           "Twig" = "Twig",
      #                           "Bark" = "Bark",
      #                           "Seed" = "Seed",
      #                           "Frass" = "Frass",
      #                           "Understorey" = "Understorey"))+
      scale_fill_brewer(palette=3, name="P demand flux")+
      guides(fill=guide_legend(ncol=2))+
      annotate(geom="text", label=expression(aCO[2]), x=2., y=100, size=8)

    
    
    p4 <- ggplot(demDF, aes(ymax=ymax2, ymin=ymin2, xmax=4, xmin=3, fill=label2)) +
      geom_rect() +
      coord_polar(theta="y")+
      xlim(c(2, 4)) +
      theme_void() +
      #theme_minimal(10)+
      #geom_label( x=3.5, aes(y=labelPosition1, label=label1), size=6)+
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(),
            panel.grid=element_blank(),
            axis.ticks = element_blank(),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            plot.title=element_text(size=14, face="bold"),
            legend.position="none")+
      scale_fill_brewer(palette=3, name="P demand flux")+
      annotate(geom="text", label=expression(eCO[2]), x=2., y=100, size=8)
    
    
    
    
    p5 <- ggplot(retDF, aes(ymax=ymax1, ymin=ymin1, xmax=4, xmin=3, fill=label1)) +
      geom_rect() +
      coord_polar(theta="y")+
      xlim(c(2, 4)) +
      theme_void() +
      #theme_minimal(10)+
      #geom_label( x=3.5, aes(y=labelPosition1, label=label1), size=6)+
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(),
            panel.grid=element_blank(),
            axis.ticks = element_blank(),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            plot.title=element_text(size=14, face="bold"),
            legend.position="none")+
      scale_fill_brewer(palette=3, name="P retranslocation flux")+
      annotate(geom="text", label=expression(aCO[2]), x=2., y=100, size=8)
    
    
    
    p6 <- ggplot(retDF, aes(ymax=ymax2, ymin=ymin2, xmax=4, xmin=3, fill=label2)) +
      geom_rect() +
      coord_polar(theta="y")+
      xlim(c(2, 4)) +
      theme_void() +
      #theme_minimal(10)+
      #geom_label( x=3.5, aes(y=labelPosition1, label=label1), size=6)+
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(),
            panel.grid=element_blank(),
            axis.ticks = element_blank(),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            plot.title=element_text(size=14, face="bold"),
            legend.position="none")+
      scale_fill_brewer(palette=3, name="P retranslocation flux")+
      annotate(geom="text", label=expression(eCO[2]), x=2., y=100, size=8)
    
    
    
    p7 <- ggplot(litDF, aes(ymax=ymax1, ymin=ymin1, xmax=4, xmin=3, fill=label1)) +
      geom_rect() +
      coord_polar(theta="y")+
      xlim(c(2, 4)) +
      theme_void() +
      #theme_minimal(10)+
      #geom_label( x=3.5, aes(y=labelPosition1, label=label1), size=6)+
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(),
            panel.grid=element_blank(),
            axis.ticks = element_blank(),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            plot.title=element_text(size=14, face="bold"),
            legend.position="none")+
      scale_fill_brewer(palette=3, name="P litter flux")+
      annotate(geom="text", label=expression(aCO[2]), x=2., y=100, size=8)+
      guides(fill=guide_legend(ncol=2))
    
    
    p8 <- ggplot(litDF, aes(ymax=ymax2, ymin=ymin2, xmax=4, xmin=3, fill=label2)) +
      geom_rect() +
      coord_polar(theta="y")+
      xlim(c(2, 4)) +
      theme_void() +
      #theme_minimal(10)+
      #geom_label( x=3.5, aes(y=labelPosition1, label=label1), size=6)+
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(),
            panel.grid=element_blank(),
            axis.ticks = element_blank(),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            plot.title=element_text(size=14, face="bold"),
            legend.position="none")+
      scale_fill_brewer(palette=3, name="P litter flux")+
      annotate(geom="text", label=expression(eCO[2]), x=2., y=100, size=8)
    
    
    
    
    dem.legend <- get_legend(p3 + theme(legend.position="right",
                                        legend.box = 'horizontal',
                                        legend.box.just = 'left'))
    
    ret.legend <- get_legend(p5 + theme(legend.position="right",
                                        legend.box = 'horizontal',
                                        legend.box.just = 'left'))
    
    lit.legend <- get_legend(p7 + theme(legend.position="right",
                                        legend.box = 'horizontal',
                                        legend.box.just = 'left'))
    
    
    grid.labs <- c("(a)", "(b)", "(c)", "(d)", "(e)")
    
    
    ## plot 
    pdf(paste0("plots_tables/output/", norm, "/P_Budget_Summary_Plots_", norm, "_2.pdf"), 
        width=10,height=14)
    top_row <- plot_grid(p1, p2, ncol=2)
    dem_row <- plot_grid(p3, p4, dem.legend, NA, ncol=4)
    ret_row <- plot_grid(p5, p6, ret.legend, NA, ncol=4)
    lit_row <- plot_grid(p7, p8, lit.legend, NA, ncol=4)
    plot_grid(top_row, 
              dem_row, 
              ret_row,
              lit_row, 
              ncol = 1, 
              rel_heights=c(1, 1, 1, 1))
    grid.text(grid.labs,x = c(0.1, 0.6, 0.05, 0.05, 0.05), y = c(0.98, 0.98, 0.7, 0.46, 0.2),
              gp=gpar(fontsize=16, col="black", fontface="bold"))
    dev.off()
    
    
    
    
    
    
    
    p1 <- ggplot(plotDF9,
                 aes(Trt, mean)) + 
      geom_bar(stat = "identity", aes(fill=Variable), position="stack", col="black") +
      geom_errorbar(data=plotDF3, aes(ymax=pos, ymin=neg), 
                    position = position_dodge(0.9), width=0.2, size=0.4,
                    color="black") +
      geom_point(data=plotDF3, aes(x=Trt, y=mean), size=2, pch=19, color="black")+
      xlab("") + ylab(expression(paste("P demand (g P ", m^-2, " ", yr^-1, ")")))+
      theme_linedraw() +
      ylim(0, 1.0)+
      theme(panel.grid.minor=element_blank(),
            axis.title.x = element_text(size=12), 
            axis.text.x = element_text(size=10),
            axis.text.y=element_text(size=10),
            axis.title.y=element_text(size=12),
            legend.text=element_text(size=10),
            legend.title=element_text(size=12),
            panel.grid.major=element_blank(),
            legend.position = c(0.8, 0.2),
            legend.background = element_rect(fill="grey",
                                             size=0.5, linetype="solid", 
                                             colour ="black"))+
      scale_fill_manual(name="Flux", 
                        values = c("retrans" = cbbPalette[5],
                                   "uptake" = cbbPalette[3]),
                        labels=c("retrans"="Resorption",
                                 "uptake"="Uptake"))+
      scale_x_discrete(limits=c("aCO2","eCO2"),
                       labels=c(expression(aCO[2]),
                                expression(eCO[2])))
    
    
    
    
    p11 <- ggplot(demDF2, aes(ymax=ymax1, ymin=ymin1, xmax=4, xmin=3, fill=label1)) +
      geom_rect(col="black") +
      coord_polar(theta="y")+
      xlim(c(2, 4)) +
      theme_void() +
      #theme_minimal(10)+
      geom_label(x=3.5, aes(y=labelPosition1, label=aCO2_p), fill="white", size=4)+
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(),
            panel.grid=element_blank(),
            axis.ticks = element_blank(),
            legend.text=element_text(size=10),
            legend.title=element_text(size=12),
            plot.title=element_text(size=12, face="bold"),
            legend.position="none")+
      scale_fill_manual(name="P demand",
                        values = c("Canopy" = Pastel1Palette[2],
                                   "Fine Root" = Pastel1Palette[4],
                                   "Understorey" = Pastel1Palette[5],
                                   "Other" = Pastel1Palette[6]),
                        labels=c("Canopy" = "Canopy",
                                 "Other" = "Other",
                                 "Fine Root" = "Fineroot",
                                 "Understorey" = "Understorey"))+
      #scale_fill_brewer(palette=3, name="P demand flux")+
      guides(fill=guide_legend(ncol=2))+
      annotate(geom="text", label=expression(aCO[2]), x=2., y=100, size=4)
    
    
    p12 <- ggplot(demDF2, aes(ymax=ymax2, ymin=ymin2, xmax=4, xmin=3, fill=label2)) +
      geom_rect(col="black") +
      coord_polar(theta="y")+
      xlim(c(2, 4)) +
      theme_void() +
      #theme_minimal(10)+
      geom_label(x=3.5, aes(y=labelPosition2, label=eCO2_p), fill="white", size=4)+
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(),
            panel.grid=element_blank(),
            axis.ticks = element_blank(),
            legend.text=element_text(size=10),
            legend.title=element_text(size=10),
            plot.title=element_text(size=10, face="bold"),
            legend.position="none")+
      scale_fill_manual(name="P demand",
                        values = c("Canopy" = Pastel1Palette[2],
                                   "Fine Root" = Pastel1Palette[4],
                                   "Understorey" = Pastel1Palette[5],
                                   "Other" = Pastel1Palette[6]),
                        labels=c("Canopy" = "Canopy",
                                 "Other" = "Other",
                                 "Fine Root" = "Fineroot",
                                 "Understorey" = "Understorey"))+
      #scale_fill_brewer(palette=3, name="P demand flux")+
      guides(fill=guide_legend(ncol=2))+
      annotate(geom="text", label=expression(eCO[2]), x=2., y=100, size=4)
    
    
    plot(p12)
    
    
    minDF2$Depth <- gsub("0-10cm", "3_0-10cm", minDF2$Depth)
    minDF2$Depth <- gsub("10-30cm", "2_10-30cm", minDF2$Depth)
    minDF2$Depth <- gsub("30-60cm", "1_30-60cm", minDF2$Depth)
    
    
    p2 <- ggplot(minDF2, aes(x=Trt, y=mean)) + 
      geom_bar(aes(fill=Depth), stat = "identity", position="stack", col="black") +
      geom_errorbar(data=plotDF6,
                    aes(x=Trt, ymax=mean+sd, ymin=mean-sd), 
                    position = position_dodge(0.9), width=0.2, 
                    size=0.4) +
      geom_point(data=plotDF6, aes(x=Trt, y=mean), size=2, color="black")+
      xlab("") + ylab(expression(paste("Soil P mineralization (g P ", m^-2, " ", yr^-1, ")")))+
      theme_linedraw() +
      ylim(0, 1.0)+
      theme(panel.grid.minor=element_blank(),
            axis.title.x = element_text(size=12), 
            axis.text.x = element_text(size=10),
            axis.text.y=element_text(size=10),
            axis.title.y=element_text(size=12),
            legend.text=element_text(size=10),
            legend.title=element_text(size=12),
            panel.grid.major=element_blank(),
            legend.position = c(0.8, 0.2),
            legend.background = element_rect(fill="grey",
                                             size=0.5, linetype="solid", 
                                             colour ="black"))+
      scale_fill_manual(name="Depth", 
                        values = c("3_0-10cm" = YlOrRdPalette[1],
                                   "2_10-30cm" = YlOrRdPalette[3],
                                   "1_30-60cm" = YlOrRdPalette[7]),
                        labels=c("3_0-10cm"="0-10cm",
                                 "2_10-30cm"="10-30cm",
                                 "1_30-60cm"="30-60cm"))+
      scale_x_discrete(limits=c("aCO2","eCO2"),
                       labels=c(expression(aCO[2]),
                                expression(eCO[2])))
    
    #plot(p2)
    
    
    p3 <- ggplot(plotDF10,
                 aes(Component, mean,group=Trt)) + 
      geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
      geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                    position = position_dodge(0.9), width=0.2, size=0.4) +
      xlab("") + ylab("P MRT (yr)")+
      theme_linedraw() +
      ylim(0,14)+
      theme(panel.grid.minor=element_blank(),
            axis.title.x = element_text(size=12), 
            axis.text.x = element_text(size=10),
            axis.text.y=element_text(size=10),
            axis.title.y=element_text(size=12),
            legend.text=element_text(size=10),
            legend.title=element_text(size=12),
            panel.grid.major=element_blank(),
            legend.position="none")+
      scale_fill_manual(name="", values = c("aCO2" = SpectralPalette[7], "eCO2" = SpectralPalette[3]),
                        labels=c(expression(aCO[2]), expression(eCO[2])))+
      scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
      scale_x_discrete(limits=c("Plant","Microbe"),
                       labels=c("Plant",
                                "Microbe"))
    
    
    p4 <- ggplot(plotDF8,
                 aes(Trt, mean)) + 
      geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
      geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                    position = position_dodge(0.9), width=0.2, size=0.4) +
      xlab("") + ylab(expression(paste("Growth PUE ( gC" * " " *gP^-1 * " )")))+
      theme_linedraw() +
      ylim(0,2500)+
      theme(panel.grid.minor=element_blank(),
            axis.title.x = element_text(size=12), 
            axis.text.x = element_text(size=10),
            axis.text.y=element_text(size=10),
            axis.title.y=element_text(size=12),
            legend.text=element_text(size=10),
            legend.title=element_text(size=12),
            panel.grid.major=element_blank(),
            legend.position="none")+
      scale_fill_manual(name="", values = c("aCO2" = SpectralPalette[7], "eCO2" = SpectralPalette[3]),
                        labels=c(expression(aCO[2]), expression(eCO[2])))+
      scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
      scale_x_discrete(limits=c("aCO2","eCO2"),
                       labels=c(expression(aCO[2]),
                                expression(eCO[2])))
    
    
    plotDF10 <- summaryBy(GPP_efficiency_gC_gP~Trt+variable, FUN=c(mean,sd),
                          data=inDF3, na.rm=T, keep.names=T)
    
    
    p5 <- ggplot(plotDF10,
                 aes(x=variable, y=GPP_efficiency_gC_gP.mean, group=Trt)) + 
      geom_bar(stat = "identity", aes(fill=Trt), 
               position="dodge") +
      geom_errorbar(aes(ymax=GPP_efficiency_gC_gP.mean+GPP_efficiency_gC_gP.sd, 
                        ymin=GPP_efficiency_gC_gP.mean-GPP_efficiency_gC_gP.sd), 
                    position = position_dodge(0.9), 
                    width=0.2, size=0.4) +
      labs(x="", 
           y=expression("GPP / Leaf P demand (g C " * g^-1 * " P)"))+
      theme_linedraw() +
      ylim(0,7500)+
      theme(panel.grid.minor=element_blank(),
            axis.title.x = element_text(size=12), 
            axis.text.x = element_text(size=10),
            axis.text.y=element_text(size=10),
            axis.title.y=element_text(size=12),
            legend.text=element_text(size=10),
            legend.title=element_text(size=12),
            panel.grid.major=element_blank(),
            legend.position="none")+
      scale_fill_manual(name="", values = c("aCO2" = SpectralPalette[7], "eCO2" = SpectralPalette[3]),
                        labels=c(expression(aCO[2]), expression(eCO[2])))+
      #scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
      #                    labels=c(expression(aCO[2]), expression(eCO[2])))+
      scale_x_discrete(limits=c("overstorey","understorey"),
                       labels=c("Overstorey",
                                "Understorey"))
    
    
    ### prepare Puptake over requirement, Puptake over mineralization
    tmpDF1 <- inDF[inDF$terms%in%c("Plant P uptake over requirement",
                                  "Plant P uptake over P mineralization"),
                  c("terms", "aCO2", "eCO2")]
    
    tmpDF2 <- inDF[inDF$terms%in%c("Plant P uptake over requirement",
                                   "Plant P uptake over P mineralization"),
                   c("terms", "aCO2_sd", "eCO2_sd")]
    
    plotDF11 <- reshape2::melt(tmpDF1, id.vars=c("terms"))
    plotDF12 <- reshape2::melt(tmpDF2, id.vars=c("terms"))
    plotDF11$sd.value <- plotDF12$value*100
    plotDF11$value <- plotDF11$value * 100
    
    
    p6 <- ggplot(plotDF11,
                 aes(x=terms, y=value, group=variable)) + 
      geom_bar(stat = "identity", aes(fill=variable), 
               position="dodge") +
      geom_errorbar(aes(ymax=value+sd.value, 
                        ymin=value-sd.value), 
                    position = position_dodge(0.9), 
                    width=0.2, size=0.4) +
      labs(x="", 
           y="Efficiency (%)")+
      theme_linedraw() +
      ylim(0,100)+
      theme(panel.grid.minor=element_blank(),
            axis.title.x = element_text(size=12), 
            axis.text.x = element_text(size=10),
            axis.text.y=element_text(size=10),
            axis.title.y=element_text(size=12),
            legend.text=element_text(size=10),
            legend.title=element_text(size=12),
            panel.grid.major=element_blank(),
            legend.position=c(0.7, 0.85))+
      scale_fill_manual(name="Treatment", values = c("aCO2" = SpectralPalette[7], "eCO2" = SpectralPalette[3]),
                        labels=c(expression(aCO[2]), expression(eCO[2])))+
      #scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
      #                    labels=c(expression(aCO[2]), expression(eCO[2])))+
      scale_x_discrete(limits=c("Plant P uptake over requirement",
                                "Plant P uptake over P mineralization"),
                       labels=c("Plant P uptake over requirement"="Uptake/Demand",
                                "Plant P uptake over P mineralization"="Uptake/Mineralization"))
    
    
    grid.labs <- c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)")
    
    
    
    bar_charts1_legend<- get_legend(p11 + theme(legend.position="bottom",
                                                legend.box = 'horizontal',
                                                legend.box.just = 'left'))
    
    
    bar_charts1 <- plot_grid(p11, p12, ncol=1)
    dem_plot <- plot_grid(bar_charts1, bar_charts1_legend, ncol=1, rel_heights=c(1, 0.2))
    
    
    
    ## plot 
    pdf(paste0("plots_tables/output/", norm, "/P_Budget_Summary_Plots_", norm, "_3.pdf"), 
        width=10,height=8)
    top_row <- plot_grid(p1, dem_plot, p2, ncol=3, rel_widths=c(1,1,1))
    bot_row <- plot_grid(p3, p5, p4, ncol=3)
    
    plot_grid(top_row, 
              bot_row, 
              ncol = 1, 
              rel_heights=c(1, 1, 1, 1))
    grid.text(grid.labs,x = c(0.09, 0.42, 0.76, 0.06, 0.44, 0.76), y = c(0.97, 0.97, 0.97, 0.47, 0.47, 0.47),
              gp=gpar(fontsize=16, col="black", fontface="bold"))
    dev.off()
    
    
}


