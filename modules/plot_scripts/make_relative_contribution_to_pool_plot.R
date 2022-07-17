make_relative_contribution_to_pool_plot <- function(inDF,norm) {
    
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
                            labels=c(expression(aCO[2]), expression(eCO[2])))
    
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
        width=12,height=8)
    bot_row <- plot_grid(p2, p3, p4, ncol=3)
    plot_grid(p1, bot_row,  ncol = 1, rel_widths = c(1, 0.6,0.6,0.6),
              rel_heights=c(1.2, 1, 1, 1))
    grid.text(grid.labs,x = c(0.15, 0.12, 0.45, 0.75), y = c(0.9, 0.42, 0.42, 0.42),
              gp=gpar(fontsize=16, col="black", fontface="bold"))
    dev.off()
    

    
}


