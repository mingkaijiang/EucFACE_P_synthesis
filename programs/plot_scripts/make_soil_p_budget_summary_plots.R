make_soil_p_budget_summary_plots <- function(inDF) {
    
    ################### Plot all P summary budget plots
    plotDF1 <- inDF[inDF$terms%in%c("Exchangeable Pi Pool", "Exchangeable Po Pool",
                                    "Moderately labile Po Pool", "Secondary Fe bound Pi Pool",
                                    "Primary Ca bound Pi Pool", "Occluded P Pool", "Soil P Pool"),]
    
    subDF1 <- data.frame(plotDF1$terms, plotDF1$aCO2, plotDF1$aCO2_sd)
    subDF2 <- data.frame(plotDF1$terms, plotDF1$eCO2, plotDF1$eCO2_sd)
    colnames(subDF1) <- colnames(subDF2) <- c("terms", "mean", "sd")
    plotDF2 <- rbind(subDF1, subDF2)
    
    plotDF2$Pool <- rep(c("Total_P", "1_Exchangeable_Pi", "2_Exchangeable_Po", "3_Moderately_labile_Po",
                          "4_Secondary_Pi", "5_Primary_Pi", "6_Occluded_P"), 2)
    plotDF2$Trt <- rep(c("amb", "ele"), each=7)
    
    plotDF3 <- plotDF2[plotDF2$Pool%in%c("1_Exchangeable_Pi", "2_Exchangeable_Po", "3_Moderately_labile_Po",
                                          "4_Secondary_Pi", "5_Primary_Pi", "6_Occluded_P"),]
    
    plotDF4 <- plotDF2[plotDF2$Pool%in%c("Total_P"),]

    p1 <- ggplot(plotDF3,
                 aes(Trt, mean)) + 
        geom_bar(stat = "identity", aes(fill=Pool), position="stack")+
        geom_point(data=plotDF4, aes(Trt, mean))+
        geom_errorbar(data=plotDF4, aes(x=Trt, ymin=mean-sd, ymax=mean+sd),
                      width=0.25)+
        xlab(expression(paste(CO[2]," Treatment")))+ 
        ylab(expression(paste("Soil P stock (g P ", m^-2, ")")))+
        theme_linedraw() +
        ylim(0, 12)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="right")+
        scale_fill_manual(name="Hedley P pools", 
                          values = c("1_Exchangeable_Pi" = cbPalette[1], "2_Exchangeable_Po" = cbPalette[2],
                                     "3_Moderately_labile_Po" = cbPalette[3], "4_Secondary_Pi" = cbPalette[4],
                                     "5_Primary_Pi" = cbPalette[5], "6_Occluded_P" = cbPalette[6]),
                          labels=c("Exchangeable Pi", "Exchangeable Po", 
                                   "Moderately labile Po", "Secondary Pi (Fe-bound)",
                                   "Primary Pi (Ca-bound)", "Occluded P"))+
        scale_x_discrete(limits=c("amb","ele"),
                         labels=c(expression(aCO[2]),
                         expression(eCO[2])))
    
    
    #plot(p1)
    


    ## plot 
    pdf(paste0("plots_tables/Soil_P_Budget_Plots_", norm, ".pdf"), 
        width=8,height=8)
    plot(p1)
    dev.off()
    
}


