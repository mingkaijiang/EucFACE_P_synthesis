make_soil_p_budget_summary_plots <- function() {
    
    ################### Plot all P summary budget plots
    ### summarize each variable
    pDF1 <- summaryBy(predicted~Trt+Datef, FUN=mean, keep.names=T, data=soil_exhanagable_pi_pool_pred)
    pDF <- summaryBy(predicted~Trt+Datef, FUN=sd, keep.names=T, data=soil_exhanagable_pi_pool_pred)
    pDF1$sd <- pDF$predicted
    
    pDF2 <- summaryBy(predicted~Trt+Datef, FUN=mean, keep.names=T, data=soil_exhanagable_po_pool_pred)
    pDF <- summaryBy(predicted~Trt+Datef, FUN=sd, keep.names=T, data=soil_exhanagable_po_pool_pred)
    pDF2$sd <- pDF$predicted
    
    pDF3 <- summaryBy(predicted~Trt+Datef, FUN=mean, keep.names=T, data=soil_mlabile_po_pool_pred)
    pDF <- summaryBy(predicted~Trt+Datef, FUN=sd, keep.names=T, data=soil_mlabile_po_pool_pred)
    pDF3$sd <- pDF$predicted
    
    pDF4 <- summaryBy(predicted~Trt+Datef, FUN=mean, keep.names=T, data=soil_secondary_pi_pool_pred)
    pDF <- summaryBy(predicted~Trt+Datef, FUN=sd, keep.names=T, data=soil_secondary_pi_pool_pred)
    pDF4$sd <- pDF$predicted
    
    pDF5 <- summaryBy(predicted~Trt+Datef, FUN=mean, keep.names=T, data=soil_primary_pi_pool_pred)
    pDF <- summaryBy(predicted~Trt+Datef, FUN=sd, keep.names=T, data=soil_primary_pi_pool_pred)
    pDF5$sd <- pDF$predicted
    
    pDF6 <- summaryBy(predicted~Trt+Datef, FUN=mean, keep.names=T, data=soil_occluded_p_pool_pred)
    pDF <- summaryBy(predicted~Trt+Datef, FUN=sd, keep.names=T, data=soil_occluded_p_pool_pred)
    pDF6$sd <- pDF$predicted
    
    soilpDF <- summaryBy(predicted~Trt, FUN=c(mean, sd), keep.names=T, data=soil_p_pool_pred)
    pDF6$predicted[pDF6$Trt=="amb"] <- soilpDF$predicted.mean[soilpDF$Trt=="amb"] - (pDF1$predicted[pDF1$Trt=="amb"]+
                                                                                         pDF2$predicted[pDF2$Trt=="amb"]+
                                                                                         pDF3$predicted[pDF3$Trt=="amb"]+
                                                                                         pDF4$predicted[pDF4$Trt=="amb"]+
                                                                                         pDF5$predicted[pDF5$Trt=="amb"])
    
    pDF6$predicted[pDF6$Trt=="ele"] <- soilpDF$predicted.mean[soilpDF$Trt=="ele"] - (pDF1$predicted[pDF1$Trt=="ele"]+
                                                                                         pDF2$predicted[pDF2$Trt=="ele"]+
                                                                                         pDF3$predicted[pDF3$Trt=="ele"]+
                                                                                         pDF4$predicted[pDF4$Trt=="ele"]+
                                                                                         pDF5$predicted[pDF5$Trt=="ele"])
    
    pDF7 <- summaryBy(predicted~Trt+Datef, FUN=mean, keep.names=T, data=soil_aqua_p_pool_pred)
    pDF <- summaryBy(predicted~Trt+Datef, FUN=sd, keep.names=T, data=soil_aqua_p_pool_pred)
    pDF7$sd <- pDF$predicted
    
    plotDF1 <- rbind(pDF1, pDF2, pDF3, pDF4, pDF5, pDF6, pDF7)
    plotDF1$Pool <- rep(c("1_Exhanagable_Pi", "2_Exhanagable_Po", "3_Moderately_labile_Po",
                          "4_Secondary_Pi", "5_Primary_Pi", "6_Occluded_P", "Aqua_total_P"), each=4)

    ### Plotting
    plotDF2 <- subset(plotDF1, Datef==2013)
    plotDF2 <- plotDF2[!plotDF2$Pool=="Aqua_total_P",]

    
    p1 <- ggplot(plotDF2,
                 aes(Trt, predicted)) + 
        geom_bar(stat = "identity", aes(fill=Pool), position="stack")+
        xlab(expression(paste(CO[2]," Treatment")))+ 
        ylab(expression(paste("Soil P stock (g P ", m^-2, ")")))+
        theme_linedraw() +
        ylim(0, 15)+
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
                          values = c("1_Exhanagable_Pi" = cbPalette[1], "2_Exhanagable_Po" = cbPalette[2],
                                     "3_Moderately_labile_Po" = cbPalette[3], "4_Secondary_Pi" = cbPalette[4],
                                     "5_Primary_Pi" = cbPalette[5], "6_Occluded_P" = cbPalette[6]),
                          labels=c("Exchangeable Pi", "Exchangeable Po", 
                                   "Moderately labile Po", "Secondary Pi (Fe-bound)",
                                   "Primary Pi (Ca-bound)", "Occluded P"))+
        scale_x_discrete(limits=c("amb","ele"),
                         labels=c(expression(aCO[2]),
                         expression(eCO[2])))
    
    
    ### organic, inorganic and residual
    

    #grid.labs <- c("(a)", "(b)")
    
    ## plot 
    pdf("plots_tables/Summary_Soil_P_Budget_Plots.pdf", width=8,height=8)
    #plot_grid(p3, p4, p5, p6, p1, p2, labels="", ncol=2, align="v", axis = "l",
    #          rel_heights = c(1, 1, 1.2))
    #grid.text(grid.labs, x = c(0.11, 0.60, 0.11, 0.60, 0.11, 0.60),
    #          y = c(0.95, 0.95, 0.65, 0.65, 0.34, 0.34), 
    #          gp=gpar(fontsize=14, col="black", fontface="bold"))
    plot(p1)
    dev.off()
    
}


