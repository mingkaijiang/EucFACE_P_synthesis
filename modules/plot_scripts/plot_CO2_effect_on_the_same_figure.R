plot_CO2_effect_on_the_same_figure <- function(budgetDF,
                                               concDF,
                                               poolDF,
                                               fluxDF) {
    
    ### data cleaning
    budgetDF <- budgetDF[,c("terms", "aCO2", "eCO2", "aCO2_sd", "eCO2_sd")]
    concDF <- concDF[,c("conc.terms", "aCO2", "eCO2", "aCO2_sd", "eCO2_sd")]
    poolDF <- poolDF[,c("terms", "aCO2", "eCO2", "aCO2_sd", "eCO2_sd")]
    fluxDF <- fluxDF[,c("terms", "aCO2", "eCO2", "aCO2_sd", "eCO2_sd")]
    
    colnames(concDF) <- c("terms", "aCO2", "eCO2", "aCO2_sd", "eCO2_sd")
    
    ### add group column
    budgetDF$Group <- "1_budget"
    concDF$Group <- "2_concentration"
    poolDF$Group <- "3_pool"
    fluxDF$Group <- "4_flux"
    
    
    ### merge all
    myDF <- rbind(budgetDF, rbind(concDF, rbind(poolDF, fluxDF)))
    
    
    ### calculate absolute difference and sd
    myDF$diff_mean <- myDF$eCO2 - myDF$aCO2
    myDF$diff_sd <- sqrt((myDF$aCO2_sd^2 + myDF$eCO2_sd^2)/2)
    
    myDF <- myDF[complete.cases(myDF$diff_mean),]
    
 
    #### plotting preparation
    plotDF1 <- subset(myDF, Group=="1_budget")
    plotDF2 <- subset(myDF, Group=="2_concentration")
    plotDF3 <- subset(myDF, Group=="3_pool")
    plotDF4 <- subset(myDF, Group=="4_flux")
    
    
    ### plotDF1 split into multiple sub DFs
    subDF1 <- plotDF1[plotDF1$terms%in%c("Total plant P stock",
                                         "Labile Pi stock",
                                         "Overstorey aboveground P stock",
                                         "Understorey aboveground P stock",
                                         "Belowground P stock",
                                         "Dead P stock"),]
    
    subDF2 <- plotDF1[plotDF1$terms%in%c("Total plant P requirement flux",
                                         "Total plant P retranslocation flux",
                                         "Plant P uptake flux",
                                         "Soil P mineralization flux"),]
    
    
    subDF3 <- plotDF1[plotDF1$terms%in%c("Plant P uptake over requirement",
                                         "Plant P uptake over P mineralization",
                                         "Leaflitter P over P mineralization",
                                         "Fineroot litter P over P mineralization",
                                         "Twig litter P over P mineralization",
                                         "Bark litter P over P mineralization",
                                         "Seed litter P over P mineralization",
                                         "Frass litter P over P mineralization",
                                         "Understorey litter P over P mineralization",
                                         "Leaching P over P mineralization"),]
    
    ### make plots
    p1 <- ggplot(subDF1) +  
        geom_segment(aes(y=reorder(terms, diff_mean), x=diff_mean-diff_sd, 
                         yend=reorder(terms, diff_mean), xend=diff_mean+diff_sd), 
                     size=6, color="grey")+
        geom_point(aes(y=reorder(terms, diff_mean), x=diff_mean), 
                   stat='identity', size=4, shape=19)+
        geom_vline(xintercept=0)+
        xlab(expression(paste(CO[2], " effect (g P ", m^-2, ")"))) + 
        ylab("") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)
    
    
    p2 <- ggplot(subDF2) +  
        geom_segment(aes(y=reorder(terms, diff_mean), x=diff_mean-diff_sd, 
                         yend=reorder(terms, diff_mean), xend=diff_mean+diff_sd), 
                     size=6, color="grey")+
        geom_point(aes(y=reorder(terms, diff_mean), x=diff_mean), 
                   stat='identity', size=4, shape=19)+
        geom_vline(xintercept=0)+
        xlab(expression(paste(CO[2], " effect (g P ", m^-2, " ", yr^-1,")"))) + 
        ylab("") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)
    
    
    p3 <- ggplot(subDF3) +  
        geom_segment(aes(y=reorder(terms, diff_mean), x=diff_mean-diff_sd, 
                         yend=reorder(terms, diff_mean), xend=diff_mean+diff_sd), 
                     size=6, color="grey")+
        geom_point(aes(y=reorder(terms, diff_mean), x=diff_mean), 
                   stat='identity', size=4, shape=19)+
        geom_vline(xintercept=0)+
        xlab(expression(paste(CO[2], " effect (unitless)"))) + 
        ylab("") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)
    
    
    p4 <- ggplot(plotDF2) +  
        geom_segment(aes(y=reorder(terms, diff_mean), x=diff_mean-diff_sd, 
                         yend=reorder(terms, diff_mean), xend=diff_mean+diff_sd), 
                     size=6, color="grey")+
        geom_point(aes(y=reorder(terms, diff_mean), x=diff_mean), 
                   stat='identity', size=4, shape=19)+
        geom_vline(xintercept=0)+
        xlab(expression(paste(CO[2], " effect (%)"))) + 
        ylab("") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)
    
    
    p5 <- ggplot(plotDF3) +  
        geom_segment(aes(y=reorder(terms, diff_mean), x=diff_mean-diff_sd, 
                         yend=reorder(terms, diff_mean), xend=diff_mean+diff_sd), 
                     size=6, color="grey")+
        geom_point(aes(y=reorder(terms, diff_mean), x=diff_mean), 
                   stat='identity', size=4, shape=19)+
        geom_vline(xintercept=0)+
        xlab(expression(paste(CO[2], " effect (g P ", m^-2, ")"))) + 
        ylab("") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)

    
    
    p6 <- ggplot(plotDF4) +  
        geom_segment(aes(y=reorder(terms, diff_mean), x=diff_mean-diff_sd, 
                         yend=reorder(terms, diff_mean), xend=diff_mean+diff_sd), 
                     size=6, color="grey")+
        geom_point(aes(y=reorder(terms, diff_mean), x=diff_mean), 
                   stat='identity', size=4, shape=19)+
        geom_vline(xintercept=0)+
        xlab(expression(paste(CO[2], " effect (g P ", m^-2, " ", yr^-1, ")"))) + 
        ylab("") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)
    
    
    require(grid)
    require(cowplot)
    
    ### Plotting
    jpeg("plots_tables/output/unnormalized/CO2_effect_on_total_p_budget.jpg", 
         width=150, height=300, unit="mm", res = 300)
    plot_grid(p1, p2, p3, labels="", ncol=1, align="h", axis="l",
              rel_heights=c(1., 1.0, 1.5))
    dev.off()
    
    jpeg("plots_tables/output/unnormalized/CO2_effect_on_all_p_variables.jpg", 
         width=150, height=500, unit="mm", res = 300)
    plot_grid(p4, p5, p6, labels="", ncol=1, align="h", axis="l",
              rel_heights=c(1.0, 1.0, 1.0))
    dev.off()
    
}
