make_overall_summary_comparison_pct_plot <- function(myDF) {
    
    #### calculate eCO2 - aCO2 difference and % difference
    myDF$diff_mean <- (myDF$eCO2 - myDF$aCO2) / myDF$eCO2 * 100
    
    #### assign categories
    myDF$Category <- c(rep("overall", 10),  
                       rep("overstorey", 6),
                       rep("belowground", 1),
                       rep("understorey", 5),
                       rep("soil", 11))  
    
    ### subset - difference
    subDF1 <- myDF[,c("terms", "diff_mean", "Category")]
    colnames(subDF1) <- c("Variable", "effect_size", "Category")

    
    ### make subplots based on subDF1
    ## plot of p pools
    plotDF1 <- subset(subDF1, Variable%in%c("total standing p stock", "overstorey standing p stock",
                                            "understorey standing p stock", "belowground standing p stock",
                                            "Soil phosphate P pool", "Exhanagable Pi pool",
                                            "Exhanagable Po pool", "Moderately labile Po pool",
                                            "Secondary Pi Fe bound pool", "Primary Pi Ca bound pool",
                                            "Occluded P pool", "Total soil P pool"))
    
    plotDF1$plot_seq <- rev(c(1:length(plotDF1$Variable)))
    
    ## plot of summary fluxes
    plotDF2 <- subset(subDF1, Variable%in%c("total p requirement", "overstorey p requirement",
                                            "understorey p requirement",
                                            "total p retranslocated", "overstorey p retranslocated",
                                            "understorey p retranslocated",
                                            "total p uptake from soil", "overstorey p uptake from soil",
                                            "understorey p uptake from soil",
                                            "soil p mineralization", "P leaching flux"))
    
    plotDF2$plot_seq <- rev(c(1:length(plotDF2$Variable)))
    
    ## plot of ecosystem properties
    plotDF3 <- subset(subDF1, Variable%in%c("total uptake over requirement","total P MRT in plant","total standing PUE"))
    
    
    ### set up labels
    y.lab1 <- c("total standing p stock"=expression(P[veg]), 
                "overstorey standing p stock"=expression(P[oveg]),
                "understorey standing p stock"=expression(P[uveg]),
                "belowground standing p stock"=expression(P[bveg]),
                "Soil phosphate P pool"=expression(P[PO[4]]), 
                "Exhanagable Pi pool"=expression(P[hed1]),
                "Exhanagable Po pool"=expression(P[hed2]),
                "Moderately labile Po pool"=expression(P[hed3]),
                "Secondary Pi Fe bound pool"=expression(P[hed4]), 
                "Primary Pi Ca bound pool"=expression(P[hed5]),
                "Occluded P pool"=expression(P[hed6]), 
                "Total soil P pool"=expression(P[soil]))
    
    y.lab2 <- c("total p requirement"=expression(P[req]), 
                "overstorey p requirement"=expression(P[o[req]]),
                "understorey p requirement"=expression(P[u[req]]),
                "total p retranslocated"=expression(P[ret]), 
                "overstorey p retranslocated"=expression(P[o[ret]]),
                "understorey p retranslocated"=expression(P[u[ret]]),
                "total p uptake from soil"=expression(P[upt]), 
                "overstorey p uptake from soil"=expression(P[o[upt]]),
                "understorey p uptake from soil"=expression(P[u[upt]]),
                "soil p mineralization"=expression(P[min]), 
                "P leaching flux"=expression(P[leach]))
    
    y.lab3 <- c("total uptake over requirement" = expression(Upt/Req),
                "total P MRT in plant" = expression(MRT),
                "total standing PUE" = expression(PUE))
    
    
    ### add conditional color to data frames
    plotDF1$col.con <- ifelse(plotDF1$effect_size<0, "#DF6747", "#37AFA9")
    plotDF2$col.con <- ifelse(plotDF2$effect_size<0, "#DF6747", "#37AFA9")
    plotDF3$col.con <- ifelse(plotDF3$effect_size<0, "#DF6747", "#37AFA9")

    
    ### plotting
    p1 <- ggplot(plotDF1,aes(x=reorder(Variable, plot_seq), y=effect_size))+ 
        geom_bar(position="dodge",stat='identity', fill=plotDF1$col.con)+
        coord_flip()+
        labs(y=expression(paste(CO[2], " effect (%)")), 
             x="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(), 
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_y_continuous(limits=c(-50, 50), 
                           breaks=c(-50, -25, 0, 25, 50), labels=c(-50, -25, 0, 25, 50))+
        scale_x_discrete(labels=y.lab1)
    
    
    
    p2 <- ggplot(plotDF2,aes(x=reorder(Variable, plot_seq), y=effect_size))+ 
        geom_bar(position="dodge",stat='identity', fill=plotDF2$col.con)+
        coord_flip()+
        labs(y=expression(paste(CO[2], " effect (%)")), 
             x="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(), 
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_y_continuous(limits=c(-50, 50), 
                           breaks=c(-50, -25, 0, 25, 50), labels=c(-50, -25, 0, 25, 50))+
        scale_x_discrete(labels=y.lab2)
    
    
    p3 <- ggplot(plotDF3,aes(x=Variable, y=effect_size))+ 
        geom_bar(position="dodge",stat='identity', fill=plotDF3$col.con)+
        coord_flip()+
        labs(y=expression(paste(CO[2], " effect (%)")), 
             x="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(siz=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_y_continuous(limits=c(-50, 50), 
                           breaks=c(-50, -25, 0, 25, 50), labels=c(-50, -25, 0, 25, 50))+
        scale_x_discrete(labels=y.lab3)
    
    

    
    pdf("plots_tables/Summary_figure_CO2_effect.pdf", width=8, height=12)
    require(cowplot)    
    plot_grid(p1, p2, p3,
              labels="auto", ncol=1, align="v", axis = "l",
              rel_heights=c(1.,0.8,0.3))
    dev.off()
    
    
    
}