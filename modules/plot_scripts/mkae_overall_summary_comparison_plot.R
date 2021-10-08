make_overall_summary_comparison_plot <- function(myDF) {
    
    #### calculate eCO2 - aCO2 difference and % difference
    myDF$diff_mean <- myDF$eCO2 - myDF$aCO2
    myDF$diff_sd <- sqrt((myDF$aCO2_sd^2 + myDF$eCO2_sd^2 ) / 2)
    
    #### assign categories
    myDF$Category <- c(rep("overall", 10),  
                       rep("overstorey", 6),
                       rep("belowground", 1),
                       rep("understorey", 5),
                       rep("soil", 11))  
    
    ### subset - difference
    subDF1 <- myDF[,c("terms", "diff_mean", "diff_sd", "Category")]
    colnames(subDF1) <- c("Variable", "effect_size", "sd", "Category")
    subDF1$conf_low <- subDF1$effect_size - subDF1$sd
    subDF1$conf_high <- subDF1$effect_size + subDF1$sd
    
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
    plotDF3 <- subset(subDF1, Variable%in%c("total uptake over requirement"))
    plotDF4 <- subset(subDF1, Variable%in%c("total P MRT in plant"))
    plotDF5 <- subset(subDF1, Variable%in%c("total standing PUE"))
    
    
    ### transform the data so that we could have break point on x axis
    #Function to transform data to y positions
    trans <- function(x) {
        if (x < 0) {
            pmax(x,-0.1) + 0.1*pmin(x+0.1,0)
        } else {
            pmin(x,0.1) + 0.1*pmax(x-0.1,0)
        }
    }
    
    ##Transform the data onto the display scale
    for (i in 1:length(plotDF1$Variable)) {
        plotDF1$effect_size_t[i] <- trans(plotDF1$effect_size[i])
        plotDF1$conf_low_t[i] <- trans(plotDF1$conf_low[i])
        plotDF1$conf_high_t[i] <- trans(plotDF1$conf_high[i])
    }   
    
    
    ### set up break points
    xticks.brk1 <- xticks.label1 <- c(-1.1, -0.5, -0.1, -0.05, 0, 0.05, 0.1, 0.5, 2.0)
    
    for (i in 1:length(xticks.label1)) {
        xticks.brk1[i] <- trans(xticks.label1[i])
    }
    
    
    xticks.brk2 <- xticks.label2 <- c(-0.11, -0.05, 0, 0.05, 0.1, 0.18)
    
    
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
    
    y.lab3 <- c("total uptake over requirement" = expression(Upt/Req))
    y.lab4 <- c("total P MRT in plant" = expression(MRT))
    y.lab5 <- c("total standing PUE" = expression(PUE))
    
    
    ### add conditional color to data frames
    plotDF1$col.con <- ifelse(plotDF1$effect_size_t<0, "#DF6747", "#37AFA9")
    plotDF2$col.con <- ifelse(plotDF2$effect_size<0, "#DF6747", "#37AFA9")
    plotDF3$col.con <- ifelse(plotDF3$effect_size<0, "#DF6747", "#37AFA9")
    plotDF4$col.con <- ifelse(plotDF4$effect_size<0, "#DF6747", "#37AFA9")
    plotDF5$col.con <- ifelse(plotDF5$effect_size<0, "#DF6747", "#37AFA9")
    
    
    ### plotting
    p1 <- ggplot(plotDF1)+ 
        geom_segment(aes(y=reorder(Variable, plot_seq), x=conf_low_t, 
                         yend=reorder(Variable, plot_seq), xend=conf_high_t), 
                     size=6, color="grey")+
        geom_point(aes(y=reorder(Variable, plot_seq), x=effect_size_t), 
                   stat='identity', size=4, shape=19, color=plotDF1$col.con)+
        labs(x=expression(paste(CO[2], " effect (g P ", m^-2, ")")), 
             y="")+
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
        geom_vline(xintercept = 0.0)+
        geom_vline(xintercept = -0.1, linetype="dashed", color="grey")+
        geom_vline(xintercept = 0.1, linetype="dashed", color="grey")+
        scale_x_continuous(limits=c(min(xticks.brk1), max(xticks.brk1)), 
                           breaks=xticks.brk1, labels=xticks.label1)+
        scale_y_discrete(labels=y.lab1)
    
    
    
    p2 <- ggplot(plotDF2)+ 
        geom_segment(aes(y=reorder(Variable, plot_seq), x=conf_low, 
                         yend=reorder(Variable, plot_seq), xend=conf_high), 
                     size=6, color="grey")+
        geom_point(aes(y=reorder(Variable, plot_seq), x=effect_size), 
                   stat='identity', size=4, shape=19, color=plotDF2$col.con)+
        labs(x=expression(paste(CO[2], " effect (g P ", m^-2, " ", yr^-1, ")")), 
             y="")+
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
        geom_vline(xintercept = 0.0)+
        #geom_vline(xintercept = -0.1, linetype="dashed", color="grey")+
        #geom_vline(xintercept = 0.1, linetype="dashed", color="grey")+
        scale_x_continuous(limits=c(min(xticks.brk2), max(xticks.brk2)), 
                           breaks=xticks.brk2, labels=xticks.label2)+
        scale_y_discrete(labels=y.lab2)
    
    
    p3 <- ggplot(plotDF3)+ 
        geom_segment(aes(y=Variable, x=conf_low, 
                         yend=Variable, xend=conf_high), 
                     size=6, color="grey")+
        geom_point(aes(y=Variable, x=effect_size), 
                   stat='identity', size=4, shape=19, color=plotDF3$col.con)+
        labs(x=expression(paste(CO[2], " effect (%)")), 
             y="")+
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
        geom_vline(xintercept = 0.0)+
        #scale_x_continuous(limits=c(min(xticks.brk2), max(xticks.brk2)), 
        #                   breaks=xticks.brk2, labels=xticks.label2)+
        scale_y_discrete(labels=y.lab3)
    
    
    pdf("plots_tables/test.pdf")
    plot(p3)
    dev.off()
    
    
    
    
    
    
    pdf("plots_tables/Summary_figure_CO2_effect.pdf", width=8, height=12)
    require(cowplot)    
    plot_grid(p1, p2, p3 ,
              labels="auto", ncol=1, align="v", axis = "l",
              rel_heights=c(1,0.8,0.25))
    dev.off()
    
    
    
}