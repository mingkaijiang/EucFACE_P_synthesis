compare_hedley_p_data <- function() {
    
    yangDF <- read.csv("temp_files/HedleyP_database.csv")
    
    ### calculate exchangeable Pi fraction 
    yangDF$ExchangeablePi <- with(yangDF, (Resin.Pi))
    
    yangDF$ExchangeablePiFrac <- with(yangDF, ExchangeablePi/Total.P*100)
    
    
    ### Hou 2017
    houDF <- read.csv("temp_files/Hou_2017.csv")
    houDF$ExchangeablePiFrac <- with(houDF, Avail_Pinorg/Total_P*100)
    houDF <- houDF[complete.cases(houDF$ExchangeablePiFrac),]
    n <- dim(houDF)[1]

    ### calculate exchangeable Pi fraction
    myDF <- read.csv("temp_files/Hedley_P_2013_Summary_12-2-19.csv")

    myDF$ExchangeablePiFrac <- with(myDF, F1_2_Pi_Exhanagable/Total_Aqua_Regia_P*100)

    v1 <- summaryBy(F1_2_Pi_Exhanagable+Total_Aqua_Regia_P+ExchangeablePiFrac~Treatment,
                    FUN=c(mean,sd), data=myDF, keep.names=T, na.rm=T)
    
    
    
    #p3 <- ggplot(yangDF) +
    #    geom_violin(aes(x=Soil.Order, y=ExchangeablePiFrac, fill=Soil.Order), alpha=0.4, outlier.alpha = 0)+
    #    geom_jitter(aes(x=Soil.Order, y=ExchangeablePiFrac, fill=Soil.Order), pch=21)+
    #    geom_hline(yintercept=v1$ExchangeablePiFrac.mean[v1$Treatment=="Amb"], col="red", size = 2)+
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.text.x=element_text(size=12),
    #          axis.title.x=element_text(size=14),
    #          axis.text.y=element_text(size=12),
    #          axis.title.y=element_text(size=14),
    #          legend.text=element_text(size=12),
    #          legend.title=element_text(size=14),
    #          panel.grid.major=element_blank(),
    #          legend.text.align = 0,
    #          legend.position="none",
    #          legend.box = 'vertical',
    #          legend.box.just = 'left')#+
    #    #scale_y_continuous(name=expression("Soil P concentration (g P " * kg^-1 * ")"), 
    #    #                   breaks=c(log(0.01), log(0.1), log(1), log(10)), 
    #    #                   labels=c(0.01, 0.1, 1, 10), 
    #    #                   limits=c(-5, 3))+
    #    #scale_fill_manual(name="Leaf form",
    #    #                  labels=c("needleleaf"="Needleleaf", 
    #    #                           "broadleaf"="Broadleaf"),
    #    #                  values=c("green", "brown"))
    
    
    
    #p4 <- ggplot(yangDF, aes(x=ExchangeablePiFrac)) +
    #    #geom_histogram(aes(y=..density..), binwidth=1, color="black", fill="white")+
    #    geom_histogram(houDF, aes(x=ExchangeablePiFrac, y=..density..))+
    #    geom_density(alpha=.2, fill="#FF6666") +
    #    geom_vline(xintercept=v1$ExchangeablePiFrac.mean[v1$Treatment=="Amb"], col="red", size = 2)+
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.text.x=element_text(size=12),
    #          axis.title.x=element_text(size=14),
    #          axis.text.y=element_text(size=12),
    #          axis.title.y=element_text(size=14),
    #          legend.text=element_text(size=12),
    #          legend.title=element_text(size=14),
    #          panel.grid.major=element_blank(),
    #          legend.text.align = 0,
    #          legend.position="none",
    #          legend.box = 'vertical',
    #          legend.box.just = 'left')
    
    
    
    
    p1 <- ggplot(houDF, aes(x=ExchangeablePiFrac)) +
        geom_histogram(aes(y=..density..), binwidth=1, color="black", fill="white")+
        geom_density(alpha=.2, fill="yellow") +
        geom_vline(xintercept=v1$ExchangeablePiFrac.mean[v1$Treatment=="Amb"],
                   col="red", size = 0.6)+
        annotate(geom="text", x=40, y=0.13, label=paste0("n = ", n), size=4)+
        xlab(expression("Fraction of extractable " * P[i] * " (%)"))+
        ylab("Density")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.text.align = 0,
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'left')
    
    
    pdf("plots_tables/output/unnormalized/hedley_comparison.pdf", 
        width=4, height=4)
    plot(p1)
    dev.off()
    
    
    
    
    
    
    
    
    
    
    
    
    ### read in Xu et al. dataset
    xuDF <- read.csv("temp_files/Xu_etal_2013/Soil_Microbial_Biomass_C_N_P_spatial.csv",
                     header=T)
    xuDF <- xuDF[-1,]
    
    xuDF$Soil_organic_carbon <- as.numeric(xuDF$Soil_organic_carbon)
    xuDF$Soil_microbial_biomass_carbon <- as.numeric(xuDF$Soil_microbial_biomass_carbon)
    xuDF$Soil_microbial_biomass_phosphorus <- as.numeric(xuDF$Soil_microbial_biomass_phosphorus)
    xuDF$Total_organic_phosphorus <- as.numeric(xuDF$Total_organic_phosphorus)
    
    ### prepare unit from mmol kg-1 to %
    xuDF$Soil_organic_carbon <- xuDF$Soil_organic_carbon * 12.0 / 10000
    xuDF$Soil_microbial_biomass_carbon <- xuDF$Soil_microbial_biomass_carbon * 12.0 / 10000
    xuDF$Soil_microbial_biomass_phosphorus <- xuDF$Soil_microbial_biomass_phosphorus * 31.0 / 10000
    xuDF$Total_organic_phosphorus <- xuDF$Total_organic_phosphorus * 31.0 / 10000
    
    xuDF$MicrobeP_Frac <- with(xuDF,Soil_microbial_biomass_phosphorus/Total_organic_phosphorus*100)
    
    #xuDF1 <- subset(xuDF, Depth%in%c("0~10",
    #                                 "0~3", "0~2.8", "0~2.7",
    #                                 "0~2.3", "0~5", "4~12",
    #                                 "3~10", "6~12", "3~11", 
    #                                 "2~6", "1.5~5", "0~2.5",
    #                                 "2.5~5", "5~10", "2.5~7.5",
    #                                 "0~2", "2~10", "0~7.5",
    #                                 "Surface (0~10)","1~3", 
    #                                 "0~8","0~6", "5~7.5", "7.5~10",
    #                                 "3.6~5", "2~3.6", "3~4", "0~11",
    #                                 "4~10","0.5~11", "0~9", "0~4", "1",
    #                                 "3", "8"))
    
    
    
    xuDF1 <- subset(xuDF, !Depth%in%c("organic layer",
                                     #"30~40", "40~50", "50~60",
                                     #"20~40", 
                                     "Not mentioned", "L layer",
                                     "F layer", "4~0", "organic horizon", 
                                     "mineral horizon", "O layer", "FH layer"))
    
    xuDF1$Depth <- "0~30"
    
    xuDF1 <- xuDF1[xuDF1$Biome%in%c("Boreal Forest", "Temperate Broadleaf Forest",
                                    "Temperate Coniferous Forest", "Tropical/Subtropical Forest"),]
    
    xuDF1 <- xuDF1[complete.cases(xuDF1$MicrobeP_Frac),]
    n1 <- dim(xuDF1)[1]
    
    
    ### EucFACE values under ambient conditions
    v1 <- 5.97 # microbial P
    v2 <- 25.1 # organic P
    v3 <- v1/v2 * 100
    
    
    subDF <- xuDF1[xuDF1$Biome%in%c("Temperate Broadleaf Forest",
                                  "Temperate Coniferous Forest",
                                  "Tropical/Subtropical Forest",
                                  "Boreal Forest"),]
    
    n2 <- dim(subDF)[1]
    
    
    
    ### prepare density plot
    d1 <- density(subDF$MicrobeP_Frac, from=0,na.rm=T)
    
    ### create data frame
    xd1 <- data.frame(d1[c("x", "y")])
    
    ### find probability distribution marks
    #probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
    probs <- c(0.25, 0.5, 0.75, 0.9)
    quantiles1 <- quantile(subDF$MicrobeP_Frac, prob=probs,na.rm=T)
    
    
    xd1$quant <- factor(findInterval(xd1$x,quantiles1))
    
    spectral.colors <- brewer.pal(5,"Spectral")
    
    
    
    
    
   # p2 <- ggplot(subDF, aes(x=MicrobeP_Frac)) +
   #     geom_histogram(aes(y=..density..), binwidth=1, color="black", fill="white")+
   #     geom_density(alpha=.2, fill="yellow") +
   #     geom_vline(xintercept=v3,
   #                col="red", size = 0.6)+
   #     annotate(geom="text", x=16, y=0.085, label=paste0("n = ", n2), size=8)+
   #     xlab(expression("Fraction of microbial " * P[o] * " (%)"))+
   #     ylab("Density")+
   #     theme_linedraw() +
   #     theme(panel.grid.minor=element_blank(),
   #           axis.text.x=element_text(size=12),
   #           axis.title.x=element_text(size=14),
   #           axis.text.y=element_text(size=12),
   #           axis.title.y=element_text(size=14),
   #           legend.text=element_text(size=12),
   #           legend.title=element_text(size=14),
   #           panel.grid.major=element_blank(),
   #           legend.text.align = 0,
   #           legend.position="none",
   #           legend.box = 'vertical',
   #           legend.box.just = 'left')
    
    
    
    p2 <- ggplot(xd1, aes(x,y)) + 
        geom_line() + 
        geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + 
        #scale_x_continuous(breaks=c(0, 100, 500, 1000),
        #                   labels=c(0, 100, 500, ">1000"),
        #                   limits = c(0,1000),expand=c(0,0)) + 
        scale_fill_manual(name="",
                          limits=c(0:4),
                          labels=c("0-25th", "25-50th", "50-75th", "75-90th", "90-100th"),
                          values=spectral.colors,
                          guide=guide_legend(nrow=5))+
        geom_vline(xintercept=v3,
                   col="black", size = 0.6)+
        annotate(geom="text", x=16, y=0.06, label=paste0("n = ", n2), size=6)+
        xlab(expression("Fraction of soil organic P as microbes (%)"))+
        ylab("Density")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_blank(),
              panel.grid.major=element_blank(),
              legend.text.align = 0,
              legend.box.background = element_rect(alpha("grey",0.5)),
              legend.position=c(0.7, 0.83),
              legend.box = 'vertical',
              legend.box.just = 'left')+
        guides(fill=guide_legend(ncol=2))
    
    plot(p2)
    
    
    
    ### remove not mentioned
    sumDF <- summaryBy(Soil_microbial_biomass_phosphorus~Biome, 
                       FUN=c(mean, sd), data=xuDF, na.rm=T, keep.names=T)
    
    sumDF <- sumDF[complete.cases(sumDF$Soil_microbial_biomass_phosphorus.mean),]
    colnames(sumDF) <- c("Biome", "Pmic.mean", 
                         "Pmic.sd")
    
    sumDF <- sumDF[sumDF$Biome%in%c("Temperate Broadleaf Forest",
                                    "Temperate Coniferous Forest",
                                    "Tropical/Subtropical Forest",
                                    "Boreal Forest"),]
    
    ### prepare eucface
    #eucDF <- data.frame("Biome"="EucFACE",
    #                    "Pmic.mean"=0.001040160,
    #                    "Pmic.sd"=0.001227397)
    
    plotDF <- rbind(eucDF, sumDF)
    
    #p3 <- ggplot(data=plotDF, aes(Biome, Pmic.mean)) +
    #    geom_errorbar(aes(ymin=Pmic.mean-Pmic.sd, ymax=Pmic.mean+Pmic.sd),
    #                  width=0.2)+
    #    geom_point(aes(fill=Biome), pch=21, size=3)+
    #    #geom_vline(xintercept=2.5, lty = 2)+
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.title.x = element_text(size=14), 
    #          axis.text.x = element_text(size=14),
    #          axis.text.y=element_text(size=14),
    #          axis.title.y=element_text(size=14),
    #          legend.text=element_text(size=14),
    #          legend.title=element_text(size=14),
    #          panel.grid.major=element_blank(),
    #          legend.position="none",
    #          legend.text.align=0)+
    #    scale_x_discrete(limits=c("EucFACE",
    #                              "Boreal Forest",
    #                              "Temperate Broadleaf Forest",
    #                              "Temperate Coniferous Forest",
    #                              "Tropical/Subtropical Forest"),
    #                     label=c("EucFACE",
    #                             "Boreal Forest",
    #                             "Temperate Broadleaf Forest",
    #                             "Temperate Coniferous Forest",
    #                             "Tropical/Subtropical Forest"))+
    #    coord_flip()+
    #    xlab("")+
    #    ylab("Microbial P concentration (%)")
    

    #grid.labs <- c("(a)")#, "(b)")
    
    #top_row <- plot_grid(p2, p3)
    
    pdf(paste0("plots_tables/output/unnormalized/microbial_P_global_comparison.pdf"), 
        width=6, height=4)
    ggdraw(p2) #+ 
        #draw_plot(p3, .45, .47, .5, .5) 
    #plot_grid(p3, p2, rel_widths=c(1,1))
    #grid.text(grid.labs,x = c(0.08, 0.92), y = c(0.9, 0.9),
    #          gp=gpar(fontsize=16, col="black", fontface="bold"))
    dev.off()
    
    
}
