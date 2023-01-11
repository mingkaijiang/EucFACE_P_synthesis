make_china_p_budget <- function(soil_p_concentration) {
    
    ### Unit: all soil P (except last one) in unit of g P kg-1 soil
    ###       Their unit must not be right. I think the units are in t ha-1,
    ###       because otherwise soil P in 50 - 100 cm depth shouldn't be that high.
    
    
    ### read
    inDF <- read.csv("temp_files/P_data_China.csv")
    
    names(inDF)[names(inDF)=="Longtitude"] <- "Longitude"
    
    ### as numeric
    inDF$litterP_t_ha <- gsub("-", "", inDF$litterP_t_ha)
    inDF$litterP_t_ha <- as.numeric(inDF$litterP_t_ha)
    
    ### unit conversion
    inDF$plantP_g_m2 <- inDF$plantP_t_ha * 100.0
    inDF$litterP_g_m2 <- inDF$litterP_t_ha * 100.0
    inDF$soilP_g_m2 <- inDF$soilP_t_ha * 100.0 * 0.6
    inDF$totalP_g_m2 <- with(inDF, (plantP_g_m2+litterP_g_m2+soilP_g_m2))
    inDF$frac <- with(inDF, plantP_g_m2/soilP_g_m2)
    
    ### ignore data with NAs
    #inDF <- inDF[!is.na(inDF$totalP_g_m2),]
    
    
    sumDF <- summaryBy(plantP_g_m2+litterP_g_m2+soilP_g_m2+totalP_g_m2~Biome+Leaf_Form, data=inDF,
                      FUN=c(mean,sd), na.rm=T, keep.names=T)
    
    
    ### calculate soil p concentration
    concDF <- summaryBy(soilP_0_10cm+soilP_10_20cm+soilP_20_30cm+soilP_30_50cm+soilP_50_100cm~Biome+Leaf_Form,
                        FUN=c(mean,sd), na.rm=T, data=inDF, keep.names=T)
    
    pconDF <- summaryBy(PercP~Depth, FUN=c(mean,sd), data=soil_p_concentration[soil_p_concentration$Ring%in%c(2,3,6),],
                        na.rm=T, keep.names=T)
    
    ### prepare density plot
    d1 <- density(inDF$totalP_g_m2,na.rm=T)
    d2 <- density(inDF$plantP_g_m2,na.rm=T)
    d3 <- density(inDF$litterP_g_m2,na.rm=T)
    d4 <- density(inDF$soilP_g_m2,na.rm=T)
    
    ### create data frame
    xd1 <- data.frame(d1[c("x", "y")])
    xd2 <- data.frame(d2[c("x", "y")])
    xd3 <- data.frame(d3[c("x", "y")])
    xd4 <- data.frame(d4[c("x", "y")])
    
    ### find probability distribution marks
    probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
    quantiles1 <- quantile(inDF$totalP_g_m2, prob=probs,na.rm=T)
    quantiles2 <- quantile(inDF$plantP_g_m2, prob=probs,na.rm=T)
    quantiles3 <- quantile(inDF$litterP_g_m2, prob=probs,na.rm=T)
    quantiles4 <- quantile(inDF$soilP_g_m2, prob=probs,na.rm=T)
    
    
    xd1$quant <- factor(findInterval(xd1$x,quantiles1))
    xd2$quant <- factor(findInterval(xd2$x,quantiles2))
    xd3$quant <- factor(findInterval(xd3$x,quantiles3))
    xd4$quant <- factor(findInterval(xd4$x,quantiles4))

    spectral.colors <- brewer.pal(6,"Spectral")
    
    
    
    ### plot
    p1 <- ggplot(xd1, aes(x,y)) + 
        geom_line() + 
        geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + 
        scale_x_continuous(breaks=c(0, 100, 500, 1000),
                           labels=c(0, 100, 500, ">1000"),
                           limits = c(0,1000),expand=c(0,0)) + 
        scale_fill_manual(name="",
                          limits=c(0:5),
                          labels=c("0-5th", "5-25th", "25-50th", "50-75th", "75-95th", "95-100th"),
                          values=spectral.colors,
                          guide=guide_legend(nrow=6))+
        geom_vline(xintercept=37.7, col="black", size = 1.0)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=10),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.text.align = 0,
              legend.position="right",
              legend.box = 'vertical',
              legend.box.just = 'left')+
        guides(color = guide_legend(nrow=5, byrow = T))+
        ylab("Density")+
        xlab(expression("Total P pool (g P " * m^-2* ")"))
    
    
    
    p2 <- ggplot(xd2, aes(x,y)) + 
        geom_line() + 
        geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + 
        scale_x_continuous(breaks=c(0, 1, 5, 50),
                           labels=c(0, 1, 5, ">50"),
                           limits = c(0,50),expand=c(0,0)) + 
        scale_fill_manual(name="",
                          limits=c(0:5),
                          labels=c("0-5th", "5-25th", "25-50th", "50-75th", "75-95th", "95-100th"),
                          values=spectral.colors,
                          guide=guide_legend(nrow=6))+
        geom_vline(xintercept=1.7, col="black", size = 1.0)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=10),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.text.align = 0,
              legend.position="right",
              legend.box = 'vertical',
              legend.box.just = 'left')+
        guides(color = guide_legend(nrow=5, byrow = T))+
        ylab("Density")+
        xlab(expression("Plant P pool (g P " * m^-2* ")"))
    
    
    
    p3 <- ggplot(xd3, aes(x,y)) + 
        geom_line() + 
        geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + 
        scale_x_continuous(breaks=c(0, 0.5, 1, 2),
                           labels=c(0, 0.5, 1,">2"),
                           limits = c(0,2),expand=c(0,0)) + 
        scale_fill_manual(name="",
                          limits=c(0:5),
                          labels=c("0-5th", "5-25th", "25-50th", "50-75th", "75-95th", "95-100th"),
                          values=spectral.colors,
                          guide=guide_legend(nrow=6))+
        geom_vline(xintercept=0.09, col="black", size = 1.0)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=10),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.text.align = 0,
              legend.position="right",
              legend.box = 'vertical',
              legend.box.just = 'left')+
        guides(color = guide_legend(nrow=5, byrow = T))+
        ylab("Density")+
        xlab(expression("Litter P pool (g P " * m^-2* ")"))
    
    
    
    p4 <- ggplot(xd4, aes(x,y)) + 
        geom_line() + 
        geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + 
        scale_x_continuous(breaks=c(0, 100, 500, 1000),
                           labels=c(0, 100, 500, ">1000"),
                           limits = c(0,1000),expand=c(0,0)) + 
        scale_fill_manual(name="",
                          limits=c(0:5),
                          labels=c("0-5th", "5-25th", "25-50th", "50-75th", "75-95th", "95-100th"),
                          values=spectral.colors,
                          guide=guide_legend(nrow=6))+
        geom_vline(xintercept=35.94, col="black", size = 1.0)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=10),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.text.align = 0,
              legend.position="right",
              legend.box = 'vertical',
              legend.box.just = 'left')+
        guides(color = guide_legend(nrow=5, byrow = T))+
        ylab("Density")+
        xlab(expression("Soil P pool (g P " * m^-2* ")"))
    
    
    
    ### plot boxplot, log scale
    p1 <- ggplot(inDF) +
        #geom_violin(aes(x=Biome, y=log(soilP_g_m2), fill=Leaf_Form), alpha=0.4)+
        geom_jitter(aes(x=Biome, y=log(soilP_g_m2), fill=Leaf_Form), pch=21)+
        geom_boxplot(aes(x=Biome, y=log(soilP_g_m2), fill=Leaf_Form), alpha=0.4,outlier.alpha = 0)+
        geom_hline(yintercept=log(37.7), col="red", size = 2)+
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
              legend.box.just = 'left')+
        scale_y_continuous(name=expression("Soil P pool (g P " * m^-2 * ")"), 
                           breaks=c(log(1), log(10), log(100), log(1000)), 
                           labels=c(1, 10, 100, 1000), 
                           limits=c(log(10), 8))+
        scale_fill_manual(name="",
                          labels=c("needleleaf"="Needleleaf", 
                                   "broadleaf"="Broadleaf"),
                          values=c("green", "brown"))#+
        #scale_x_discrete(name="Forest biome",
        #                 breaks=c("1_tropic","2_temperate","3_boreal","4_Med"),
        #                 labels=c("Tropics & subtropics", "Temperate", "Boreal",
        #                          "Mediterranean"))+
        #annotate("text", x=1, y = 8, label = paste0("n = ", nDF[1]))+
        #annotate("text", x=2, y = 8, label = paste0("n = ", nDF[2]))+
        #annotate("text", x=3, y = 8, label = paste0("n = ", nDF[3]))+
        #annotate("text", x=4, y = 8, label = paste0("n = ", nDF[4]))
    
    #plot(p1)
    
    
    
    p2 <- ggplot(inDF) +
        geom_jitter(aes(x=Biome, y=log(plantP_g_m2), fill=Leaf_Form), pch=21, alpha=0.4)+
        geom_boxplot(aes(x=Biome, y=log(plantP_g_m2), fill=Leaf_Form), outlier.alpha = 0)+
        geom_hline(yintercept=log(1.7), col="red", size = 2)+
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
              legend.box.just = 'left')+
        scale_y_continuous(name=expression("Plant P pool (g P " * m^-2 * ")"), 
                           breaks=c(log(0.1), log(1), log(10), log(100)), 
                           labels=c(0.1, 1, 10, 100), 
                           limits=c(-5, 8))+
        scale_fill_manual(name="",
                          labels=c("needleleaf"="Needleleaf", 
                                   "broadleaf"="Broadleaf"),
                          values=c("green", "brown"))
    
    
    
    p3 <- ggplot(inDF) +
        geom_jitter(aes(x=Biome, y=log(soilP_0_10cm), fill=Leaf_Form), pch=21, alpha=0.4)+
        geom_boxplot(aes(x=Biome, y=log(soilP_0_10cm), fill=Leaf_Form), outlier.alpha = 0)+
        geom_hline(yintercept=log(pconDF$PercP.mean[pconDF$Depth=="0_10"]*10), col="red", size = 2)+
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
              legend.box.just = 'left')+
        scale_y_continuous(name=expression("Soil P concentration (g P " * kg^-1 * ")"), 
                           breaks=c(log(0.01), log(0.1), log(1), log(10)), 
                           labels=c(0.01, 0.1, 1, 10), 
                           limits=c(-5, 3))+
        scale_fill_manual(name="",
                          labels=c("needleleaf"="Needleleaf", 
                                   "broadleaf"="Broadleaf"),
                          values=c("green", "brown"))
    
    
    p4 <- ggplot(inDF) +
        geom_jitter(aes(x=Biome, y=log(frac), fill=Leaf_Form), pch=21, alpha=0.4)+
        geom_boxplot(aes(x=Biome, y=log(frac), fill=Leaf_Form), outlier.alpha = 0)+
        geom_hline(yintercept=log(round(1.7/35.94,2)), col="red", size = 2)+
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
              legend.box.just = 'left')+
        scale_y_continuous(name=expression("Plant P / Soil P"), 
                           breaks=c(log(0.01), log(0.05), log(0.1), log(0.5), log(1)), 
                           labels=c(0.01, 0.05, 0.1, 0.5, 1), 
                           limits=c(-5, 1))+
        scale_fill_manual(name="",
                          labels=c("needleleaf"="Needleleaf", 
                                   "broadleaf"="Broadleaf"),
                          values=c("green", "brown"))
    
    
    #plot(p4)
    
    
    
    
    lit.legend <- get_legend(p2 + theme(legend.position="right",
                                        legend.box = 'horizontal',
                                        legend.box.just = 'left'))
    
    pdf(paste0("plots_tables/output/unnormalized/comparison_China_P.pdf"),
        width=10,height=4)
    toprow <- plot_grid(p2, p4, labels="", ncol=2, align="v", axis = "l")
    plot_grid(toprow, lit.legend, ncol=1, rel_heights=c(1, 0.2))
    grid.text(c("(a)", "(b)"), x = c(0.45, 0.95),
              y = c(0.9, 0.9), 
              gp=gpar(fontsize=14, col="black", fontface="bold"))
    dev.off()
    
    
    
    

}
