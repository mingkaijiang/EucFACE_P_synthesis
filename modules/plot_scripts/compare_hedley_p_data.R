compare_hedley_p_data <- function() {
    
    yangDF <- read.csv("temp_files/HedleyP_database.csv")
    
    ### calculate exchangeable Pi fraction 
    yangDF$ExchangeablePi <- with(yangDF, (Resin.Pi))
    
    yangDF$ExchangeablePiFrac <- with(yangDF, ExchangeablePi/Total.P*100)

    ### calculate exchangeable Pi fraction
    myDF <- read.csv("temp_files/Hedley_P_2013_Summary_12-2-19.csv")

    myDF$ExchangeablePiFrac <- with(myDF, F1_2_Pi_Exhanagable/Total_Aqua_Regia_P*100)

    v1 <- summaryBy(F1_2_Pi_Exhanagable+Total_Aqua_Regia_P+ExchangeablePiFrac~Treatment,
                    FUN=c(mean,sd), data=myDF, keep.names=T, na.rm=T)
    
    
    
    p3 <- ggplot(yangDF) +
        geom_jitter(aes(x=Soil.Order, y=ExchangeablePiFrac, fill=Soil.Order), pch=21, alpha=0.4)+
        geom_boxplot(aes(x=Soil.Order, y=ExchangeablePiFrac, fill=Soil.Order), outlier.alpha = 0)+
        geom_hline(yintercept=v1$ExchangeablePiFrac.mean[v1$Treatment=="Amb"], col="red", size = 2)+
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
              legend.box.just = 'left')#+
        #scale_y_continuous(name=expression("Soil P concentration (g P " * kg^-1 * ")"), 
        #                   breaks=c(log(0.01), log(0.1), log(1), log(10)), 
        #                   labels=c(0.01, 0.1, 1, 10), 
        #                   limits=c(-5, 3))+
        #scale_fill_manual(name="Leaf form",
        #                  labels=c("needleleaf"="Needleleaf", 
        #                           "broadleaf"="Broadleaf"),
        #                  values=c("green", "brown"))
    
    plot(p3)
    
    
    
}