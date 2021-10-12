compare_fineroot_c_pool <- function (inDF1, 
                                     inDF2,
                                     inDF3,
                                     inDF4,
                                     name1,
                                     name2,
                                     name3,
                                     name4) {
    
    ### add colnames
    inDF1$Name <- name1
    inDF2$Name <- name2
    inDF3$Name <- name3
    inDF4$Name <- name4
    
    ### merge all data
    myDF <- rbind(inDF1, rbind(inDF2, rbind(inDF3, inDF4)))
    
    myDF$Trt <- "aCO2"
    myDF$Trt[myDF$Ring%in%c("1","4","5")] <- "eCO2"
    
    plotDF <- summaryBy(fineroot_pool+fineroot_0_10_cm+fineroot_10_30_cm+fineroot_30_60_cm~Trt+Name, FUN=c(mean,sd),
                        data=myDF, keep.names=T, na.rm=T)
    
    
    p1 <- ggplot(plotDF, 
                 aes(Name, fineroot_pool.mean, group=Trt)) + 
        geom_errorbar(aes(x=Name, ymin=fineroot_pool.mean-fineroot_pool.sd,
                          ymax=fineroot_pool.mean+fineroot_pool.sd),
                      position=position_dodge(width=0.5), width=0.3)+
        geom_point(aes(fill=Trt), pch=21, col="black", size=4,
                   position=position_dodge(width=0.5)) +
        xlab("") + ylab(expression(C[froot] * " (g C " * m^-2 * ")"))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="bottom") +
        scale_fill_manual(name="",
                          values=c("aCO2"="blue2", "eCO2"="red3"),
                          label=c("aCO2"=expression(aCO[2]),
                                  "eCO2"=expression(eCO[2])))+
        scale_x_discrete(limit=c("small_root", "rev_bk",
                           "intermediate_root", "large_root"))
    
    
    p2 <- ggplot(plotDF, 
                 aes(Name, fineroot_0_10_cm.mean, group=Trt)) + 
        geom_errorbar(aes(x=Name, ymin=fineroot_0_10_cm.mean-fineroot_0_10_cm.sd,
                          ymax=fineroot_0_10_cm.mean+fineroot_0_10_cm.sd),
                      position=position_dodge(width=0.5), width=0.3)+
        geom_point(aes(fill=Trt), pch=21, col="black", size=4,
                   position=position_dodge(width=0.5)) +
        xlab("") + ylab(expression(C[froot] * " 0-10cm (g C " * m^-2 * ")"))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="bottom") +
        scale_fill_manual(name="",
                          values=c("aCO2"="blue2", "eCO2"="red3"),
                          label=c("aCO2"=expression(aCO[2]),
                                  "eCO2"=expression(eCO[2])))+
        scale_x_discrete(limit=c("small_root", "rev_bk",
                                 "intermediate_root", "large_root")); p2
    
    
    p3 <- ggplot(plotDF, 
                 aes(Name, fineroot_10_30_cm.mean, group=Trt)) + 
        geom_errorbar(aes(x=Name, ymin=fineroot_10_30_cm.mean-fineroot_10_30_cm.sd,
                          ymax=fineroot_10_30_cm.mean+fineroot_10_30_cm.sd),
                      position=position_dodge(width=0.5), width=0.3)+
        geom_point(aes(fill=Trt), pch=21, col="black", size=4,
                   position=position_dodge(width=0.5)) +
        xlab("") + ylab(expression(C[froot] * " 10-30cm (g C " * m^-2 * ")"))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="bottom") +
        scale_fill_manual(name="",
                          values=c("aCO2"="blue2", "eCO2"="red3"),
                          label=c("aCO2"=expression(aCO[2]),
                                  "eCO2"=expression(eCO[2])))+
        scale_x_discrete(limit=c("small_root", "rev_bk",
                                 "intermediate_root", "large_root")); p3
    
    
    p4 <- ggplot(plotDF, 
                 aes(Name, fineroot_30_60_cm.mean, group=Trt)) + 
        geom_errorbar(aes(x=Name, ymin=fineroot_30_60_cm.mean-fineroot_30_60_cm.sd,
                          ymax=fineroot_30_60_cm.mean+fineroot_30_60_cm.sd),
                      position=position_dodge(width=0.5), width=0.3)+
        geom_point(aes(fill=Trt), pch=21, col="black", size=4,
                   position=position_dodge(width=0.5)) +
        xlab("") + ylab(expression(C[froot] * " 30-60cm (g C " * m^-2 * ")"))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="bottom") +
        scale_fill_manual(name="",
                          values=c("aCO2"="blue2", "eCO2"="red3"),
                          label=c("aCO2"=expression(aCO[2]),
                                  "eCO2"=expression(eCO[2])))+
        scale_x_discrete(limit=c("small_root", "rev_bk",
                                 "intermediate_root", "large_root")); p4
    
    
    pdf("plots_tables/checks/fineroot_pool_method_comparison.pdf")
    plot(p1)
    plot(p2)
    plot(p3)
    plot(p4)
    dev.off()
    
    
    ### check depth profile
    
    
}
