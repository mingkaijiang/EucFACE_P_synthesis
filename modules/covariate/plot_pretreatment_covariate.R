plot_pretreatment_covariate <- function (corDF) {
    
    ### obtain variable names
    l <- dim(corDF)[2]
    var.list <- colnames(corDF)[3:l]
    
    ### calculate means for all variables
    mean.values <- colMeans(corDF[3:l])
    
    ### normalize all values by the means
    tmpDF <- corDF
    for (i in c(3:l)) {
        tmpDF[i] <- tmpDF[i] / mean.values[i-2]
    }
    
    ### summaryby
    sumDF1 <- summaryBy(.~Trt, data=tmpDF, keep.names=T,
                        na.rm=T, FUN=mean)
    
    sumDF1$Ring <- NULL
    
    sumDF2 <- summaryBy(.~Trt, data=tmpDF, keep.names=T,
                        na.rm=T, FUN=sd)
    
    sumDF2$Ring <- NULL
    
    ### convert into long format
    plotDF1 <- reshape2::melt(sumDF1, id.vars="Trt")
    plotDF2 <- reshape2::melt(sumDF2, id.vars="Trt")
    names(plotDF2)[names(plotDF2)=="value"] <- "sdvalue"
    
    plotDF <- merge(plotDF1, plotDF2, by=c("Trt", "variable"))
    
    ### plot the normalized responses
    p1 <- ggplot(plotDF, aes(variable, value, group=Trt))+
        geom_abline(slope=0, intercept=1)+
        geom_errorbar(aes(x=variable, ymin=value-sdvalue,
                          ymax=value+sdvalue),
                      position=position_dodge2(), width=0.3)+
        geom_point(stat = "identity", 
                   aes(fill=Trt), size=4, col="black", pch=21,
                   position=position_dodge2(width=0.3)) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        coord_flip()+
        scale_fill_manual(name="",
                          values=c("amb"="blue3",
                                   "ele"="red2"))
    
    pdf(paste0("plots_tables/covariate/pretreatment_covariate_comparison.pdf"), 
        width=8, height=5)
    plot(p1)
    dev.off()
    
}