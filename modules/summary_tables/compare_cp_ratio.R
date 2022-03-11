compare_cp_ratio <- function(myDF1, myDF2) {
    
    
    ### calculate original CP ratio under aCO2
    ### calculate original CP ratio under eCO2
    ### calculate new CP ratio under aCO2, this is the 10% scenario
    
    
    ### reshape
    reDF1 <- melt(setDT(myDF1), id.vars = c("Ring"), variable.name = "Variable")
    reDF2 <- melt(setDT(myDF2), id.vars = c("Ring"), variable.name = "Variable")
    
    ### add treatment
    reDF1$Trt <- "amb"
    reDF1$Trt[reDF1$Ring%in%c(1,4,5)] <- "ele"
    
    reDF2 <- reDF2[reDF2$Ring%in%c(2,3,6),]
    reDF2$Trt <- "scenario"
    
    myDF <- rbind(reDF1, reDF2)
    myDF2 <- summaryBy(value~Variable+Trt, FUN=c(mean,sd),
                       data=myDF, na.rm=T, keep.names=T)
    
    ### ignore soil, litter
    plotDF <- myDF2[myDF2$Variable%in%c("canopy", "wood", "sapwood",
                                        "heartwood", "fineroot", "understorey")]
    
    
    ###plot the three, check statistical significance
    p1 <- ggplot(plotDF,
                 aes(Variable, value.mean)) + 
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge")+
        geom_errorbar(aes(ymax=value.mean+value.sd, ymin=value.mean-value.sd, 
                          color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        xlab("")+ 
        ylab("CP ratios")+
        coord_flip()+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="right")+
        scale_fill_manual(name="Treatment", 
                          values = c("amb" = "blue3", 
                                     "ele" = "red2", 
                                     "scenario" = cbPalette[5]),
                          labels=c(expression(aCO[2]), 
                                   expression(eCO[2]),
                                   "10%+ scenario"))+
        scale_color_manual(name="Treatment", 
                          values = c("amb" = "black",  
                                     "ele" = "black", 
                                     "scenario" = "black"),
                          labels=c(expression(aCO[2]), 
                                   expression(eCO[2]),
                                   "10%+ scenario"))
        
    pdf(paste0("plots_tables/output/unnormalized/CP_ratio_sensitivity_test.pdf"),
        width=6,height=6)
    plot(p1)
    dev.off()
    
}
