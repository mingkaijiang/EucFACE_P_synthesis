compare_soil_p_mineralization_fluxes <- function (flux1, 
                                                  flux2, 
                                                  flux3,
                                                  flux4,
                                                  name1,
                                                  name2,
                                                  name3,
                                                  name4) {
    
    
    pDF1 <- summaryBy(p_mineralization_mg_m2_d~Date+Ring, 
                      FUN=sum, data=flux1, keep.names=T, na.rm=T)
    
    pDF2 <- summaryBy(p_mineralization_mg_m2_d~Date+Ring, 
                      FUN=sum, data=flux2, keep.names=T, na.rm=T)

    pDF3 <- summaryBy(p_mineralization_mg_m2_d~Date+Ring, 
                      FUN=sum, data=flux3, keep.names=T, na.rm=T)
    
    pDF4 <- summaryBy(p_mineralization_mg_m2_d~Date+Ring, 
                      FUN=sum, data=flux4, keep.names=T, na.rm=T)
    
    ### calculate averages for each ring
    sumDF1 <- summaryBy(p_mineralization_mg_m2_d~Ring, FUN=c(mean,sd),
                        data=pDF1, keep.names=T, na.rm=T)
    
    sumDF2 <- summaryBy(p_mineralization_mg_m2_d~Ring, FUN=c(mean,sd),
                        data=pDF2, keep.names=T, na.rm=T)
    
    sumDF3 <- summaryBy(p_mineralization_mg_m2_d~Ring, FUN=c(mean,sd),
                        data=pDF3, keep.names=T, na.rm=T)
    
    sumDF4 <- summaryBy(p_mineralization_mg_m2_d~Ring, FUN=c(mean,sd),
                        data=pDF4, keep.names=T, na.rm=T)
    
    ### assign name
    sumDF1$Name <- name1
    sumDF2$Name <- name2
    sumDF3$Name <- name3
    sumDF4$Name <- name4
    
    plotDF <- rbind(sumDF1, rbind(sumDF2, rbind(sumDF3, sumDF4)))
    
    
    
    p1 <- ggplot(plotDF, aes(Ring, p_mineralization_mg_m2_d.mean, group=Name)) + 
        geom_point(aes(col=Name, fill=Name)) +
        xlab("Ring") + ylab(expression(P[min] * " (mg m-2 d-1)"))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="bottom") ; p1
    
    pdf("plots_tables/checks/soil_P_mineralization_flux_method_comparison.pdf")
    plot(p1)
    dev.off()
    
    
}
