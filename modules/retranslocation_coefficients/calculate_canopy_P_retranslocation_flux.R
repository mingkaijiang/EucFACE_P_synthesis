calculate_canopy_P_retranslocation_flux <- function (tflux,
                                                     lflux,
                                                     retransDF) {
    
    ### merge the two
    myDF <- merge(tflux, lflux, by=c("Date", "Ring"))
    
    myDF$canopy_p_retrans_flux <- with(myDF, leaf_p_flux - leaflitter_p_flux_mg_m2_d)
    myDF$canopy_p_retrans_coef <- with(myDF, canopy_p_retrans_flux/leaf_p_flux)
    
    subDF <- summaryBy(canopy_p_retrans_coef~Ring, FUN=mean,
                       data=myDF, na.rm=T, keep.names=T)
    
    subDF$Method <- "flux"
    
    ### process retransDF
    subDF2 <- retransDF[,c("Ring", "canopy")]
    
    myDF2 <- merge(myDF, subDF2, by=c("Ring"))
    myDF2$canopy_p_retrans_flux2 <- with(myDF2, leaflitter_p_flux_mg_m2_d/canopy)
    
    
    colnames(subDF2) <- c("Ring", "canopy_p_retrans_coef")
    subDF2$Method <- "conc"
    
    
    ### make plot
    plotDF <- rbind(subDF, subDF2)
    plotDF$Trt <- "aCO2"
    plotDF$Trt[plotDF$Ring%in%c(1,4,5)] <- "eCO2"
    plotDF2 <- summaryBy(canopy_p_retrans_coef~Method+Trt, data=plotDF, na.rm=T, FUN=c(mean,sd),
                         keep.names=T)
    
    p1 <- ggplot(plotDF2, aes(x=Trt, y=canopy_p_retrans_coef.mean, group=Method)) +
        geom_bar(aes(fill=Method), position="dodge", stat="identity")
    
    pdf("plots_tables/checks/canopy_P_retranslocation_comparison.pdf")
    plot(p1)
    dev.off()

    
    
    ### prepare retranslocation flux
    outDF <- myDF[,c("Date", "Ring", "Start_date.x", "End_date.x",
                     "canopy_p_retrans_flux", "Days.x")]
    
    colnames(outDF) <- c("Date", "Ring", "Start_date", "End_date",
                         "canopy_p_retrans_flux", "Days")
    
    return(outDF)
    
}