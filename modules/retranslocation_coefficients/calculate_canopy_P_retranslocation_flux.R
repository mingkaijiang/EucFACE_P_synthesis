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
    colnames(subDF2) <- c("Ring", "canopy_p_retrans_coef")
    subDF2$Method <- "conc"
    
    
    ### make plot
    plotDF <- rbind(subDF, subDF2)
    p1 <- ggplot(plotDF, aes(x=Ring, y=canopy_p_retrans_coef, group=Method)) +
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