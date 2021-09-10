calculate_understorey_P_retranslocation_flux <- function (tflux,
                                                          lflux,
                                                          retransDF) {
    
    ### merge the two
    myDF <- merge(tflux, lflux, by=c("Date", "Ring"))
    
    myDF$understorey_p_retrans_flux <- with(myDF, understorey_p_flux - understorey_litter_p_flux)
    myDF$understorey_p_retrans_coef <- with(myDF, understorey_p_retrans_flux/understorey_p_flux)
    
    subDF <- summaryBy(understorey_p_retrans_coef~Ring, FUN=mean,
                       data=myDF, na.rm=T, keep.names=T)
    
    subDF$Method <- "flux"
    
    ### process retransDF
    subDF2 <- retransDF[,c("Ring", "understorey")]
    colnames(subDF2) <- c("Ring", "understorey_p_retrans_coef")
    subDF2$Method <- "conc"
    
    
    ### make plot
    plotDF <- rbind(subDF, subDF2)
    p1 <- ggplot(plotDF, aes(x=Ring, y=understorey_p_retrans_coef, group=Method)) +
        geom_bar(aes(fill=Method), position="dodge", stat="identity")
    
    pdf("plots_tables/checks/understorey_P_retranslocation_comparison.pdf")
    plot(p1)
    dev.off()
    
    
    ### prepare retranslocation flux
    outDF <- myDF[,c("Date", "Ring", "Start_date.x", "End_date.x",
                     "understorey_p_retrans_flux", "Days.x")]
    
    colnames(outDF) <- c("Date", "Ring", "Start_date", "End_date",
                         "understorey_p_retrans_flux", "Days")
    
    return(outDF)
    
}