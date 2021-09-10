calculate_coarseroot_P_retranslocation_flux <- function (tflux,
                                                         retransDF) {
    
    retrans <- retransDF[,c("Ring", "coarseroot")]
    
    ### merge the two
    myDF <- merge(tflux, retrans, by="Ring")
    
    myDF$coarseroot_p_retrans_flux <- with(myDF, coarse_root_p_flux * coarseroot)

    
    ### prepare retranslocation flux
    outDF <- myDF[,c("Date", "Ring", "Start_date", "End_date",
                     "coarseroot_p_retrans_flux", "Days")]
    
    return(outDF)
    
}