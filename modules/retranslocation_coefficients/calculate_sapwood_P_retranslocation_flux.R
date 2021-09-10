calculate_sapwood_P_retranslocation_flux <- function (tflux,
                                                      retransDF) {
    
    retrans <- retransDF[,c("Ring", "sapwood")]
    
    ### merge the two
    myDF <- merge(tflux, retrans, by="Ring")
    
    myDF$sapwood_p_retrans_flux <- with(myDF, wood_p_flux * sapwood)

    
    ### prepare retranslocation flux
    outDF <- myDF[,c("Date", "Ring", "Start_date", "End_date",
                     "sapwood_p_retrans_flux", "Days")]
    
    return(outDF)
    
}