#- Make the understorey P retranslocation coefficient
make_understorey_p_retranslocation_coefficient <- function(df1, df2){
    
    
    df1sum <- summaryBy(PercP~Ring, FUN=mean, data=df1, na.rm=T)
    df2sum <- summaryBy(PercP~Ring, FUN=mean, data=df2, na.rm=T)
    
    df1sum$old <- df2sum$PercP.mean

    return(outDF)
    
}
