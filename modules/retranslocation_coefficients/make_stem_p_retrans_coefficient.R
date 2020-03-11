make_stem_p_retrans_coefficient <- function(sapwood) {
    
    
    df <- summaryBy(PercP~Ring, FUN=mean, data=sapwood, na.rm=T)

    df$old <- 0.004
    
    df$retrans <- with(df, PercP.mean-old)
    
    df$retrans_coef <- round(with(df, retrans/PercP.mean), 2)
    
    outDF <- df[,c("Ring", "retrans_coef")]
    
    return(outDF)
    
}