make_leaflitter_p_concentration <- function() {
    
    ### new file code
    infile <- "download/FACE_P0020_RA_NPsenesced_2013-2018-L2.csv"
    
    if(!file.exists(infile)) {
        download_leaflitter()
    }
    
    df <- read.csv("download/FACE_P0020_RA_NPsenesced_2013-2018-L2.csv")
    
    df$Date <- paste0("1-", as.character(df$Campaign))
    df$Date <- as.Date(df$Date, "%d-%b-%y")
    
    ### lit leaf p, average across rings and date, unit = %
    df.lit.p <- summaryBy(Perc.P~Ring+Date,
                            data=df,FUN=mean,keep.names=T,na.rm=T)
    df.lit.p$month <- month(df.lit.p$Date)
    df.lit.p$year <- year(df.lit.p$Date)
    
    colnames(df.lit.p) <- c("Ring", "Date", "PercP", "month", "year")
    

    return(df.lit.p[,1:3])
}