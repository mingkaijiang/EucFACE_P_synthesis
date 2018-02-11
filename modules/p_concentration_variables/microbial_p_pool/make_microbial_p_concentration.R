#- Make the microbial P concentration
make_microbial_p_concentration <- function(func) {
    # return ring-specific, continuous microbial P concentration

    #Pmic,ug g-1,"Fumigation extraction with Bray P AQ2 determination of PO4.
    
    # download the data
    download_microbial_p_data()
    
    df <- read.csv(file.path(getToPath(), 
                             "FACE_P0014_RA_MicrobialBiomassCNP_L1_20120613-20151130.csv"))

    df$Date <- as.character(df$date)
    df$date <- as.Date(df$Date, format="%d/%m/%Y")    
        
    # first, averaged across depths, unit: ug g-1
    df2 <- summaryBy(Pmic~ring+date+plot,
                      data=df,FUN=mean,keep.names=T,na.rm=T)
    
    # now, mean/min/max across rings and date
    df.m <- summaryBy(Pmic~ring+date,
                     data=df2,FUN=func,keep.names=T,na.rm=T)
    
    df.m$PercP <- df.m$Pmic * 10^-4
    
    df.m <- df.m[!is.infinite(df.m$Pmic),] 
    df.m <- df.m[complete.cases(df.m),]
    
    df.out <- df.m[,c("date", "ring", "PercP")]
    colnames(df.out) <- c("Date", "Ring", "PercP")
    
    return(df.out)
    
}
