#- Make the microbial P concentration
make_microbial_p_concentration <- function(){
    # return ring-specific, continuous microbial P concentration

    # download the data
    download_microbial_p_data()
    
    df <- read.csv(file.path(getToPath(), 
                             "FACE_P0014_RA_MicrobialBiomassCNP_L1_20120613-20151130.csv"))

    df$date <- as.Date(df$date, format="%m/%d/%Y")
        
        
    # averaged across rings and depths, unit: mg/kg
    df.m <- summaryBy(Pmic~ring+date,
                      data=df,FUN=mean,keep.names=T,na.rm=T)
    
    df.m$PercP <- df.m$Pmic * 10^-4
    
    df.m <- df.m[complete.cases(df.m),]
    
    df.out <- df.m[,c("date", "ring", "PercP")]
    colnames(df.out) <- c("Date", "Ring", "PercP")
    
    return(df.out)
    
}
