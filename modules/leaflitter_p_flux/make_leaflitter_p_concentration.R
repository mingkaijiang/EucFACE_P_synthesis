make_leaflitter_p_concentration <- function() {
    
    infile <- "download/FACE_P0020_RA_leafP-Eter_20130201-20151115_L1.csv"
    
    if(!file.exists(infile)) {
        download_leaflitter()
    }
    
    df <- read.csv("download/FACE_P0020_RA_leafP-Eter_20130201-20151115_L1.csv")
    
    ### setting up the date
    df$Date <- paste0("1-", as.character(df$Campaign))
    df$Date <- as.Date(df$Date, "%d-%b-%y")
    df.litter <- subset(df, Type == "Leaf litter")
    df.dead <- subset(df, Type == "sceneced leaf")
    myDF <- rbind(df.litter, df.dead)
    
    ### Leaf litter p, average across rings and date, unit = %
    df.litter.p <- summaryBy(PercP~Ring+Date,
                             data=myDF,FUN=mean,keep.names=T,na.rm=T)
    df.litter.p$month <- month(df.litter.p$Date)
    df.litter.p$year <- year(df.litter.p$Date)

    return(df.litter.p[,1:3])
}