make_wood_p_pool <- function() {
    ### download the data
    download_canopy_p_data()
    
    df <- read.csv("download/FACE_P0020_RA_leafP-Eter_20130201-20151115_L1.csv")
    
    ### setting up the date
    df$Date <- paste0("1-", as.character(df$Campaign))
    df$Date <- as.Date(df$Date, "%d-%b-%y")
    
    df.wood <- subset(df, Type == "wood")
    
    ### Wood p, average across rings and date, unit = %
    df.wood.p <- summaryBy(PercP~Ring+Date,
                           data=df.wood,FUN=mean,keep.names=T,na.rm=T)
    df.wood.p$month <- month(df.wood.p$Date)
    df.wood.p$year <- year(df.wood.p$Date)
    
    ### Wood c, average across rings and date, unit = %
    df.wood.c <- summaryBy(PercC~Ring+Date,
                           data=df.wood,FUN=mean,keep.names=T,na.rm=T)
    df.wood.c$month <- month(df.wood.c$Date)
    df.wood.c$year <- year(df.wood.c$Date)
    
    
}