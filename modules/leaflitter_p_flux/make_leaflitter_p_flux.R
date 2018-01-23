make_leaflitter_p_flux <- function() {
    
    df <- read.csv("download/FACE_P0020_RA_leafP-Eter_20130201-20151115_L1.csv")
    
    ### setting up the date
    df$Date <- paste0("1-", as.character(df$Campaign))
    df$Date <- as.Date(df$Date, "%d-%b-%y")
    df.litter <- subset(df, Type == "Leaf litter")
    
    ### Leaf litter p, average across rings and date, unit = %
    df.litter.p <- summaryBy(PercP~Ring+Date,
                             data=df.litter,FUN=mean,keep.names=T,na.rm=T)
    df.litter.p$month <- month(df.litter.p$Date)
    df.litter.p$year <- year(df.litter.p$Date)
    
    ### Leaf litter c, average across rings and date, unit = %
    df.litter.c <- summaryBy(PercC~Ring+Date,
                             data=df.litter,FUN=mean,keep.names=T,na.rm=T)
    df.litter.c$month <- month(df.litter.c$Date)
    df.litter.c$year <- year(df.litter.c$Date)
    
    
    ### Leaf litter flux in unit of mg m-2 d-1 of leaf, not C!!!
    litter_flux <- make_leaflitter_flux()
    
    ### assign percentage to litter P production flux
    out <- assign_percent_to_flux(df.litter.p, litter_flux)
    
    return(out)
}