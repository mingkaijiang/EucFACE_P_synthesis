#- Make the canopy P pool 
make_canopy_p_pool <- function(){
    ### return ring-specific canopy P data (mg/kg)

    ### download the data
    download_canopy_p_data()
    
    df <- read.csv("download/FACE_P0020_RA_leafP-Eter_20130201-20151115_L1.csv")
    
    ### setting up the date
    df$Date <- paste0("1-", as.character(df$Campaign))
    df$Date <- as.Date(df$Date, "%d-%b-%y")

    ### only include green leaf
    df.green <- subset(df, Type == "green leaf")
    df.yellow <- subset(df, Type == "sceneced leaf")

    ### green leaf p, average across rings and date, unit = %
    df.green.p <- summaryBy(PercP~Ring+Date,
                      data=df.green,FUN=mean,keep.names=T,na.rm=T)
    df.green.p$month <- month(df.green.p$Date)
    df.green.p$year <- year(df.green.p$Date)

    ### sceneced p, average across rings and date, unit = %
    df.yellow.p <- summaryBy(PercP~Ring+Date,
                            data=df.yellow,FUN=mean,keep.names=T,na.rm=T)
    df.yellow.p$month <- month(df.yellow.p$Date)
    df.yellow.p$year <- year(df.yellow.p$Date)
    
    ### green leaf c, average across rings and date, unit = %
    df.green.c <- summaryBy(PercC~Ring+Date,
                            data=df.green,FUN=mean,keep.names=T,na.rm=T)
    df.green.c$month <- month(df.green.c$Date)
    df.green.c$year <- year(df.green.c$Date)
    
    ### sceneced c, average across rings and date, unit = %
    df.yellow.c <- summaryBy(PercC~Ring+Date,
                             data=df.yellow,FUN=mean,keep.names=T,na.rm=T)
    df.yellow.c$month <- month(df.yellow.c$Date)
    df.yellow.c$year <- year(df.yellow.c$Date)

    ### make lai, sla and leaf c pool 
    lai_variable <- make_lai_variable()
    sla_variable <- make_sla_variable()
    leaf_c_pool <- make_leaf_pool(lai_variable, sla_variable)
    leaf_c_pool$month <- month(leaf_c_pool$Date)
    leaf_c_pool$year <- year(leaf_c_pool$Date)
    
    ### find the common month and year for green leaf
    greenDF <- assign_percent_to_pool(df.green.c, df.green.p, leaf_c_pool,
                                      "green.c", "green.p")

    ### complete cases for dead leaf df
    deadDF <- assign_percent_to_pool(df.yellow.c, df.yellow.p, leaf_c_pool,
                                      "dead.c", "dead.p")    
        
    ### calculate leaf P pool g P m-2
    greenDF$leaf_p_pool <- greenDF$leaf_pool/greenDF$green.c*greenDF$green.p
    deadDF$leaf_p_pool <- deadDF$leaf_pool/deadDF$dead.c*deadDF$dead.p
    
    return(list(greenleaf = data.table(greenDF), 
                deadleaf = data.table(deadDF)))

}


