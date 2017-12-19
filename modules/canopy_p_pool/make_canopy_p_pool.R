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
    df.litter <- subset(df, Type == "Leaf litter")
    df.wood <- subset(df, Type == "wood")
    
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
    
    ### Leaf litter p, average across rings and date, unit = %
    df.litter.p <- summaryBy(PercP~Ring+Date,
                            data=df.litter,FUN=mean,keep.names=T,na.rm=T)
    df.litter.p$month <- month(df.litter.p$Date)
    df.litter.p$year <- year(df.litter.p$Date)
    
    ### Wood p, average across rings and date, unit = %
    df.wood.p <- summaryBy(PercP~Ring+Date,
                            data=df.wood,FUN=mean,keep.names=T,na.rm=T)
    df.wood.p$month <- month(df.wood.p$Date)
    df.wood.p$year <- year(df.wood.p$Date)
    
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
    
    ### Leaf litter c, average across rings and date, unit = %
    df.litter.c <- summaryBy(PercC~Ring+Date,
                             data=df.litter,FUN=mean,keep.names=T,na.rm=T)
    df.litter.c$month <- month(df.litter.c$Date)
    df.litter.c$year <- year(df.litter.c$Date)
    
    ### Wood c, average across rings and date, unit = %
    df.wood.c <- summaryBy(PercC~Ring+Date,
                           data=df.wood,FUN=mean,keep.names=T,na.rm=T)
    df.wood.c$month <- month(df.wood.c$Date)
    df.wood.c$year <- year(df.wood.c$Date)

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
        
 
    
    for (i in 1:6) {
        dfr[dfr$ring == i, "leaf_p_pool"] <- dfr[dfr$ring == i, "leaf_pool"]/1000 * df.m[df.m$Ring == i, "P"]/1000
    }
    
    # leaf p pool in unit of g/m2
    df.out <- dfr[, c("Date", "ring", "leaf_p_pool")]
    
    
    return(df.out)
    
}


assign_percent_to_pool <- function(input1, input2, input3, colname1, colname2) {
    
    ### input1 <- leaf % c
    ### input2 <- leaf % p
    ### input3 <- leaf_c_pool
    
    out <- input3
    
    ### find the common month and year for dead leaf
    for (i in c(1:6)) {
        mydf1 <- subset(input1, Ring == i)
        mydf2 <- subset(input2, Ring == i)
        
        for (j in mydf1$year) {
            
            mydf3 <- subset(mydf1, year == j)
            mydf4 <- subset(mydf2, year == j)
            
            for (k in mydf3$month) {
                mydf5 <- subset(mydf3, month == k)
                mydf6 <- subset(mydf4, month == k)
                
                out[out$Ring == i & out$year == j & out$month == k, colname1] <- mydf5$PercC
                out[out$Ring == i & out$year == j & out$month == k, colname2] <- mydf6$PercP
            }
        }
    }
    ### complete cases
    outDF <- out[complete.cases(out),]
    
    return(outDF)
}