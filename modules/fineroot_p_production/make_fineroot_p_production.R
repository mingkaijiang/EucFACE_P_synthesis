#### Make the fine root P production
make_fineroot_p_production <- function(){
    
    ### download the data
    download_fineroot_p_data()
    
    ### read in the csv
    myDF <- read.csv("download/EucFACE_FineRootData.csv")
    prodDF <- read.csv(file.path(getToPath(), 
                                "FACE_P0083_RA_FR-PRODUCTION_L1_20140601-20150915.csv"))
    
    ### setting up the date
    myDF$Date <- paste0("1-", as.character(myDF$Date))
    myDF$date <- as.Date(myDF$Date, "%d-%b-%y")
    prodDF$Date <- paste0("1-", as.character(prodDF$Date))
    prodDF$Date <- as.Date(prodDF$Date, "%d-%b-%y")
    
    
    ### average across rings and dates, for each depth
    frp.1 <- summaryBy(P.ppm.0~Ring.ID+date,data=myDF,FUN=mean,keep.names=T, na.rm=T)
    frp.2 <- summaryBy(P.ppm30~Ring.ID+date,data=myDF,FUN=mean,keep.names=T, na.rm=T)
    frb.1 <- summaryBy(Fine.Root.Production..g.m2.day..0.10cm~Ring.ID+Date,data=prodDF,FUN=mean,keep.names=T, na.rm=T)
    frb.2 <- summaryBy(Fine.Root.Production..g.m2.day..10.30cm~Ring.ID+Date,data=prodDF,FUN=mean,keep.names=T, na.rm=T)
    
    ### convert date to character
    frp.1$date <- as.character(frp.1$date)
    frp.2$date <- as.character(frp.2$date)
    frb.1$Date <- as.character(frb.1$Date)
    frb.2$Date <- as.character(frb.2$Date)
    
    ### assign biomass data onto P concentration data according to date - 0 - 10 cm
    for (i in unique(frb.1$Date)) {
        for (j in 1:6) {
            frp.1[frp.1$Ring.ID == j & frp.1$date == i, "frp_0_10cm"] <- frp.1[frp.1$Ring.ID == j & frp.1$date == i, 
                                                                               "P.ppm.0"] * frb.1[frb.1$Ring.ID == j & frb.1$Date == i, "Fine.Root.Production..g.m2.day..0.10cm"] / 1000000
        }
    }
    
    ### assign biomass data onto P concentration data according to date - 10 - 30 cm
    for (i in unique(frb.2$Date)) {
        for (j in 1:6) {
            frp.2[frp.2$Ring.ID == j & frp.2$date == i, "frp_10_30cm"] <- frp.2[frp.2$Ring.ID == j & frp.2$date == i, 
                                                                                "P.ppm30"] * frb.2[frb.2$Ring.ID == j & frb.2$Date == i, "Fine.Root.Production..g.m2.day..10.30cm"] / 1000000
        }
    }
    
    
    ### summing across depths
    frp.1$frp_10_30cm <- frp.2$frp_10_30cm 
    frp.1$frp_0_30cm <- (frp.1$frp_10_30cm + frp.1$frp_0_10cm) 
    
    
    #- format dataframe to return
    frp.out <- frp.1[,c("date","Ring.ID","frp_0_30cm")]
    names(frp.out) <- c("Date", "Ring", "Fineroot_P_production_g_m2_d")
    
    return(frp.out)
}