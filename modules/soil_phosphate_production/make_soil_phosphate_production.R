
make_soil_phosphate_production <- function(bk_density) {
    # download the data
    download_soil_phosphate_data()
    
    ## read in data - extractable NP data
    myDF1 <- read.csv(file.path(getToPath(), 
                                "FACE_P0014_ALL_extractableNP_L1_20140314-2014-11-17_V1.csv"))
    
    myDF2 <- read.csv(file.path(getToPath(), 
                                "FACE_P0014_ALL_extractableNP_L1_20150310-20151130_V1.csv"))
    
    myDF1 <- myDF1[,1:8]
    myDF2 <- myDF2[,c("Date", "sample_number", "ring", "plot", "depth", "nitrate", "ammonium", "phosphate")]
    names(myDF2) <- names(myDF1)
    
    myDF3 <- rbind(myDF1, myDF2)
    myDF3$date <- dmy(myDF3$date)
    
    # average across rings, dates, and depths, unit: mg/kg PO4
    myDF3.m <- summaryBy(phosphate~date+ring+depth,data=myDF3,FUN=mean,keep.names=T,na.rm=T)
    
    myDF3.m <- myDF3.m[which(myDF3.m$depth %in% " 0_10cm"),]
    
    # obtain ring averaged soil bulk density (0 - 30 cm only)
    bk_density <- subset(bk_density, Depth == "0-10cm")
    
    # convert from mg/kg of PO4 to g/m2 of P
    for (i in 1:6){
        myDF3.m[myDF3.m$ring == i, "bk_density"] <- bk_density[bk_density$ring == i, "bulk_density_kg_m3"] 
    }
    
    myDF3.m$phosphate_g_m2 <- myDF3.m$phosphate * myDF3.m$bk_density * 0.1 / g_to_mg
    myDF3.m$P_g_m2 <- myDF3.m$phosphate * 31/(31+16*4)
    
    # output table
    myDF3.out <- myDF3.m[,c("date", "ring", "depth", "P_g_m2")]
    
    # plotting time series
    #with(myDF3.out, plot(P_g_m2~date, group_by="ring", col=rainbow(6),
    #                     ylim=c(0, 1)))
    #legend("topright", col=rainbow(6), legend=c(1:6), pch = 1)

        

    return(myDF3.out)
}