
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
    myDF3.m <- myDF3.m[,c("date", "ring", "phosphate")]
    
    myDF3.m <- myDF3.m[-c(1:6),]
    
    
    # read in Shun's data to expand the temporal coverages of the previous data
    myDF4 <- read.csv(file.path(getToPath(), 
                                "FACE_RA_P0023_SOILEXTRACTABLENUTRIENTS_L3_20120613-20140310.csv"))
    
    myDF4.m <- summaryBy(phosphate~date+ring,data=myDF4,FUN=mean,keep.names=T,na.rm=T)
    myDF4.m$date <- as.Date(myDF4.m$date)
    
    # combine both dataframes
    myDF <- rbind(myDF4.m, myDF3.m)
    
    # obtain ring averaged soil bulk density (0 - 10 cm only)
    bk_density <- subset(bk_density, Depth == "0-10cm")
    
    # convert from mg/kg of PO4 to g/m2 of P
    for (i in 1:6){
        myDF[myDF$ring == i, "bk_density"] <- bk_density[bk_density$ring == i, "bulk_density_kg_m3"] 
    }
    
    myDF$phosphate_g_m2 <- myDF$phosphate * myDF$bk_density * 0.1 / g_to_mg
    myDF$P_g_m2 <- myDF$phosphate * 31/(31+16*4)
    
    # output table
    myDF.out <- myDF[,c("date", "ring", "P_g_m2")]
    
    # plotting time series
    pdf("plots_tables/soil_phosphate_concentration_over_time.pdf")
    with(myDF.out, plot(P_g_m2~date, group_by="ring", col=rainbow(6), 
                         ylim=c(0, 1)))
    legend("topright", col=rainbow(6), legend=c(1:6), pch = 1)

    # compare eCO2 and aCO2 treatment
    myDF.out[myDF.out$ring == 1, "CO2"] <- "eCO2"
    myDF.out[myDF.out$ring == 4, "CO2"] <- "eCO2"
    myDF.out[myDF.out$ring == 5, "CO2"] <- "eCO2"
    
    myDF.out[myDF.out$ring == 2, "CO2"] <- "aCO2"
    myDF.out[myDF.out$ring == 3, "CO2"] <- "aCO2"
    myDF.out[myDF.out$ring == 6, "CO2"] <- "aCO2"
    
    boxplot(P_g_m2~CO2*date, data=myDF.out,
            col=c("gold", "green"), ylab="PO4-P (g m-2)")
    legend("topright", c("aCO2", "eCO2"), col=c("gold", "green"),  fill=c("gold", "green"))

    boxplot(P_g_m2~CO2, data=myDF.out,
            col=c("gold", "green"), ylab="PO4-P (g m-2)")
    legend("topright", c("aCO2", "eCO2"), col=c("gold", "green"),  fill=c("gold", "green"))
    
    dev.off()
    
    return(myDF.out)
}