
make_soil_p_mineralization_flux <- function(bk_density) {
    
    # download the data
    download_soil_p_mineralization_data()
    
    ## read in data - extractable NP data
    myDF1 <- read.csv(file.path(getToPath(), 
                                "FACE_RA_P0023_SOILMINERALISATION_L3_20120724-20140124.csv"))
    
    myDF2 <- read.csv(file.path(getToPath(), 
                                "FACE_RA_P0023_SOILMINERALISATION_L1_20140428-20160121.csv"))
    
    myDF1 <- myDF1[,c("date", "ring", "nitrification", "N_mineralisation", "P_mineralisation")]
    myDF2 <- myDF2[,c("date", "ring", "nitrification", "N_mineralisation", "P_mineralisation")]
    
    myDF1$date <- as.Date(as.character(myDF1$date), format="%Y-%m-%d")
    myDF2$date <- as.Date(as.character(myDF2$date), format="%d/%m/%Y")
    
    myDF <- rbind(myDF1, myDF2)
    
    
    # average across rings, dates, and depths, unit: mg/kg/d 
    myDF.m <- summaryBy(P_mineralisation~date+ring,data=myDF,FUN=mean,keep.names=T,na.rm=T)
    

    # obtain ring averaged soil bulk density (0 - 10 cm only)
    bk_density <- subset(bk_density, Depth == "0-10cm")
    
    # add bulk density
    for (i in 1:6){
        myDF.m[myDF.m$ring == i, "bk_density"] <- bk_density[bk_density$ring == i, "bulk_density_kg_m3"] 
    }
    
    # from mg kg-1 d-1 to mg m-2 d-1
    myDF.m$p_mineralization_mg_m2_d <- myDF.m$P_mineralisation * myDF.m$bk_density * 0.1 

    # output table
    myDF.out <- myDF.m[,c("date", "ring", "p_mineralization_mg_m2_d")]
    colnames(myDF.out) <- c("Date", "Ring", "p_mineralization_mg_m2_d")
    
    ### year 2016 only has one value, which is in Jan, decided to group it with 2015
    #myDF.out$Date <- gsub("2016-01-21", "2015-01-21", myDF.out$Date)
    
    
    myDF.out$Start_date <- myDF.out$End_date <- myDF.out$Date
    myDF.out$Days <- 1
    
    myDF.out <- myDF.out[complete.cases(myDF.out$p_mineralization_mg_m2_d),]
    

    # plotting time series
    #pdf("plots_tables/soil_p_mineralization_over_time.pdf")
    #
    ## compare eCO2 and aCO2 treatment
    #myDF.out[myDF.out$Ring == 1, "CO2"] <- "eCO2"
    #myDF.out[myDF.out$Ring == 4, "CO2"] <- "eCO2"
    #myDF.out[myDF.out$Ring == 5, "CO2"] <- "eCO2"
    #
    #myDF.out[myDF.out$Ring == 2, "CO2"] <- "aCO2"
    #myDF.out[myDF.out$Ring == 3, "CO2"] <- "aCO2"
    #myDF.out[myDF.out$Ring == 6, "CO2"] <- "aCO2"
    #
    ## check CO2 treatment averages
    #test1 <- summaryBy(p_mineralization_mg_m2_d~CO2, 
    #                   data=myDF.out, FUN=mean, keep.names=T, na.rm=T)
    
    ## plotting
    #p <- ggplot(myDF.out, aes(Date, p_mineralization_mg_m2_d, color=factor(CO2))) +   
    #    geom_point(size = 5) +
    #    xlab("Date") + ylab("P mineralization rate (mg m-2 d-1)")# + 
    #    #annotate("text", x = "2013-10-22", y = 4, 
    #    #         label = paste0("aCO2 = ", round(test1[1,2], 2))) +
    #    #annotate("text", x = "2013-10-22", y = 3.8, 
    #    #         label = paste0("eCO2 = ", round(test1[2,2], 2)))
    #plot(p) 
    #dev.off()
    
    # Shun's GCB paper indicates that P mineralization rate was higher
    # under eCO2, but here it is lower. 
    # However, the exact texts were:
    # "eCO2 was also associated with faster nutrient turnover rates
    # in the first six months of the experiment,
    # with higher N (+175%) and P (+211%) mineralization rates compared to ambient rings, 
    # although this difference did not persist."
    # Hence, CO2 treatment effct is only there for the first few points! 
    
    return(myDF.out)
}