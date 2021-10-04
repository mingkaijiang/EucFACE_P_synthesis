
make_soil_p_mineralization_flux_bk_first <- function(bk_density,
                                            fineroot_c_pool,
                                            which.variable) {
    
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
    
    
    # average across rings, dates, depth only for top 10cm, unit: mg/kg/d 
    myDF.m <- summaryBy(P_mineralisation~date+ring,data=myDF,
                        FUN=mean,keep.names=T,na.rm=T)
    
    # assign depth
    myDF.m$Depth <- "0_10"
    names(myDF.m)[names(myDF.m)=="ring"] <- "Ring"
    
    # merge with soil bulk density to calculate mineralization flux in unit of mg/m3/d
    bk_density <- bk_density[bk_density$Depth=="0_10",]
    
    myDF <- merge(myDF.m, bk_density, by=c("Ring", "Depth"))
    
    # convert from mg/m3/d to mg/m2/d
    myDF$p_min_mg_m2_d <- with(myDF, P_mineralisation*bulk_density_kg_m3 * 0.1)
    
    
    # read in tmpDF
    # read in the Oct 2018 Johanna data at deeper depths
    tmpDF <- read.csv("temp_files/belowground_P_working_sheet.csv")
    tmpDF$Date <- as.Date(tmpDF$Date, format="%d/%m/%y")
    
    ### there are two NANs in the Cmic dataset, fill the gap
    v <- mean(tmpDF$Cmic[tmpDF$Depth=="transition"], na.rm=T)
    tmpDF$Cmic[tmpDF$Depth=="transition"&tmpDF$Ring%in%c(6,2)] <- v
    
    ### add fineroot c pool
    frDF <- summaryBy(fineroot_0_10_cm+fineroot_10_30_cm+fineroot_30_60_cm~Ring,
                      FUN=mean, data=fineroot_c_pool, keep.names=T, na.rm=T)
    frDF1 <- frDF[,c("Ring", "fineroot_0_10_cm")]
    colnames(frDF1) <- c("Ring", "FRC")
    frDF1$Depth <- "0_10"
    frDF2 <- frDF[,c("Ring", "fineroot_10_30_cm")]
    colnames(frDF2) <- c("Ring", "FRC")
    frDF2$Depth <- "10_30"
    frDF3 <- frDF[,c("Ring", "fineroot_30_60_cm")]
    colnames(frDF3) <- c("Ring", "FRC")
    frDF3$Depth <- "transition"
    frDF <- rbind(frDF1, rbind(frDF2, frDF3))
    
    tmpDF <- merge(tmpDF, frDF, by=c("Ring", "Depth"))
    
    
    if (which.variable == "Pmic") {
        names(tmpDF)[names(tmpDF)=="Pmic"] <- "Ref_var"
    } else if (which.variable == "Cmic") {
        names(tmpDF)[names(tmpDF)=="Cmic"] <- "Ref_var"
    } else if (which.variable == "SoilC") {
        names(tmpDF)[names(tmpDF)=="SoilC"] <- "Ref_var"
    } else if (which.variable == "FinerootC") {
        names(tmpDF)[names(tmpDF)=="FRC"] <- "Ref_var"
    } 
    
    tmpDF <- tmpDF[,c("Date", "Ring", "Depth", "Ref_var")]

    ### assign ring-specific reduction relationship
    for (i in 1:6) {
        tmpDF$Red[tmpDF$Ring==i&tmpDF$Depth=="10_30"] <- tmpDF$Ref_var[tmpDF$Ring==i&tmpDF$Depth=="10_30"]/tmpDF$Ref_var[tmpDF$Ring==i&tmpDF$Depth=="0_10"]
        tmpDF$Red[tmpDF$Ring==i&tmpDF$Depth=="transition"] <- tmpDF$Ref_var[tmpDF$Ring==i&tmpDF$Depth=="transition"]/tmpDF$Ref_var[tmpDF$Ring==i&tmpDF$Depth=="0_10"]
    }
    
    ### calculate mineralization rate at deeper soil depths
    for (i in 1:6) {
        myDF$Pmin_10_30[myDF$Ring==i] <- myDF$p_min_mg_m2_d[myDF$Ring==i] * tmpDF$Red[tmpDF$Ring==i&tmpDF$Depth=="10_30"]
        myDF$Pmin_transition[myDF$Ring==i] <- myDF$p_min_mg_m2_d[myDF$Ring==i] * tmpDF$Red[tmpDF$Ring==i&tmpDF$Depth=="transition"]
    }

    myDF1 <- myDF[,c("date", "Ring", "Depth", "p_min_mg_m2_d")]
    myDF2 <- myDF[,c("date", "Ring", "Depth", "Pmin_10_30")]
    myDF3 <- myDF[,c("date", "Ring", "Depth", "Pmin_transition")]
    
    myDF2$Depth <- "10_30"
    myDF3$Depth <- "transition"
    
    colnames(myDF1) <- colnames(myDF2) <- colnames(myDF3) <- c("Date", "Ring", "Depth", "p_mineralization_mg_m2_d")
    
    myDF <- rbind(myDF1, rbind(myDF2, myDF3))
    
    
    # output table
    myDF.out <- myDF[,c("Date", "Ring", "Depth", "p_mineralization_mg_m2_d")]

    ### year 2016 only has one value, which is in Jan, decided to group it with 2015
    #myDF.out$Date <- gsub("2016-01-21", "2015-01-21", myDF.out$Date)
    
    
    myDF.out$Start_date <- myDF.out$End_date <- myDF.out$Date
    myDF.out$Days <- 1
    
    myDF.out <- myDF.out[complete.cases(myDF.out$p_mineralization_mg_m2_d),]
    

    # plotting time series
    pdf("plots_tables/checks/soil_p_mineralization_over_time.pdf")
    
    # compare eCO2 and aCO2 treatment
    myDF.out[myDF.out$Ring == 1, "CO2"] <- "eCO2"
    myDF.out[myDF.out$Ring == 4, "CO2"] <- "eCO2"
    myDF.out[myDF.out$Ring == 5, "CO2"] <- "eCO2"
    
    myDF.out[myDF.out$Ring == 2, "CO2"] <- "aCO2"
    myDF.out[myDF.out$Ring == 3, "CO2"] <- "aCO2"
    myDF.out[myDF.out$Ring == 6, "CO2"] <- "aCO2"
    
    # check CO2 treatment averages
    test1 <- summaryBy(p_mineralization_mg_m2_d~CO2, 
                       data=myDF.out, FUN=mean, keep.names=T, na.rm=T)

    # plotting
    p <- ggplot(myDF.out, aes(Date, p_mineralization_mg_m2_d, color=factor(CO2))) +   
        geom_point(size = 5) +
        xlab("Date") + ylab("P mineralization rate (mg m-2 d-1)") 
    plot(p) 
    dev.off()
    
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