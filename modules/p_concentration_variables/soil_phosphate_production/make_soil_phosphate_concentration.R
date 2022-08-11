
make_soil_phosphate_concentration <- function() {
    
    ### need to use depth profile to extrapolate earlier datasets that have top 10 cm of soil only
    ### purpose: to preserve the depth profile in all time points.
    
    # phosphate:	Phosphate-P concentrations in kg dry soil (mg/kg) 
    #Extractable PO4 as determined by Bray P extraction and AQ2 determination
    
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
    
    # get only top 10 cm
    #myDF3 <- myDF3[which(myDF3$depth %in% " 0_10cm"),]
    myDF3$depth <- "0_10"
    myDF3 <- myDF3[!is.na(myDF3$phosphate),]
    
    # read in Shun's data to expand the temporal coverages of the previous data
    myDF4 <- read.csv(file.path(getToPath(), 
                                "FACE_RA_P0023_SOILEXTRACTABLENUTRIENTS_L3_20120613-20140310.csv"))
    
    myDF4.m <- summaryBy(phosphate~date+ring,data=myDF4,FUN=mean,keep.names=T,na.rm=T)
    myDF4.m$date <- as.Date(myDF4.m$date)
    myDF4$depth <- "0_10"
    
    myDF3 <- myDF3[,c("date", "ring", "depth", "phosphate")]
    myDF4 <- myDF4[,c("date", "ring", "depth", "phosphate")]
    myDF4$date <- as.Date(myDF4$date, "%Y-%m-%d")
    
    # combine both dataframes
    myDF <- rbind(myDF4, myDF3)
    
    colnames(myDF) <- c("Date", "Ring", "Depth", "Phosphate")
    myDF$Date <- as.Date(myDF$Date, format="%Y-%m-%d")
    
    
    # read in the Oct 2018 Johanna data at deeper depths
    tmpDF <- read.csv("temp_files/belowground_P_working_sheet.csv")
    
    tmpDF <- tmpDF[,c("Date", "Ring", "Depth", "BrayP")]
    colnames(tmpDF) <- c("Date", "Ring", "Depth", "Phosphate")
    tmpDF$Date <- as.Date(tmpDF$Date, format="%d/%m/%y")
    
    ### calculate trt-averaged depth reduction
    tmpDF$Trt[tmpDF$Ring%in%c(1,4,5)] <- "eCO2"
    tmpDF$Trt[tmpDF$Ring%in%c(2,3,6)] <- "aCO2"
    
    #tmpDF2 <- summaryBy(Phosphate~Depth+Trt, FUN=mean, data=tmpDF, keep.names=T)
    #
    #tmpDF2$Red[tmpDF2$Trt=="aCO2"&tmpDF2$Depth=="10_30"] <- tmpDF2$Phosphate[tmpDF2$Trt=="aCO2"&tmpDF2$Depth=="10_30"]/tmpDF2$Phosphate[tmpDF2$Trt=="aCO2"&tmpDF2$Depth=="0_10"]
    #tmpDF2$Red[tmpDF2$Trt=="eCO2"&tmpDF2$Depth=="10_30"] <- tmpDF2$Phosphate[tmpDF2$Trt=="eCO2"&tmpDF2$Depth=="10_30"]/tmpDF2$Phosphate[tmpDF2$Trt=="eCO2"&tmpDF2$Depth=="0_10"]
    #
    #tmpDF2$Red[tmpDF2$Trt=="aCO2"&tmpDF2$Depth=="transition"] <- tmpDF2$Phosphate[tmpDF2$Trt=="aCO2"&tmpDF2$Depth=="transition"]/tmpDF2$Phosphate[tmpDF2$Trt=="aCO2"&tmpDF2$Depth=="0_10"]
    #tmpDF2$Red[tmpDF2$Trt=="eCO2"&tmpDF2$Depth=="transition"] <- tmpDF2$Phosphate[tmpDF2$Trt=="eCO2"&tmpDF2$Depth=="transition"]/tmpDF2$Phosphate[tmpDF2$Trt=="eCO2"&tmpDF2$Depth=="0_10"]
    
    
    tmpDF2 <- summaryBy(Phosphate~Depth+Ring, FUN=mean, data=tmpDF, keep.names=T)
    
    for (i in 1:6) {
        tmpDF2$Red[tmpDF2$Ring==i&tmpDF2$Depth=="10_30"] <- tmpDF2$Phosphate[tmpDF2$Ring==i&tmpDF2$Depth=="10_30"]/tmpDF2$Phosphate[tmpDF2$Ring==i&tmpDF2$Depth=="0_10"]
        tmpDF2$Red[tmpDF2$Ring==i&tmpDF2$Depth=="transition"] <- tmpDF2$Phosphate[tmpDF2$Ring==i&tmpDF2$Depth=="transition"]/tmpDF2$Phosphate[tmpDF2$Ring==i&tmpDF2$Depth=="0_10"]
        
    }
    
    
    
    
    # update earlier datasets
    myDF$Trt[myDF$Ring%in%c(1,4,5)] <- "eCO2"
    myDF$Trt[myDF$Ring%in%c(2,3,6)] <- "aCO2"
    
    myDF2 <- myDF3 <- myDF
    myDF2$Depth <- "10_30"
    myDF3$Depth <- "transition"
    
    #myDF2$Rev[myDF2$Trt=="aCO2"] <- myDF2$Phosphate[myDF2$Trt=="aCO2"] * tmpDF2$Red[tmpDF2$Trt=="aCO2"&tmpDF2$Depth=="10_30"]
    #myDF2$Rev[myDF2$Trt=="eCO2"] <- myDF2$Phosphate[myDF2$Trt=="eCO2"] * tmpDF2$Red[tmpDF2$Trt=="eCO2"&tmpDF2$Depth=="10_30"]
    #
    #myDF3$Rev[myDF3$Trt=="aCO2"] <- myDF3$Phosphate[myDF3$Trt=="aCO2"] * tmpDF2$Red[tmpDF2$Trt=="aCO2"&tmpDF2$Depth=="transition"]
    #myDF3$Rev[myDF3$Trt=="eCO2"] <- myDF3$Phosphate[myDF3$Trt=="eCO2"] * tmpDF2$Red[tmpDF2$Trt=="eCO2"&tmpDF2$Depth=="transition"]
    
    for (i in 1:6) {
        myDF2$Rev[myDF2$Ring==i] <- myDF2$Phosphate[myDF2$Ring==i] * tmpDF2$Red[tmpDF2$Ring==i&tmpDF2$Depth=="10_30"]
        myDF3$Rev[myDF3$Ring==i] <- myDF3$Phosphate[myDF3$Ring==i] * tmpDF2$Red[tmpDF2$Ring==i&tmpDF2$Depth=="transition"]
        
    }
    
    
    myDF2 <- myDF2[,c("Date", "Ring", "Depth", "Rev")]
    myDF3 <- myDF3[,c("Date", "Ring", "Depth", "Rev")]
    colnames(myDF2) <- colnames(myDF3) <- c("Date", "Ring", "Depth", "Phosphate")
    
    myDF <- myDF[,c("Date", "Ring", "Depth", "Phosphate")]
    
    updDF <- rbind(myDF, rbind(myDF2, myDF3))

    tmpDF <- tmpDF[,c("Date", "Ring", "Depth", "Phosphate")]
    
    myDF <- rbind(updDF, tmpDF)
    

    # average across rings, dates, and depths, unit: mg/kg PO4-P
    outDF <- summaryBy(Phosphate~Date+Ring+Depth,
                       data=myDF,FUN=mean,keep.names=T,na.rm=T)
    
    outDF <- outDF[,c("Date", "Ring", "Depth", "Phosphate")]
    
    
    # convert PO4-P in mg/kg to %
    outDF$PercP <- outDF$Phosphate / 10000
    
    
    # output table
    out <- outDF[,c("Date", "Ring", "Depth", "PercP")]

    #out$Ring <- as.character(outDF$Ring)
    
    ### plot
    p1 <- ggplot(out, aes(Date, PercP, group=Ring))+
        geom_point(aes(pch=Depth, col=Ring, fill=Ring))
    
    ### calculate average PercP per depth
    plotDF <- summaryBy(PercP~Depth, data=out, FUN=mean, keep.names=T, na.rm=T)
    
    p2 <- ggplot(plotDF, aes(Depth, PercP))+
        geom_point()
    
    ### subtract PercP in Johanna's dataset
    subDF <- subset(out, Date=="2018-10-01")
    
    p3 <- ggplot(subDF, aes(Depth, PercP))+
        geom_point()
    
    pdf("plots_tables/checks/BrayP_check.pdf")
    plot(p1)
    plot(p2)
    plot(p3)
    dev.off()

    
    
    return(out)
}