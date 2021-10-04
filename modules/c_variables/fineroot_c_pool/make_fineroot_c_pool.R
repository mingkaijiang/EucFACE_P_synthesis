#- Make the fine root C pool
make_fineroot_c_pool <- function(){
    
    ### download data
    download_fineroot_c_data()
    
    
    
    
    
    ### depth profile
    dDF <- read.csv("download/FACE_PILOTROOT_RA_FINEROOTS_L1_20131201-20131231.csv")
    colnames(dDF) <- c("Number", "RingID", "Replicate1", "Replicate2", "Depth", "Date",
                       "BulkDensity", "SoilpH", "Moisture", "FRB", "Cpercent", "FRB_0_50",
                       "Abv_biomass", "Root_shoot", "DistanceNT", "DBH_of_NT")
    dDF$Date <- as.Date(as.character(paste0("01-", dDF$Date)), "%d-%b-%y")
    
    dDF <- dDF[,c("Number", "RingID", "Depth", "Date", "FRB", "Cpercent")]
    dDF$Depth <- gsub("Oct-20", "10 - 20", dDF$Depth)
    dDF$RingID[160] <- "R4"
    
    ### convert from fresh to dry
    dDF$FRB <- dDF$FRB * 0.5
    
    
    ### estimate 50-60 profile, assuming 1/24
    dDF2 <- dDF[dDF$Depth == "30 - 50",]
    dDF2$Depth <- "50 - 60"
    dDF2$FRB <- dDF2$FRB / 2
    
    ## merge
    myDF <- rbind(dDF, dDF2)
    
    ## calculate C content
    myDF$FRC <- myDF$FRB * myDF$Cpercent / 100.0
    
    ### re-categorize, 0-10, 10-30, 30-60
    reDF1 <- myDF[myDF$Depth == "0 - 10", c("Number", "RingID", "Date", "FRC", "Depth")]
    
    reDF1$Depth <- gsub("0 - 10", "0_10", reDF1$Depth)
    
    reDF2 <- myDF[myDF$Depth %in%c("10 - 20", "20 - 30"), ]
    reDF3 <- myDF[myDF$Depth %in%c("30 - 50", "50 - 60"), ]
    
    ### calculate total for 10 - 30
    reDF4 <- summaryBy(FRC~Number+RingID+Date, FUN=sum,
                       data=reDF2, na.rm=T, keep.names=T)
    
    reDF4$Depth <- "10_30"
    
    reDF5 <- summaryBy(FRC~Number+RingID+Date, FUN=sum,
                       data=reDF3, na.rm=T, keep.names=T)
    
    reDF5$Depth <- "transition"
    
    depthDF <- rbind(reDF1, rbind(reDF4, reDF5))
    
    ### calculate total C
    totDF <- summaryBy(FRC~Number+RingID+Date, FUN=sum, 
                       data=depthDF, na.rm=T, keep.names=T)
    colnames(totDF) <- c("Number", "RingID", "Date", "FRC_0_60")
    
    depthDF <- merge(depthDF, totDF, by=c("Number", "RingID", "Date"))
    
    depthDF$pct <- depthDF$FRC / depthDF$FRC_0_60
    
    sumDF <- summaryBy(pct~RingID+Depth, FUN=c(mean,sd),
                       data=depthDF, keep.names=T, na.rm=T)
    
    names(sumDF)[names(sumDF)=="RingID"] <- "Ring"
    sumDF$Ring <- gsub("R", "", sumDF$Ring)
    sumDF$Ring <- as.numeric(sumDF$Ring)
    transDF <- sumDF[sumDF$Depth=="transition",]
    
    
    
    # read in the csv
    frb1 <- read.csv("temp_files/EucFACERootsRingDateDepth.csv")
    frb1$Date <- as.Date(frb1$Dateform, format="%d-%m-%Y")
    
    # fineroot C pool
    frb1$frb_top <- with(frb1, FRB_0.10cm * C0_0.10cm / 100)
    frb1$frb_bot <- with(frb1, FRB_10.30cm * C0_10.30cm / 100)
    frb1$frb_tot <- with(frb1, frb_top + frb_bot)
    
    # average across rings and dates
    outDF <- summaryBy(frb_tot+frb_top+frb_bot~Date+Ring,
                       data=frb1,FUN=mean,keep.names=T)
    
    ## colnames
    colnames(outDF) <- c("Date","Ring","fineroot_pool_0_30cm", 
                         "fineroot_0_10_cm", "fineroot_10_30_cm")
    
    outDF <- merge(outDF, transDF, by=c("Ring"))
    
    ### calculate total and 30-60cm
    outDF$fineroot_pool <- with(outDF, fineroot_pool_0_30cm/(1-pct.mean))
    outDF$fineroot_30_60_cm <- with(outDF, fineroot_pool-fineroot_pool_0_30cm)
    
    
    ### clean
    outDF <- outDF[,c("Ring", "Date", "fineroot_pool", 
                      "fineroot_0_10_cm",
                      "fineroot_10_30_cm", 
                      "fineroot_30_60_cm")]
    
    ### add the 2013 data
    depthDF <- depthDF[,c("Number", "RingID", "Depth", "Date", "FRC")]
    names(depthDF)[names(depthDF)=="RingID"] <- "Ring"
    depthDF$Ring <- gsub("R", "", depthDF$Ring)
    depthDF$Ring <- as.numeric(depthDF$Ring)
    
    sumDF <- summaryBy(FRC~Ring+Depth+Date, FUN=mean,
                       data=depthDF, na.rm=T, keep.names=T)
    
    outDF2 <- dcast(setDT(sumDF), Ring~Depth, value.var="FRC")
    outDF2 <- as.data.frame(outDF2)
    colnames(outDF2) <- c("Ring", "fineroot_0_10_cm", "fineroot_10_30_cm",
                          "fineroot_30_60_cm")
    outDF2$Date <- "2013-12-01"
    outDF2$fineroot_pool <- outDF2$fineroot_0_10_cm + outDF2$fineroot_10_30_cm + outDF2$fineroot_30_60_cm
    outDF2 <- outDF2[,c("Ring", "Date", "fineroot_pool",
                        "fineroot_0_10_cm", "fineroot_10_30_cm",
                        "fineroot_30_60_cm")]
    
    out <- rbind(outDF, outDF2)
    
    out <- out[order(out$Ring, out$Date),]
    
    ## test with 
    #test <- summaryBy(fineroot_pool~Date, FUN=mean, data=out, keep.names=T)
    #with(test, plot(fineroot_pool~Date))
    
    ### return
    return(out)
}