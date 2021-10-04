#- Make the fine root C pool
make_fineroot_c_pool <- function(){
    
    ### download data
    download_fineroot_c_data()
    
    
    # read in the csv
    frb1 <- read.csv("temp_files/EucFACERootsRingDateDepth.csv")
    frb1$Date <- as.Date(frb1$Dateform, format="%d-%m-%Y")
    
    # fineroot C pool
    frb1$frb_top <- with(frb1, FRB_0.10cm * C0_0.10cm / 100)
    frb1$frb_bot <- with(frb1, FRB_10.30cm * C0_10.30cm / 100)
    frb1$frb_tot <- with(frb1, frb_top + frb_bot)
    
    # average across rings and dates
    frb.m <- summaryBy(frb_tot+frb_top+frb_bot~Date+Ring,data=frb1,FUN=mean,keep.names=T)
    
    ## colnames
    colnames(frb.m) <- c("Date","Ring","fineroot_pool", "fineroot_0_10_cm", "fineroot_10_30_cm")
    
    
    ### depth profile
    dDF <- read.csv("download/FACE_PILOTROOT_RA_FINEROOTS_L1_20131201-20131231.csv")
    colnames(dDF) <- c("Number", "RingID", "Replicate1", "Replicate2", "Depth", "Date",
                       "BulkDensity", "SoilpH", "Moisture", "FRB", "Cpercent", "FRB_0_50",
                       "Abv_biomass", "Root_shoot", "DistanceNT", "DBH_of_NT")
    dDF$Date <- as.Date(as.character(paste0("01-", dDF$Date)), "%d-%b-%y")
    
    dDF <- dDF[,c("Number", "Depth", "Date", "FRB", "Cpercent")]
    dDF$Depth <- gsub("Oct-20", "10 - 20", dDF$Depth)
    
    #tmpDF1 <- dDF[dDF$Depth == "0 - 10",]
    #tmpDF2 <- dDF[dDF$Depth == "10 - 20",]
    #tmpDF3 <- dDF[dDF$Depth == "20 - 30",]
    #tmpDF4 <- dDF[dDF$Depth == "30 - 50",]

    ### estimate 50-60 profile, assuming 1/24
    dDF2 <- dDF[dDF$Depth == "30 - 50",]
    dDF2$Depth <- "50 - 60"
    dDF2$FRB <- dDF2$FRB / 2
    
    ## merge
    myDF <- rbind(dDF, dDF2)
    
    ### consistency check
    test <- summaryBy(FRB~Number+Date, FUN=sum, data=myDF, na.rm=T, keep.names=T)
    colnames(test) <- c("Number", "Date", "FRB_0_60")
    myDF <- merge(myDF, test, by=c("Number", "Date"))
    
    
    
    
    dDF$FRB_0_60 <- dDF$FRB_0_50 * 1/24 + dDF$FRB_0_50
    
    
    
    
    ### obtain C
    dDF$fineroot_C <- dDF$FRB * dDF$Cpercent / 100
    
    ### return
    return(frb.m)
}