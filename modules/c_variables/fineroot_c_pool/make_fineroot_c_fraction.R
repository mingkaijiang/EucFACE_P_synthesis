#- Make the fine root C fraction
make_fineroot_c_fraction <- function(back.calculate,
                                     soil_bulk_density){
    
    
    ### there are three datasets with different structures, dates and fineroot data
    ### Most importantly, one dataset contains the depth profile,
    ### and one contains the different root size profile.
    ### The fineroot (< 2mm in diameter) is tiny compared to the intermediate size (2-3mm),
    ### so it's important to include the intermediate root
    
    ### download data
    download_fineroot_c_data()
    
    ### depth profile
    dDF <- read.csv("download/FACE_PILOTROOT_RA_FINEROOTS_L1_20131201-20131231.csv")
    colnames(dDF) <- c("Number", "RingID", "Replicate1", "Replicate2", "Depth", "Date",
                       "BulkDensity", "SoilpH", "Moisture", "FRB", "Cpercent", "FRB_0_50",
                       "Abv_biomass", "Root_shoot", "DistanceNT", "DBH_of_NT")
    dDF$Date <- as.Date(as.character(paste0("01-", dDF$Date)), "%d-%b-%y")
    
    dDF <- dDF[,c("Number", "RingID", "Depth", "Date", "FRB", "Cpercent", "BulkDensity")]
    dDF$Depth <- gsub("Oct-20", "10 - 20", dDF$Depth)
    dDF$RingID[160] <- "R4"
    
    names(dDF)[names(dDF)=="RingID"] <- "Ring"
    dDF$Ring <- gsub("R", "", dDF$Ring)
    dDF$Ring <- as.numeric(dDF$Ring)
    
    ### calculate FRB based on revised bulk density
    if (back.calculate==T) {
        tmpDF <- dDF
        tmpDF$FRB1 <- ifelse(tmpDF$Depth%in%c("0 - 10", "10 - 20", "20 - 30"),
                            tmpDF$FRB/0.1, 
                            tmpDF$FRB/0.2)
        
        tmpDF$FRB2 <- tmpDF$FRB1 / tmpDF$BulkDensity
        
        ### add new bulk density
        for (i in c(1:6)) {
            tmpDF$BDrev[tmpDF$Ring==i&tmpDF$Depth=="0 - 10"] <- soil_bulk_density$bulk_density_kg_m3[soil_bulk_density$Ring==i&soil_bulk_density$Depth=="0_10"]
            tmpDF$BDrev[tmpDF$Ring==i&tmpDF$Depth=="10 - 20"] <- soil_bulk_density$bulk_density_kg_m3[soil_bulk_density$Ring==i&soil_bulk_density$Depth=="10_30"]
            tmpDF$BDrev[tmpDF$Ring==i&tmpDF$Depth=="20 - 30"] <- soil_bulk_density$bulk_density_kg_m3[soil_bulk_density$Ring==i&soil_bulk_density$Depth=="10_30"]
            tmpDF$BDrev[tmpDF$Ring==i&tmpDF$Depth=="30 - 50"] <- soil_bulk_density$bulk_density_kg_m3[soil_bulk_density$Ring==i&soil_bulk_density$Depth=="transition"]
        }
        
        tmpDF$BDrev <- tmpDF$BDrev / 1000.0
        
        ### recalculate FRB
        tmpDF$FRB3 <- tmpDF$FRB2 * tmpDF$BDrev
        
        ### last to consider depth
        tmpDF$FRB4 <- ifelse(tmpDF$Depth%in%c("0 - 10", "10 - 20", "20 - 30"),
                             tmpDF$FRB3*0.1, 
                             tmpDF$FRB3*0.2)
        
        ### prepare out
        dDF <- tmpDF[,c("Number", "Ring", "Depth", "Date", "FRB4", "Cpercent")]
        names(dDF)[names(dDF)=="FRB4"] <- "FRB"
        
    }
    
    ### convert from fresh to dry
    dDF$FRB <- dDF$FRB * 0.5
    
    
    ### estimate 50-60 profile, assuming 1/24
    dDF2 <- dDF[dDF$Depth == "30 - 50",]
    dDF2$Depth <- "50 - 60"
    dDF2$FRB <- dDF2$FRB / 2
    
    ## merge
    myDF <- rbind(dDF, dDF2)
    
    ## calculate C content
    #myDF$FRC <- myDF$FRB * myDF$Cpercent / 100.0
    
    ### re-categorize, 0-10, 10-30, 30-60
    reDF1 <- myDF[myDF$Depth == "0 - 10", c("Number", "Ring", "Date", "Cpercent", "Depth")]
    
    reDF1$Depth <- gsub("0 - 10", "0_10", reDF1$Depth)
    
    outDF <- summaryBy(Cpercent~Ring, FUN=mean, 
                       data=reDF1, na.rm=T, keep.names=T)
    
    outDF$Cpercent <- outDF$Cpercent / 100.0
    
    
    reDF2 <- myDF[myDF$Depth %in%c("10 - 20", "20 - 30"), ]
    reDF3 <- myDF[myDF$Depth %in%c("30 - 50", "50 - 60"), ]
    
    ### calculate total for 10 - 30
    reDF4 <- summaryBy(Cpercent~Number+Ring+Date, FUN=mean,
                       data=reDF2, na.rm=T, keep.names=T)
    
    reDF4$Depth <- "10_30"
    
    reDF5 <- summaryBy(Cpercent~Number+Ring+Date, FUN=mean,
                       data=reDF3, na.rm=T, keep.names=T)
    
    reDF5$Depth <- "transition"
    
    depthDF <- rbind(reDF1, rbind(reDF4, reDF5))
    
    ### calculate total C
    outDF <- summaryBy(Cpercent~Ring+Depth, FUN=mean, 
                       data=depthDF, na.rm=T, keep.names=T)
    
    outDF$Cpercent <- outDF$Cpercent / 100.0
    
    
    ### return
    return(outDF)
}