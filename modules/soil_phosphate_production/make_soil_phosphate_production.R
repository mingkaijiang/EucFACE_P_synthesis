
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
    
    
    
    
    
    
    return(myDF3.m)
}