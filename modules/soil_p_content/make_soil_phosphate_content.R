
make_soil_phosphase_content <- function() {
    # download the data
    download_soil_p_data()
    
    ## read in data - extractable NP data
    myDF6 <- read.csv(file.path(getToPath(), 
                                "FACE_P0014_ALL_extractableNP_L1_20140314-2014-11-17_V1.csv"))
    
    myDF7 <- read.csv(file.path(getToPath(), 
                                "FACE_P0014_ALL_extractableNP_L1_20150310-20151130_V1.csv"))
    
    myDF6 <- myDF6[,1:8]
    myDF7 <- myDF7[,c("Date", "sample_number", "ring", "plot", "depth", "nitrate", "ammonium", "phosphate")]
    names(myDF7) <- names(myDF6)
    
    myDF8 <- rbind(myDF6, myDF7)
    myDF8$date <- dmy(myDF8$date)
    
    # average across rings, dates, and depths, unit: mg/kg PO4
    myDF8.m <- summaryBy(phosphate~date+ring+depth,data=myDF8,FUN=mean,keep.names=T,na.rm=T)
    
    return(myDF8.m)
}