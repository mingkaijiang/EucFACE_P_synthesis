#- Make the soil C pool
make_soil_c_pool <- function(){

    # download the data
    infile <- "FACE_P0014_ALL_BasicSoilProperties_L1_2012.csv"
    if(!file.exists(paste0("download/", infile))) {
        download_soil_p_data()
    }

    ## read in data - soil property data
    myDF2 <- read.csv(file.path(getToPath(), 
                                "FACE_P0014_ALL_BasicSoilProperties_L1_2012.csv"))
    
    myDF3 <- read.csv(file.path(getToPath(), 
                                "FACE_P0014_ALL_BasicSoilProperties_L1_2013.csv"))   
    
    myDF4 <- read.csv(file.path(getToPath(), 
                                "FACE_P0014_ALL_BasicSoilProperties_L1_2014.csv"))
    
    myDF5 <- read.csv(file.path(getToPath(), 
                                "FACE_P0014_ALL_BasicSoilProperties_L1_2015.csv"))
    
    myDF2 <- myDF2[,1:10]
    myDF3 <- myDF3[,1:10]
    myDF4 <- myDF4[,1:10]
    myDF5 <- myDF5[,1:10]
    
    colnames(myDF2) <- c("Date", "SampleNumber", "ring", "plot", "depth", "pH", "gmc", "totC", "totN", "totP_ppm")
    colnames(myDF3) <- c("Date", "SampleNumber", "ring", "plot", "depth", "pH", "gmc", "totC", "totN", "totP_ppm")
    colnames(myDF4) <- c("Date", "SampleNumber", "ring", "plot", "depth", "pH", "gmc", "totC", "totN", "totP_ppm")
    colnames(myDF5) <- c("Date", "SampleNumber", "ring", "plot", "depth", "pH", "gmc", "totC", "totN", "totP_ppm")
    
    myDF <- rbind(myDF2, myDF3, myDF4, myDF5)
    myDF$Date <- dmy(myDF$Date)
    
    # average across rings, dates, and depths, unit: ppm which is mg/kg
    myDF.m <- summaryBy(totC~Date+ring+depth,data=myDF,FUN=mean,keep.names=T,na.rm=T)
    myDF.m <- data.frame(lapply(myDF.m, trimws), stringsAsFactors = FALSE)
    myDF.m$totC <- as.numeric(myDF.m$totC)
    myDF.m$Date <- as.Date(myDF.m$Date)
    myDF.m <- myDF.m[!(is.nan(myDF.m$totC)), ]
    
    myDF.out <- myDF.m[,c("Date", "ring", "depth", "totC")]
    colnames(myDF.out) <- c("Date", "Ring", "Depth", "TotC")

    
    # incomplete, as we need to sum across all three depths!
    
    return(myDF.out)
    
}
