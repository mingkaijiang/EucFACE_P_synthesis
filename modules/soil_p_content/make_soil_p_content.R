#- Make the soil P content pool
make_soil_p_content <- function(bk_density){
    # return ring-specific, time series data of soil P content 
    # need to read in multiple P data sources
    # and soil bulk density data
    # bk_density is the ring-specific soil density data (kg/m3) across depths
    
    # download the data
    download_soil_p_data()
    
    ## read in the csv - elemental analysis data
    myDF1 <- read.csv(file.path(getToPath(), 
                                "FACE_P0014_ALL_ ElementalAnalysis_2012to2014_V2.csv"))
    
    # Updating date and variable names
    myDF1$Date <- as.Date(myDF1$Date)
    names(myDF1)[30] <- "Phosphorus_mg_kg"
    
    # average across rings, dates, and depths, unit: mg/kg
    myDF1.m <- summaryBy(Phosphorus_mg_kg~Date+Ring+Depth,data=myDF1,FUN=mean,keep.names=T,na.rm=T)
    
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
    
    # average across rings, dates, and depths, unit: ppm
    myDF.m <- summaryBy(totP_ppm~Date+ring+depth,data=myDF,FUN=mean,keep.names=T,na.rm=T)
    

    return()
    
}
