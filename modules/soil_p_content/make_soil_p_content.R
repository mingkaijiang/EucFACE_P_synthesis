#- Make the soil P content pool
make_soil_p_content <- function(){
    # return ring-specific, time series data of soil P content 
    # need to read in multiple P data sources
    # and soil bulk density data
    
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

    ## we have phosphate (mg/kg)
    
    
    
    
    #- convert to mg C m-2 day-1
    frp.m$fineroot_production_flux <- frp.m$frp_tot*c_fraction_fr*1000
    
    #- format dataframe to return
    frp.out <- frp.m[,c("Date","Ring","fineroot_production_flux")]
    return(frp.out)
    
}
