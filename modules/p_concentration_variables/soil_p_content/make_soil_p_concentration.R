#- Make the soil P concentration
make_soil_p_concentration <- function(){
    # return ring-specific, time series data of soil P content 
    # need to read in multiple P data sources
    # and soil bulk density data

    # download the data
    download_soil_p_data()
    
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
    
    myDF$depth <- gsub(" 0-10cm", "0_10", myDF$depth)
    myDF$depth <- gsub("0-10cm", "0_10", myDF$depth)
    myDF$depth <- gsub(" 10-20cm", "10_30", myDF$depth)
    myDF$depth <- gsub("10-20cm", "10_30", myDF$depth)
    myDF$depth <- gsub(" 20-30cm", "10_30", myDF$depth)
    myDF$depth <- gsub("20-30cm", "10_30", myDF$depth)
    
    myDF <- myDF[,c("Date", "ring", "depth", "totP_ppm")]
    colnames(myDF) <- c("Date", "Ring", "Depth", "totP_ppm")
    
    
    
    # read in the Oct 2018 Johanna data at deeper depths
    tmpDF <- read.csv("temp_files/belowground_P_working_sheet.csv")
    
    tmpDF <- tmpDF[,c("Date", "Ring", "Depth", "TotalP")]
    colnames(tmpDF) <- c("Date", "Ring", "Depth", "totP_ppm")
    tmpDF$Date <- as.Date(tmpDF$Date, format="%d/%m/%y")
    
    mgDF <- rbind(myDF, tmpDF)
    
    #p1 <- ggplot(mgDF, aes(Date, totP_ppm))+
    #    geom_point(aes(pch=Depth, col=Ring)); p1
    
    # average across depths first, unit: ppm which is mg/kg
    myDF.m <- summaryBy(totP_ppm~Date+Ring+Depth,
                        data=mgDF,FUN=mean,keep.names=T,na.rm=T)
    
    
    myDF.m$totP_ppm <- as.numeric(myDF.m$totP_ppm)
    
    myDF.m$PercP <- myDF.m$totP_ppm / 10000
    
    myDF.m <- myDF.m[complete.cases(myDF.m),]

    myDF.out <- myDF.m[,c("Date", "Ring", "Depth", "PercP")]

    #p1 <- ggplot(myDF.out, aes(Date, PercP))+
    #    geom_point(aes(pch=Depth, col=Ring)); p1
    
    
    return(myDF.out)
    
}
