#- Make the soil P content pool
make_soil_p_content <- function(bk_density){
    # return ring-specific, time series data of soil P content 
    # need to read in multiple P data sources
    # and soil bulk density data
    # bk_density is the ring-specific soil density data (kg/m3) across depths
    
    # download the data
    download_soil_p_data()
    
#    ## read in the csv - elemental analysis data
#    myDF1 <- read.csv(file.path(getToPath(), 
#                                "FACE_P0014_ALL_ ElementalAnalysis_2012to2014_V2.csv"))
#    myDF1 <- data.frame(lapply(myDF1, trimws), stringsAsFactors = FALSE)
#    
#    # Updating date and variable names
#    myDF1$Date <- as.Date(myDF1$Date)
#    names(myDF1)[30] <- "Phosphorus_mg_kg"
#    myDF1$Phosphorus_mg_kg <- as.numeric(myDF1$Phosphorus_mg_kg)
#    
#    # average across rings, dates, and depths, unit: mg/kg
#    myDF1.m <- summaryBy(Phosphorus_mg_kg~Date+Ring+Depth,data=myDF1,FUN=mean,keep.names=T,na.rm=T)
#    
#    # assign bulk density onto each ring and each depth
#    for (i in 1:6) {
#        for (j in unique(myDF1.m$Depth)) {
#            myDF1.m[myDF1.m$Ring == i & myDF1.m$Depth == j, "bulk_density_kg_m3"] <- 
#                bk_density[bk_density$ring == i & bk_density$Depth == j, "bulk_density_kg_m3"] 
#        }
#    }
#    
#    # calculate total P in each soil depth (hence the * 0.1), unit mg m-2
#    myDF1.m$Phosphorus_mg_m2 <- myDF1.m$Phosphorus_mg_kg * myDF1.m$bulk_density_kg_m3 * 0.1
    
    
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
    myDF.m <- summaryBy(totP_ppm~Date+ring+depth,data=myDF,FUN=mean,keep.names=T,na.rm=T)
    
    myDF.m <- data.frame(lapply(myDF.m, trimws), stringsAsFactors = FALSE)
    
    # assign bulk density onto each ring and each depth
    for (i in 1:6) {
        for (j in unique(myDF.m$depth)) {
            myDF.m[myDF.m$ring == i & myDF.m$depth == j, "bulk_density_kg_m3"] <- 
                bk_density[bk_density$ring == i & bk_density$Depth == j, "bulk_density_kg_m3"] 
        }
    }
    
    # calculate total P in each soil depth (hence the * 0.1), unit mg m-2
    myDF.m$totP_mg_m2 <- as.numeric(myDF.m$totP_ppm) * myDF.m$bulk_density_kg_m3 * 0.1
    
    # return in unit of g/m2
    myDF.m$totP_g_m2 <-myDF.m$totP_mg_m2 / 1000.0
    
    myDF.out <- myDF.m[,c("Date", "ring", "depth", "totP_g_m2")]

    return(myDF.out)
    
}
