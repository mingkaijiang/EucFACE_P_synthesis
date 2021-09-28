make_soil_organic_p_concentration <- function(){
    # return ring-specific, time series data of soil P content 
    # need to read in multiple P data sources
    # and soil bulk density data

    
    # read in the Oct 2018 Johanna data at deeper depths
    tmpDF <- read.csv("temp_files/belowground_P_working_sheet.csv")
    
    tmpDF <- tmpDF[,c("Date", "Ring", "Depth", "OrgP")]
    colnames(tmpDF) <- c("Date", "Ring", "Depth", "OrgP_ppm")
    tmpDF$Date <- as.Date(tmpDF$Date, format="%d/%m/%y")
    
    
    # average across depths first, unit: ppm which is mg/kg
    myDF.m <- summaryBy(OrgP_ppm~Date+Ring+Depth,
                        data=tmpDF,FUN=mean,keep.names=T,na.rm=T)
    
    
    myDF.m$OrgP_ppm <- as.numeric(myDF.m$OrgP_ppm)
    
    myDF.m$PercP <- myDF.m$OrgP_ppm / 10000
    
    myDF.m <- myDF.m[complete.cases(myDF.m),]

    myDF.out <- myDF.m[,c("Date", "Ring", "Depth", "PercP")]

    
    return(myDF.out)
    
}
