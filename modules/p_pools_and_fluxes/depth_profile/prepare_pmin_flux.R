prepare_pmin_flux <- function() {
    
    # download the data
    download_soil_p_mineralization_data()
    
    ## read in data - extractable NP data
    myDF1 <- read.csv(file.path(getToPath(), 
                                "FACE_RA_P0023_SOILMINERALISATION_L3_20120724-20140124.csv"))
    
    myDF2 <- read.csv(file.path(getToPath(), 
                                "FACE_RA_P0023_SOILMINERALISATION_L1_20140428-20160121.csv"))
    
    myDF1 <- myDF1[,c("date", "ring", "nitrification", "N_mineralisation", "P_mineralisation")]
    myDF2 <- myDF2[,c("date", "ring", "nitrification", "N_mineralisation", "P_mineralisation")]
    
    myDF1$date <- as.Date(as.character(myDF1$date), format="%Y-%m-%d")
    myDF2$date <- as.Date(as.character(myDF2$date), format="%d/%m/%Y")
    
    myDF <- rbind(myDF1, myDF2)
    
    ## 
    outDF <- myDF[,c("date", "ring", "P_mineralisation")]
    colnames(outDF) <- c("Date", "Ring", "Pmin")
    
    return(outDF)
    
}