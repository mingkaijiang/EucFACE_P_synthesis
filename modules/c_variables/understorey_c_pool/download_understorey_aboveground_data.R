download_understorey_aboveground_data <- function() {
    
    infile1 <- "FACE_P0061_RA_PATHARE_UNDERSTORY_ABOVEGROUND_BIOMASS_L2_20150201_20160730.csv"
    
    # Varsha's harvest data
    if (!file.exists(paste0("download/", infile1))) {
        downloadHIEv(hiev=searchHIEv(infile1))
        
    }

    # Matthias's harvest data
    #downloadHIEv(hiev=searchHIEv(".csv"))
}
