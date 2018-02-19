download_soil_inorganic_n_data <- function() {

    downloadHIEv(hiev=searchHIEv("FACE_RA_P0010_SOILIEMNUTRIENTS_L1_20120620-20140118"))
    
    downloadHIEv(hiev=searchHIEv("FACE_RA_P0010_SOILIEMNUTRIENTS_L3_20120620-20140118"))
    
    downloadHIEv(hiev=searchHIEv("FACE_RA_P0023_SOILEXTRACTABLENUTRIENTS_L3_20120613-20140310"))
    
    downloadHIEv(hiev=searchHIEv("FACE_RA_P0023_SOILEXTRACTABLENUTRIENTS_L3_20120613-20140310"))
    
    
}