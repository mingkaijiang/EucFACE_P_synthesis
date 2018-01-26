download_lma_data <- function(){
    
    infile <- "FACE_P0020_RA_LMA_20150129-20150416_L2.csv"
    
    if(!file.exists(paste0("download/", infile))) {
        downloadCSV(infile)
    }
}


download_lai_variable <- function(){
    
    downloadTOA5("FACE_P0037_RA_GAPFRACLAI_OPEN_L2.dat", quiet=TRUE)
    
}


