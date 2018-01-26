#- Make the fineroot c production flux
make_fineroot_c_production_flux <- function(c_fraction){
    
    # returns fine root production flux (mg m-2 d-1)
    # 5 time points, possibly measured at quarterly timesteps
    # start and end dates not included yet
    
    infile <- "FACE_P0083_RA_FR-PRODUCTION_L1_20140601-20150915.csv"
    
    #- download the data
    if(!file.exists(paste0("download/FACE_P0083_RA_FR-PRODUCTION_L1_20140601-20150915.csv"))) {
        download_fineroot_data()
    }

    #- read in the csv
    frp1 <- read.csv(file.path(getToPath(), 
                               infile))
    
    # Fix Date format:
    # Assume that e.g. 'Jan-13' means the last day of that month (2013-01-31).
    frp1$Date <- as.Date(paste0("1-", frp1$Date), format = "%d-%b-%y") + months(1) - days(1)
    
    names(frp1)[2] <- "Ring"
    names(frp1)[8] <- "frp_tot"
    
    #- average across rings and dates
    frp.m <- summaryBy(frp_tot~Date+Ring,data=frp1,FUN=mean,keep.names=T,na.rm=T)
    
    #- convert to mg C m-2 day-1
    frp.m$fineroot_production_flux <- frp.m$frp_tot*c_fraction*1000
    
    #- format dataframe to return
    frp.out <- frp.m[,c("Date","Ring","fineroot_production_flux")]
    return(frp.out)
    
}