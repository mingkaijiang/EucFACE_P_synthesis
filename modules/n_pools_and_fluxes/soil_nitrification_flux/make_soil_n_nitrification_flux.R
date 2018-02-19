
make_soil_n_nitrification_flux <- function(bk_density) {
    
    ## read in data - extractable NP data
    myDF1 <- read.csv(file.path(getToPath(), 
                                "FACE_RA_P0023_SOILMINERALISATION_L3_20120724-20140124.csv"))
    
    # average across rings, dates, and depths, unit: mg/kg/d 
    myDF1.m <- summaryBy(nitrification~date+ring,data=myDF1,FUN=mean,keep.names=T,na.rm=T)
    

    # obtain ring averaged soil bulk density (0 - 10 cm only)
    bk_density <- subset(bk_density, Depth == "0-10cm")
    
    # add bulk density
    for (i in 1:6){
        myDF1.m[myDF1.m$ring == i, "bk_density"] <- bk_density[bk_density$ring == i, "bulk_density_kg_m3"] 
    }
    
    # from mg kg-1 d-1 to mg m-2 d-1
    myDF1.m$soil_n_nitrification <- myDF1.m$nitrification * myDF1.m$bk_density * 0.1 

    # output table
    myDF1.out <- myDF1.m[,c("date", "ring", "soil_n_nitrification")]
    colnames(myDF1.out) <- c("Date", "Ring", "soil_n_nitrification_flux")
    
    return(myDF1.out)
}