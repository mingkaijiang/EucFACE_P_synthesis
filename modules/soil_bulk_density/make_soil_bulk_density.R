#- Make the soil bulk density variable
make_soil_bulk_density <- function(){
    # return ring-specific soil density data

    # download the data
    download_soil_bulk_density_data()
    
    df <- read.xls(file.path(getToPath(), 
                            "FACE_RA_P0088_BULKDENSITY_L1_20170914.xlsx"), 
                  sheet = 1, header = TRUE)

    df <- df[,1:7]
    names(df)[7] <- "bulk_density"
    df$bulk_density <- as.numeric(as.character(df$bulk_density))
    
    # average across rings and depths, unit: g/cm3
    df.m <- summaryBy(bulk_density~ring+Depth,
                      data=df,FUN=mean,keep.names=T,na.rm=T)
    
    df.m$bulk_density_kg_m3 <- df.m$bulk_density * g_to_kg / cm3_to_m3
    
    df.out <- df.m[,c("ring", "Depth", "bulk_density_kg_m3")]
    
    return(df.out)
    
}
