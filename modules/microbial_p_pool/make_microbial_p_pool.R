#- Make the microbial P pool
make_microbial_p_pool <- function(bk_density){
    # return ring-specific, continuous microbial P pool

    # download the data
    download_microbial_p_data()
    
    df <- read.csv(file.path(getToPath(), 
                             "FACE_P0014_RA_MicrobialBiomassCNP_L1_20120613-20151130.csv"))

    # average across rings and depths, unit: mg/kg
    df.m <- summaryBy(Pmic~ring+date,
                      data=df,FUN=mean,keep.names=T,na.rm=T)
    
    
    # obtain ring averaged soil bulk density (0 - 30 cm only)
    bk_density <- subset(bk_density, Depth != "30-42")
    bk_density <- subset(bk_density, Depth != "30-44")
    bk_density <- subset(bk_density, Depth != "30-55")
    bk_density <- subset(bk_density, Depth != "30-60")
    bk_density <- subset(bk_density, Depth != "60-88")
    
    bk.r<-with(bk_density, tapply(bulk_density_kg_m3, ring, mean))
    
    for (i in 1:6) {
        df.m[df.m$ring == i, "bk_density"] <- bk.r[i]
    }
        
    # unit conversion: mg/kg to g/m2
    df.m$Pmic_g_m2 <- df.m$bk_density * df.m$Pmic * 0.3 / g_to_mg
    
    # update variables to output Pmic in unit g m-2
    df.out <- df.m[,c("ring", "date", "Pmic_g_m2")]
    
    df.out <- df.out[complete.cases(df.out),]
    
    return(df.out)
    
}
