
make_soil_p_mineralization_flux <- function(bk_density) {
    # download the data
    download_soil_p_mineralization_data()
    
    ## read in data - extractable NP data
    myDF1 <- read.csv(file.path(getToPath(), 
                                "FACE_RA_P0023_SOILMINERALISATION_L3_20120724-20140124.csv"))
    
    
    
    # average across rings, dates, and depths, unit: mg/kg PO4
    myDF1.m <- summaryBy(P_mineralisation~date+ring,data=myDF1,FUN=mean,keep.names=T,na.rm=T)
    

    # obtain ring averaged soil bulk density (0 - 10 cm only)
    bk_density <- subset(bk_density, Depth == "0-10cm")
    
    # add bulk density
    for (i in 1:6){
        myDF1.m[myDF1.m$ring == i, "bk_density"] <- bk_density[bk_density$ring == i, "bulk_density_kg_m3"] 
    }
    
    myDF1.m$p_mineralization_g_m2_d <- myDF1.m$P_mineralisation * myDF1.m$bk_density * 0.1 / g_to_mg

    # output table
    myDF1.out <- myDF1.m[,c("date", "ring", "p_mineralization_g_m2_d")]
    
    # plotting time series
    #with(myDF1.out, plot(p_mineralization_g_m2_d~date, col=ring,
    #                     ylim=c(-0.01, 0.01)))
    #legend("topright", col=rainbow(6), legend=c(1:6), pch = 1)

    #require(lattice)
    #xyplot(p_mineralization_g_m2_d ~ date, group=ring, data=myDF1.out, 
    #       auto.key=list(space="right"), 
    #       jitter.x=TRUE, jitter.y=TRUE)

    return(myDF1.out)
}