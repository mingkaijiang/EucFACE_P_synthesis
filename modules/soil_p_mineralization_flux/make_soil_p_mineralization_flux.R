
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
    pdf("plots_tables/soil_p_mineralization_over_time.pdf")
    
    # compare eCO2 and aCO2 treatment
    myDF1.out[myDF1.out$ring == 1, "CO2"] <- "eCO2"
    myDF1.out[myDF1.out$ring == 4, "CO2"] <- "eCO2"
    myDF1.out[myDF1.out$ring == 5, "CO2"] <- "eCO2"
    
    myDF1.out[myDF1.out$ring == 2, "CO2"] <- "aCO2"
    myDF1.out[myDF1.out$ring == 3, "CO2"] <- "aCO2"
    myDF1.out[myDF1.out$ring == 6, "CO2"] <- "aCO2"
    
    
    p <- ggplot(myDF1.out, aes(date, p_mineralization_g_m2_d, color=factor(CO2))) +   
        geom_point(size = 5) +
        xlab("Date") + ylab("P mineralization rate (g m-2 d-1)") + 
        ggtitle("P mineralization rate over time") 
    plot(p)
    dev.off()
    
    return(myDF1.out[,1:3])
}