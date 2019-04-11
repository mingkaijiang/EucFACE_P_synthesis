#- Make the fine root C pool
make_fineroot_c_pool <- function(c_fraction){
    
    #- download the data
    download_fineroot_c_data()
    
    #- read in the csv
    frb1 <- read.csv(file.path(getToPath(), 
                               "FACE_P0083_RA_FR-BIOMASS_L1_20140201-20150915.csv"))
    frb1$Date <- as.Date(frb1$Date)
    names(frb1)[2] <- "Ring"
    names(frb1)[8] <- "frb_tot"
    
    #- average across rings and dates
    frb.m <- summaryBy(frb_tot~Date+Ring,data=frb1,FUN=mean,keep.names=T)
    
    #- convert to g C m-2. Use fine-root specific c_fraction from Juan.
    frb.m$fineroot_pool <- frb.m$frb_tot*c_fraction_fr
    
    #- format dataframe to return
    frb.out <- frb.m[,c("Date","Ring","fineroot_pool")]
    
    # read in 2017 aug/sep data
    frb2 <- read.csv("temp_files/MASTER_root_biomass_P0091.csv")
    frb2.m <- summaryBy(root.smal.2.mm.mg.g~ring+depth, data=frb2, 
                        FUN=mean, keep.names=T, na.rm=T)
    
    frb2.sub1 <- subset(frb2.m, depth=="0-10")
    frb2.sub2 <- subset(frb2.m, depth=="10-30")
    
    bk1 <- subset(soil_bulk_density, Depth == "0-10cm")
    bk2 <- subset(soil_bulk_density, Depth %in%c("10-20cm", "20-30cm"))
    bk2.s <- summaryBy(bulk_density_kg_m3~ring, data=bk2, FUN=mean, keep.names=T)
    
    for (i in 1:6) {
        frb2.sub1[frb2.sub1$ring==i, "rootmass"] <- frb2.sub1[frb2.sub1$ring==i,"root.smal.2.mm.mg.g"] * bk1[bk1$ring==i, "bulk_density_kg_m3"]
        frb2.sub2[frb2.sub1$ring==i, "rootmass"] <- frb2.sub2[frb2.sub2$ring==i,"root.smal.2.mm.mg.g"] * bk2.s[bk2.s$ring==i, "bulk_density_kg_m3"]
        
    }
    
    frb2.sub1$rootc <- frb2.sub1$rootmass * 0.1 * c_fraction_fr
    frb2.sub2$rootc <- frb2.sub2$rootmass * 0.2 * c_fraction_fr
    
    for (i in 1:6) {
        frb2.sub1[frb2.sub1$ring==i, "fineroot_pool"] <- frb2.sub1[frb2.sub1$ring==i, "rootc"] + frb2.sub2[frb2.sub2$ring==i, "rootc"]
    }
    
    frb2.out <- frb2.sub1[,c("ring", "fineroot_pool")]
    colnames(frb2.out) <- c("Ring", "fineroot_pool")
    frb2.out$Date <- "2017-09-01"
    frb2.out <- frb2.out[,c("Date", "Ring", "fineroot_pool")]
    
    frb.out <- rbind(frb.out, frb2.out)
    
    return(frb.out)
}