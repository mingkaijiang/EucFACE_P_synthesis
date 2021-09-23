belowground_P_working_sheet <- function (bk_density) {
    
    ### read in Pmineralization flux
    pmin <- prepare_pmin_flux()
    
    ### extract the period where the data overlap with the microbial P dataset
    pmin <- subset(pmin, Date>="2014-09-09" & Date<= "2015-10-20")
    pmin$depth <- "0_10"
    pmin <- pmin[,c("Ring", "Date", "depth", "Pmin")]
    colnames(pmin) <- c("ring", "date", "depth", "Pmin")
    pmin$Pminyear <- pmin$Pmin * 365
    
    ### prepare the microbial P dataset
    pmic <- prepare_pmic_conc()
    subDF1 <- subset(pmic, date<="2015-11-30")
    
    ### assign p flux dates
    subDF1$date2[subDF1$date=="2014-09-09"] <- "2014-10-13"
    subDF1$date2[subDF1$date=="2015-03-10"] <- "2015-01-12"
    subDF1$date2[subDF1$date=="2015-06-09"] <- "2015-04-24"
    subDF1$date2[subDF1$date=="2015-09-09"] <- "2015-07-25"
    subDF1$date2[subDF1$date=="2015-11-30"] <- "2015-10-20"
    
    subDF1$date2 <- as.Date(subDF1$date2)
    
    ### merge the two
    mgDF <- merge(subDF1, pmin, by.x=c("ring", "date2", "depth"),
                  by.y=c("ring", "date", "depth"))
    
    ### test for linear relationship
    t1 <- lm(Pmin~Pmic, data=mgDF)
    summary(t1)
    
    #t2 <- lm(Pmin~Pmic, data=mgDF[mgDF$ring==6,])
    #summary(t2)
    
    p1 <- ggplot(mgDF, aes(Pmic, Pmin, group=as.character(ring)))+
        geom_point(aes(col=as.character(ring)))+
        geom_smooth(method="lm"); p1
    
    ### prepare the linear fit coefficients
    lm.int <- coef(t1)[1]
    lm.slp <- coef(t1)[2]
    
    ### fit all data to predict Pmin flux
    pmic$Pmin <- with(pmic, Pmic*lm.slp + lm.int)
    
    ### add bulk density, but first convert 
    bkDF <- read.csv("temp_files/bulk_density.csv")
    bkDF$ring <- gsub("R", "", bkDF$ring)
    bkDF$ring <- as.numeric(bkDF$ring)
    
    bkDF$depth <- gsub("0-10cm", "0_10", bkDF$depth)
    bkDF$depth <- gsub("10-30cm", "10_30", bkDF$depth)
    
    ## merge
    pmic <- merge(pmic, bkDF, by=c("ring", "depth"))
    
    ## convert unit for bulk density, from g cm3 to kg m-3
    pmic$bulk_density <- pmic$bulk_density * 1000
    
    ### convert unit, from mg kg-1 d-1 to mg m-2 d-1
    depth.list <- c("0_10", "10_30", "transition")
    
    pmic$Pmin_mg_m2_d <- ifelse(pmic$depth=="0_10", pmic$Pmin * pmic$bulk_density * 0.1,
                                ifelse(pmic$depth=="10_30", pmic$Pmin * pmic$bulk_density * 0.2,
                                       ifelse(pmic$depth=="transition", pmic$Pmin * pmic$bulk_density * 0.3, NA)))
    
    
    
    ### calculate total across depth and date
    outDF <- summaryBy(Pmin+Pmin_mg_m2_d~ring+depth, FUN=mean,
                       data=pmic, na.rm=T, keep.names=T)
    
    outDF$Pmin_mg_m2_yr <- with(outDF, Pmin_mg_m2_d * 365)
    
    outDF2 <- summaryBy(Pmin_mg_m2_yr~ring, FUN=sum,
                        data=outDF, na.rm=T, keep.names=T)
    
    ### end
    return(outDF2)
    
    
}
