make_china_p_budget <- function(return.unit) {
    
    ### Unit: all soil P (except last one) in unit of g P kg-1 soil
    ###       Their unit must not be right. I think the units are in t ha-1,
    ###       because otherwise soil P in 50 - 100 cm depth shouldn't be that high.
    
    
    ### read
    inDF <- read.csv("temp_files/P_data_China.csv")
    
    names(inDF)[names(inDF)=="Longtitude"] <- "Longitude"
    
    ### as numeric
    inDF$litterP_t_ha <- gsub("-", "", inDF$litterP_t_ha)
    inDF$litterP_t_ha <- as.numeric(inDF$litterP_t_ha)
    
    ### unit conversion
    inDF$plantP_g_m2 <- inDF$plantP_t_ha * 100.0
    inDF$litterP_g_m2 <- inDF$litterP_t_ha * 100.0
    
    
    ### subset broadleaf
    inDF <- subset(inDF, Leaf_Form=="broadleaf")
    
    ### extract by plant P size
    inDF <- subset(inDF, plantP_g_m2 <= 1 & plantP_g_m2 >= 0.5)
    
    ### extract by soil P size
    inDF <- subset(inDF, soilP_0_10cm <= 0.1)
    
    
    ### this is to confirm that the units should be in t ha. 
    #inDF$test <- with(inDF, soilP_0_10cm+soilP_10_20cm+soilP_20_30cm+soilP_30_50cm+soilP_50_100cm)
    
    ### regroup
    inDF$soilP_50_60cm <- inDF$soilP_50_100cm / 0.5 / 1000 * 100
    inDF$soilP_30_40cm <- inDF$soilP_30_50cm / 0.2 / 1000 * 100
    inDF$soilP_40_50cm <- inDF$soilP_30_50cm / 0.2 / 1000 * 100
    
    
    inDF$soilP_30_60cm <- (inDF$soilP_30_40cm + inDF$soilP_40_50cm + inDF$soilP_50_60cm)/3
    inDF$soilP_10_30cm <- (inDF$soilP_10_20cm + inDF$soilP_20_30cm)/2
    
    ### select
    inDF <- inDF[,c("ID", "Latitude", "Longitude", "Biome", "Leaf_Form",
                    "soilP_0_10cm", "soilP_10_30cm", "soilP_30_60cm", 
                    "plantP_g_m2", "litterP_g_m2")]
    
    plotDF <- reshape2::melt(inDF, id.vars=c("ID", "Latitude", "Longitude", "Biome", "Leaf_Form"))
    
    sumDF <- summaryBy(value~variable, data=plotDF, na.rm=T, 
                       keep.names=T, FUN=c(mean,sd))
    
    
    if (return.unit == "concentration") {
        ### final units
        ### soil P: g kg-1
        ### plant: g m-2
        
        outDF <- sumDF[sumDF$variable%in%c("soilP_0_10cm",
                                           "soilP_10_30cm",
                                           "soilP_30_60cm"),]
    } else if (return.unit == "g_m2") {
        
        subDF1 <- plotDF[plotDF$variable%in%c("soilP_0_10cm",
                                            "soilP_10_30cm",
                                            "soilP_30_60cm"),]
        
        subDF2 <- plotDF[plotDF$variable%in%c("plantP_g_m2",
                                              "litterP_g_m2"),]
        
        subDF1$value2 <- ifelse(subDF1$variable=="soilP_0_10cm", subDF1$value * 100, 
                                ifelse(subDF1$variable=="soilP_10_30cm", subDF1$value * 200,
                                       ifelse(subDF1$variable=="soilP_30_60cm", subDF1$value * 300, NA)))
        
        subDF1$value <- NULL
        names(subDF1)[names(subDF1)=="value2"] <- "value"
        
        outDF1 <- rbind(subDF1, subDF2)
        
        outDF <- summaryBy(value~variable, data=outDF1, na.rm=T, 
                           keep.names=T, FUN=c(mean,sd))
    }
    
   return(outDF)
    
}
