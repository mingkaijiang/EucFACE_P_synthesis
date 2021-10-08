prepare_pretreatment_covariate <- function (soil_bulk_density,
                                            soil_c_pool,
                                            lai_variable,
                                            soil_p_concentration,
                                            soil_p_pool,
                                            soil_p_mineralization) {
    
    ### Purpose:
    ### 1. to synthesize all variables with pre-treatment measurements
    ### 2. compare their relative values at the ring-level
    ### 3. ready for statistical test
    
    ### Notes:
    ### There are multiple variables available for testing, 
    ### ranging from P concentration, P pools and fluxes,
    ### to C-related variables, e.g. LAI, Soil C.
    ### Also, it's important to bear in mind the vegetation and soil perspective,
    ### and the fact that C budget used LAI.
    ### Additionally, soil depth information needs to be incorporated,
    ### so does the dates over which to obtain pre-treatment means.
    
    ###################### prepare outDF ############################
    outDF <- data.frame("Ring"=c(1:6),
                        "Trt"=c("ele","amb","amb",
                                "ele","ele","amb"))
    
    
    ###################### soil bulk density ############################
    ### This is measured in 2017 09 14, so strictly speaking, it is not
    ### pre-treatment measurement. 
    ### Any variable that uses this value should not be used for pre-treatment
    ### calculations.
    ### But this is useful to back-calculate the p pools and fluxes to their
    ### respective concentration, which has pre-treatment measurements.
    

    ###################### LAI ############################
    ### subset the pre-treatment period
    subDF <- lai_variable[lai_variable$Date<="2013-02-06",]
    
    ### calculate ring-specific means
    sumDF <- summaryBy(lai_variable~Ring, data=subDF, 
                       FUN=mean, keep.names=T, na.rm=T)
    
    ### merge with outDF
    outDF <- merge(outDF, sumDF, by=c("Ring"))
    
    
    
    
    ###################### Soil C Pool ############################
    
    ### subset, two dates, three depths
    subDF <- soil_c_pool[soil_c_pool$Date <= "2013-02-06",]
    
    ### merge with bulk density
    subDF <- merge(subDF, soil_bulk_density, by=c("Ring", "Depth"))
    
    ### Calculate P concentration
    subDF$soil_c_concentration <- ifelse(subDF$Depth=="0_10",subDF$soil_carbon_pool/1000*100/0.1/subDF$bulk_density_kg_m3,
                                         ifelse(subDF$Depth=="10_30", subDF$soil_carbon_pool/1000*100/0.2/subDF$bulk_density_kg_m3,
                                                ifelse(subDF$Depth=="transition", subDF$soil_carbon_pool/1000*100/0.3/subDF$bulk_density_kg_m3, NA)))
    
    ### calculate means, ignore date
    sumDF <- summaryBy(soil_carbon_pool~Ring+Depth, data=subDF,
                       FUN=mean, keep.names=T, na.rm=T)
    
    ### top 10 cm layer
    sumDF1 <- sumDF[sumDF$Depth=="0_10",]
    sumDF1$Depth <- NULL
    names(sumDF1)[names(sumDF1)=="soil_carbon_pool"] <- "soil_carbon_pool_0_10"
    
    ### total
    sumDF2 <- summaryBy(soil_carbon_pool~Ring, data=sumDF,
                        FUN=sum, keep.names=T, na.rm=T)
    names(sumDF2)[names(sumDF2)=="soil_carbon_pool"] <- "soil_carbon_pool_0_60"
    
    ### select concentration over top 10 cm
    tmpDF1 <- subDF[subDF$Depth=="0_10",]
    sumDF3 <- summaryBy(soil_c_concentration~Ring, data=tmpDF1,
                        FUN=mean, na.rm=T, keep.names=T)
    names(sumDF3)[names(sumDF3)=="soil_c_concentration"] <- "soil_c_concentration_0_10"
    
    
    ### merge
    outDF <- merge(outDF, sumDF1, by=c("Ring"))
    outDF <- merge(outDF, sumDF2, by=c("Ring"))
    outDF <- merge(outDF, sumDF3, by=c("Ring"))
    
    
    
    ###################### Soil P Pool ############################
    
    ### subset, only two dates, and two depth - 0 - 10, 10-30
    subDF <- soil_p_pool[soil_p_pool$Date <= "2013-02-06",]
    
    ### merge with bulk density
    subDF <- merge(subDF, soil_bulk_density, by=c("Ring", "Depth"))
    
    ### Calculate P concentration
    subDF$soil_p_concentration <- ifelse(subDF$Depth=="0_10",subDF$soil_p_g_m2/1000*100/0.1/subDF$bulk_density_kg_m3,
                                         ifelse(subDF$Depth=="10_30", subDF$soil_p_g_m2/1000*100/0.2/subDF$bulk_density_kg_m3,
                                                ifelse(subDF$Depth=="transition", subDF$soil_p_g_m2/1000*100/0.3/subDF$bulk_density_kg_m3, NA)))
    
    ### calculate means, ignore date
    sumDF <- summaryBy(soil_p_g_m2~Ring+Depth, data=subDF,
                        FUN=mean, keep.names=T, na.rm=T)
    
    ### top 10 cm layer
    sumDF1 <- sumDF[sumDF$Depth=="0_10",]
    sumDF1$Depth <- NULL
    names(sumDF1)[names(sumDF1)=="soil_p_g_m2"] <- "soil_p_g_m2_0_10"
    
    ### total
    sumDF2 <- summaryBy(soil_p_g_m2~Ring, data=sumDF,
                        FUN=sum, keep.names=T, na.rm=T)
    names(sumDF2)[names(sumDF2)=="soil_p_g_m2"] <- "soil_p_g_m2_0_30"
    
    ### select concentration over top 10 cm
    tmpDF1 <- subDF[subDF$Depth=="0_10",]
    sumDF3 <- summaryBy(soil_p_concentration~Ring, data=tmpDF1,
                        FUN=mean, na.rm=T, keep.names=T)
    names(sumDF3)[names(sumDF3)=="soil_p_concentration"] <- "soil_p_concentration_0_10"
    
        
    ### merge
    outDF <- merge(outDF, sumDF1, by=c("Ring"))
    outDF <- merge(outDF, sumDF2, by=c("Ring"))
    outDF <- merge(outDF, sumDF3, by=c("Ring"))
    
    
    
    
    
    ###################### Return ############################
    return(outDF)
    
    ### End
}
