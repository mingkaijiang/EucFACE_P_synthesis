#- Make the soil P content pool
make_soil_inorganic_p_pool <- function(p_conc, bk_density){
    # return ring-specific, time series data of soil P content 
    # p_conc: soil p concentration variable
    # bk_density: ring-specific soil density data (kg/m3) across depths
    
    ### merge
    p_conc <- merge(p_conc, bk_density, by=c("Ring", "Depth"))
    
    
    # calculate total P in top 30cm of soil (hence the * 0.1), unit kg m-2
    p_conc$soil_p_kg_m2 <- ifelse(p_conc$Depth=="0_10", p_conc$PercP * p_conc$bulk_density_kg_m3 * 0.1 / 100, 
                                  ifelse(p_conc$Depth=="10_30", p_conc$PercP * p_conc$bulk_density_kg_m3 * 0.2 / 100,
                                         ifelse(p_conc$Depth=="transition", p_conc$PercP * p_conc$bulk_density_kg_m3 * 0.3 / 100, NA)))
    
    # return in unit of g/m2
    p_conc$soil_inorganic_p_g_m2 <-p_conc$soil_p_kg_m2 * 1000.0
    
    myDF.out <- p_conc[,c("Date", "Ring", "Depth", "soil_inorganic_p_g_m2")]
    
    myDF.out <- myDF.out[complete.cases(myDF.out),]

    return(myDF.out)
    
}
