#- Make the soil P pools of different bioavailability
make_soil_p_pool_hedley <- function(p_conc, bk_density){
    # p_conc: soil p concentration of different bioavailability
    # bk_density: ring-specific soil density data (kg/m3) across depths
    
    ### averaging bulk density across depths
    #bk <- summaryBy(bulk_density_kg_m3~ring, data=bk_density, FUN=mean,
    #                keep.names=T, na.rm=T)
    bk <- subset(bk_density, Depth == "0-10cm")
    
    p_pool <- p_conc
    
    # assign bulk density onto each ring and each depth
    for (i in 1:6) {
        p_conc[p_conc$Ring == i, "bk_kg_m3"] <- bk[bk$ring == i, "bulk_density_kg_m3"] 
    }
    
    # calculate each P pool in top 10 cm of soil (hence the * 0.1), unit kg m-2
    p_pool$F1_2_Pi_Exchangeable <- p_conc$F1_2_Pi_Exchangeable * p_conc$bk_kg_m3 * 0.1 / 100
    p_pool$F1_2_Po_Exchangeable <- p_conc$F1_2_Po_Exchangeable * p_conc$bk_kg_m3 * 0.1 / 100
    p_pool$F3_Po_Moderately_labile <- p_conc$F3_Po_Moderately_labile * p_conc$bk_kg_m3 * 0.1 / 100
    p_pool$F3_Fe_bound_P_Secondary_mineral <- p_conc$F3_Fe_bound_P_Secondary_mineral * p_conc$bk_kg_m3 * 0.1 / 100
    p_pool$F4_Ca_bound_Primary_Mineral <- p_conc$F4_Ca_bound_Primary_Mineral * p_conc$bk_kg_m3 * 0.1 / 100
    p_pool$F5_6_Occluded <- p_conc$F5_6_Occluded * p_conc$bk_kg_m3 * 0.1 / 100
    p_pool$Total_Aqua_Regia_P <- p_conc$Total_Aqua_Regia_P * p_conc$bk_kg_m3 * 0.1 / 100

    # return in unit of g/m2
    p_pool$F1_2_Pi_Exchangeable <- p_pool$F1_2_Pi_Exchangeable * 1000.0
    p_pool$F1_2_Po_Exchangeable <- p_pool$F1_2_Po_Exchangeable * 1000.0
    p_pool$F3_Po_Moderately_labile <- p_pool$F3_Po_Moderately_labile * 1000.0
    p_pool$F3_Fe_bound_P_Secondary_mineral <- p_pool$F3_Fe_bound_P_Secondary_mineral * 1000.0
    p_pool$F4_Ca_bound_Primary_Mineral <- p_pool$F4_Ca_bound_Primary_Mineral * 1000.0
    p_pool$F5_6_Occluded <- p_pool$F5_6_Occluded * 1000.0
    p_pool$Total_Aqua_Regia_P <- p_pool$Total_Aqua_Regia_P * 1000.0

    return(p_pool)
    
}
