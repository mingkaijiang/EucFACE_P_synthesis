make_plant_P_mean_residence_time <- function(p_stand, p_flux) {
    
    
    out <- data.frame(p_stand$Ring, p_stand$Trt, p_stand$total, p_flux$Total_plant_uptake_P_flux)
    colnames(out) <- c("Ring", "Trt", "Total_standing_P_stock", "Total_plant_P_uptake_flux")
    
    out$plant_P_MRT <- with(out, Total_standing_P_stock / Total_plant_P_uptake_flux)
    
    return(out)
}