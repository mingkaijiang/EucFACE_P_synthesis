make_plant_P_mean_residence_time <- function(norm, p_stand, p_flux) {
    
    
    ### remove coarse woody component and litter
    p_stand$tot_reduced <- with(p_stand, leaf+fineroot+understorey+sapwood)
    
    
    tmp1 <- data.frame("Ring" = p_stand$Ring, "Trt" = p_stand$Trt, 
                      "Total_standing_P_stock" = p_stand$tot_reduced)
    
    tmp2 <- as.numeric(p_flux[p_flux$terms=="Total vegetation uptake P flux",2:7])
    
    out <- data.frame(tmp1, 
                      "Total_plant_P_uptake_flux"=tmp2)
    
    out$plant_P_MRT <- with(out, Total_standing_P_stock / Total_plant_P_uptake_flux)
    
    return(out)
}