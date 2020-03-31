make_normalized_total_p_budget <- function() {
    #### This function calculates all P budgeting variables
    ### out df
    terms <- c("Total plant P stock", 
               "Total plant P requirement flux", 
               "Total plant P retranslocation flux", 
               "Plant P uptake flux", 
               "Soil P mineralization flux",
               "Labile Pi stock",
               "Plant P uptake over requirement",
               "Plant P MRT", 
               "Plant PUE",
               "Overstorey aboveground P stock",
               "Understorey aboveground P stock",
               "Belowground P stock")
    
    out <- data.frame(terms, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(out) <- c("terms", "R1", "R2", "R3", "R4", "R5", "R6", "aCO2", "eCO2")
    
    ### assign values
    out[out$terms == "Total plant P stock", 2:7] <- round(vegetation_standing_p_stock_norm$total,2)
    
    out[out$terms == "Total plant P requirement flux", 2:7] <- round(total_plant_p_fluxes_norm$Total_plant_P_requirement_flux,2)
    
    out[out$terms == "Total plant P retranslocation flux", 2:7] <- round(total_plant_p_fluxes_norm$Total_plant_retranslocation_P_flux,2)
    
    out[out$terms == "Plant P uptake flux", 2:7] <- round(total_plant_p_fluxes_norm$Total_plant_uptake_P_flux,2)
    
    out[out$terms == "Soil P mineralization flux", 2:7] <- round(summary_table_flux_norm[summary_table_flux_norm$terms=="Mineralization P flux",2:7],2)
    
    out[out$terms == "Plant P uptake over requirement", 2:7] <- round(total_plant_p_fluxes_norm$Total_uptake_over_requirement_ratio,2)
    
    out[out$terms == "Plant P MRT", 2:7] <- round(plant_p_MRT_norm$plant_P_MRT,2)
    
    out[out$terms == "Plant PUE", 2:7] <- round(plant_p_use_efficiency_norm$PUE,2)
    
    out[out$terms == "Overstorey aboveground P stock", 2:7] <- round(vegetation_standing_p_stock_norm$oa,2)
    
    out[out$terms == "Understorey aboveground P stock", 2:7] <- round(vegetation_standing_p_stock_norm$understorey,2)
    
    out[out$terms == "Belowground P stock", 2:7] <- round(vegetation_standing_p_stock_norm$belowground,2)
    
    out[out$terms == "Labile Pi stock", 2:7] <- round(summary_table_pool_norm[summary_table_pool_norm$terms=="Exchangeable Pi Pool", 2:7],2)
    
    
    
    ### aCO2 and eCO2 averages
    out$aCO2 <- round(rowMeans(data.frame(out$R2, out$R3, out$R6)), 4)
    out$eCO2 <- round(rowMeans(data.frame(out$R1, out$R4, out$R5)) , 4)
    
    
    ### sd
    out$aCO2_sd <- rowSds(as.matrix(subset(out, select=c(R2, R3, R6))), na.rm=T)
    out$eCO2_sd <- rowSds(as.matrix(subset(out, select=c(R1, R4, R5))), na.rm=T)
    
    ### save
    write.csv(out, "plots_tables/total_p_budget_normalized.csv", row.names=F)
    
    
    return(out)
    
}