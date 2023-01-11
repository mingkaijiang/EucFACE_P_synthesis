make_total_p_budget <- function(norm, 
                                summary_table_flux,
                                summary_table_pool,
                                vegetation_standing_p_stock,
                                plant_p_MRT,
                                plant_p_use_efficiency,
                                plant_GPP_efficiency) {
    
    
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
               "Microbe P MRT",
               "Microbe P MRT 0-10cm",
               "Microbe P MRT 10-30cm",
               "Microbe P MRT 30-60cm",
               "Overstorey GPP efficiency",
               "Understorey GPP efficiency",
               "Plant P uptake over P mineralization",
               "Leaflitter P over P mineralization",
               "Fineroot litter P over P mineralization",
               "Twig litter P over P mineralization",
               "Bark litter P over P mineralization",
               "Seed litter P over P mineralization",
               "Frass litter P over P mineralization",
               "Understorey litter P over P mineralization",
               "Leaching P over P mineralization",
               "Overstorey aboveground P stock",
               "Understorey aboveground P stock",
               "Belowground P stock",
               "Dead P stock")
    
    out <- data.frame(terms, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(out) <- c("terms", "R1", "R2", "R3", "R4", "R5", "R6", "aCO2", "eCO2")
    
    ### assign values
    out[out$terms == "Total plant P stock", 2:7] <- round(as.numeric(vegetation_standing_p_stock$total),2)
    
    out[out$terms == "Total plant P requirement flux", 2:7] <- round(as.numeric(summary_table_flux[summary_table_flux$terms=="Total vegetation production P flux", 2:7]),2)
    
    out[out$terms == "Total plant P retranslocation flux", 2:7] <- round(as.numeric(summary_table_flux[summary_table_flux$terms=="Total vegetation retranslocation P flux", 2:7]),2)
    
    out[out$terms == "Plant P uptake flux", 2:7] <- round(as.numeric(summary_table_flux[summary_table_flux$terms=="Total vegetation uptake P flux", 2:7]),2)
    
    out[out$terms == "Soil P mineralization flux", 2:7] <- round(as.numeric(summary_table_flux[summary_table_flux$terms=="Mineralization P flux 0-10cm",2:7]+
                                                                                summary_table_flux[summary_table_flux$terms=="Mineralization P flux 10-30cm",2:7]+
                                                                                summary_table_flux[summary_table_flux$terms=="Mineralization P flux 30-60cm",2:7]),2)
    
    out[out$terms == "Plant P uptake over requirement", 2:7] <- round(as.numeric(out[out$terms=="Plant P uptake flux", 2:7])/as.numeric(out[out$terms=="Total plant P requirement flux", 2:7]),2)
    
    out[out$terms == "Plant P MRT", 2:7] <- round(plant_p_MRT$plant_P_MRT,2)
    
    out[out$terms == "Plant PUE", 2:7] <- round(plant_p_use_efficiency$PUE,2)
    
    out[out$terms == "Plant P uptake over P mineralization", 2:7] <- round(as.numeric(out[out$terms=="Plant P uptake flux", 2:7])/as.numeric(out[out$terms=="Soil P mineralization flux", 2:7]),2)
    
    
    ### plant P uptake over P mineralization
    out[out$terms == "Leaflitter P over P mineralization", 2:7] <- round(as.numeric(summary_table_flux[summary_table_flux$terms=="Leaflitter P flux", 2:7])/as.numeric(out[out$terms=="Soil P mineralization flux", 2:7]),2)
    
    out[out$terms == "Fineroot litter P over P mineralization", 2:7] <- round(as.numeric(summary_table_flux[summary_table_flux$terms=="Fineroot Litter P flux", 2:7])/as.numeric(out[out$terms=="Soil P mineralization flux", 2:7]),2)
    
    out[out$terms == "Twig litter P over P mineralization", 2:7] <- round(as.numeric(summary_table_flux[summary_table_flux$terms=="Twig litter P flux", 2:7])/as.numeric(out[out$terms=="Soil P mineralization flux", 2:7]),2)
    
    out[out$terms == "Bark litter P over P mineralization", 2:7] <- round(as.numeric(summary_table_flux[summary_table_flux$terms=="Bark litter P flux", 2:7])/as.numeric(out[out$terms=="Soil P mineralization flux", 2:7]),2)
    
    out[out$terms == "Seed litter P over P mineralization", 2:7] <- round(as.numeric(summary_table_flux[summary_table_flux$terms=="Seed litter P flux", 2:7])/as.numeric(out[out$terms=="Soil P mineralization flux", 2:7]),2)
    
    out[out$terms == "Frass litter P over P mineralization", 2:7] <- round(as.numeric(summary_table_flux[summary_table_flux$terms=="Frass P flux", 2:7])/as.numeric(out[out$terms=="Soil P mineralization flux", 2:7]),2)
    
    out[out$terms == "Understorey litter P over P mineralization", 2:7] <- round(as.numeric(summary_table_flux[summary_table_flux$terms=="Understorey Litter P flux", 2:7])/as.numeric(out[out$terms=="Soil P mineralization flux", 2:7]),2)
    
    out[out$terms == "Leaching P over P mineralization", 2:7] <- round(as.numeric(summary_table_flux[summary_table_flux$terms=="Leaching P flux", 2:7])/as.numeric(out[out$terms=="Soil P mineralization flux", 2:7]),2)
    
    
    out[out$terms == "Overstorey aboveground P stock", 2:7] <- round(vegetation_standing_p_stock$oa,2)
    
    out[out$terms == "Understorey aboveground P stock", 2:7] <- round(vegetation_standing_p_stock$understorey,2)
    
    out[out$terms == "Belowground P stock", 2:7] <- round(vegetation_standing_p_stock$belowground,2)
    
    
    out[out$terms == "Dead P stock", 2:7] <- round(vegetation_standing_p_stock$litter,2)
    
    
    out[out$terms == "Labile Pi stock", 2:7] <- round(summary_table_pool[summary_table_pool$terms=="Exchangeable Pi Pool", 2:7],2)
    
    
    out[out$terms == "Overstorey GPP efficiency", 2:7] <- round(plant_GPP_efficiency$GPP_efficiency_gC_gP[plant_GPP_efficiency$variable=="overstorey"],2)
    out[out$terms == "Understorey GPP efficiency", 2:7] <- round(plant_GPP_efficiency$GPP_efficiency_gC_gP[plant_GPP_efficiency$variable=="understorey"],2)
    
    out[out$terms == "Microbe P MRT", 2:7] <- round(colSums(summary_table_pool[summary_table_pool$terms%in%c("Microbial P Pool 0-10cm",
                                                                                                             "Microbial P Pool 10-30cm",
                                                                                                             "Microbial P Pool 30-60cm"), 2:7])/
                                                        out[out$terms == "Soil P mineralization flux", 2:7],2)
    
    
    out[out$terms == "Microbe P MRT 0-10cm", 2:7] <- round(summary_table_pool[summary_table_pool$terms=="Microbial P Pool 0-10cm", 2:7]/
                                                               summary_table_flux[summary_table_flux$terms=="Mineralization P flux 0-10cm",2:7],2)
    
    out[out$terms == "Microbe P MRT 10-30cm", 2:7] <- round(summary_table_pool[summary_table_pool$terms=="Microbial P Pool 10-30cm", 2:7]/
                                                               summary_table_flux[summary_table_flux$terms=="Mineralization P flux 10-30cm",2:7],2)
    
    out[out$terms == "Microbe P MRT 30-60cm", 2:7] <- round(summary_table_pool[summary_table_pool$terms=="Microbial P Pool 30-60cm", 2:7]/
                                                               summary_table_flux[summary_table_flux$terms=="Mineralization P flux 30-60cm",2:7],2)
    
    
    ### aCO2 and eCO2 averages
    out$aCO2 <- round(rowMeans(data.frame(out$R2, out$R3, out$R6)), 6)
    out$eCO2 <- round(rowMeans(data.frame(out$R1, out$R4, out$R5)) , 6)
    
    
    ### sd
    out$aCO2_sd <- rowSds(as.matrix(subset(out, select=c(R2, R3, R6))), na.rm=T)
    out$eCO2_sd <- rowSds(as.matrix(subset(out, select=c(R1, R4, R5))), na.rm=T)
    
    
    ###### Diff (eCO2 - aCO2)
    out$diff <- round(out$eCO2 - out$aCO2, 6)
    
    ### se of the diff
    out$diff_se <- sqrt((out$aCO2_sd^2+out$eCO2_sd^2)/2) * (sqrt(2/3))
    
    ### confidence interval of the diff
    out$diff_cf <- qt(0.975, 4) * out$diff_se
    
    ###### percent differences (eCO2 - aCO2) / aCO2 * 100
    out$percent_diff <- round((out$eCO2 - out$aCO2) / (out$aCO2) * 100, 2)
    
    
    ### save
    write.csv(out, paste0("plots_tables/summary_tables/", norm, 
                          "/total_p_budget_unnormalized.csv"), row.names=F)
    
    
    return(out)
    
}