make_soil_p_budgeting_variables_normalized <- function(sumDF) {
    #### This function calculates all soil P budgeting variables
    
    ### P mineralization rate
    p_mineralization <- summaryBy(predicted~Ring, FUN=mean, keep.names=T, data=soil_p_mineralization_pred)
    p_mineralization$p_mineralization <- with(p_mineralization, predicted)
    
    ### P leaching flux
    p_leaching <- summaryBy(predicted~Ring, FUN=mean, keep.names=T, data=soil_p_leaching_pred)
    p_leaching$p_leaching <- with(p_leaching, predicted)
    
    
    ### P phosphate pool
    soil_phosphate <- summaryBy(predicted~Ring, FUN=mean, keep.names=T, data=soil_phosphate_pool_pred)
    
    ### Total soil P pool
    total_soil_p <- summaryBy(predicted~Ring, FUN=mean, keep.names=T, data=soil_p_pool_pred)
    
    ### Aqua regia P
    aqua_regia_p <- summaryBy(Total_Aqua_Regia_P~Ring, FUN=mean, keep.names=T, data=soil_p_pool_hedley)
    
    ### out df
    terms <- c("P mineralization flux",
               "P leaching flux",
               "Soil phosphate P pool", 
               "Exhanagable Pi pool",
               "Exhanagable Po pool",
               "Moderately labile Po pool",
               "Secondary Pi Fe bound pool", 
               "Primary Pi Ca bound pool",
               "Occluded P pool",
               "Total aqua regia P pool",
               "Total soil P pool")
    
    out <- data.frame(terms, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(out) <- c("terms", "R1", "R2", "R3", "R4", "R5", "R6", "aCO2", "eCO2", "notes")
    
    ### assign values
    out[out$terms == "P mineralization flux", 2:7] <- round(p_mineralization$p_mineralization,2)
    
    out[out$terms == "P leaching flux", 2:7] <- round(p_leaching$p_leaching,6)
    
    
    out[out$terms == "Soil phosphate P pool", 2:7] <- round(soil_phosphate$predicted,2)

    out[out$terms == "Exhanagable Pi pool", 2:7] <- round(sumDF[sumDF$terms=="Exhanagable Pi Pool",2:7],2)
    
    out[out$terms == "Exhanagable Po pool", 2:7] <- round(sumDF[sumDF$terms=="Exhanagable Po Pool",2:7],2)
    
    out[out$terms == "Moderately labile Po pool", 2:7] <- round(sumDF[sumDF$terms=="Moderately labile Po Pool",2:7],2)
    
    out[out$terms == "Secondary Pi Fe bound pool", 2:7] <- round(sumDF[sumDF$terms=="Secondary Fe bound Pi Pool",2:7],2)
    
    out[out$terms == "Primary Pi Ca bound pool", 2:7] <- round(sumDF[sumDF$terms=="Primary Ca bound Pi Pool",2:7],2)
    
    out[out$terms == "Occluded P pool", 2:7] <- round(sumDF[sumDF$terms=="Occluded P Pool",2:7],2)
    
    out[out$terms == "Total aqua regia P pool", 2:7] <- round(aqua_regia_p$Total_Aqua_Regia_P,2)
    
    out[out$terms == "Total soil P pool", 2:7] <- round(total_soil_p$predicted,2)
    
    ### aCO2 and eCO2 averages
    out$aCO2 <- round(rowMeans(data.frame(out$R2, out$R3, out$R6)), 4)
    out$eCO2 <- round(rowMeans(data.frame(out$R1, out$R4, out$R5)) , 4)
    
    
    ### sd
    out$aCO2_sd <- rowSds(as.matrix(subset(out, select=c(R2, R3, R6))), na.rm=T)
    out$eCO2_sd <- rowSds(as.matrix(subset(out, select=c(R1, R4, R5))), na.rm=T)
    
    
    ### notes
    out[out$terms == "P mineralization flux", "notes"] <- "normalized"
    
    out[out$terms == "P leaching flux", "notes"] <- "normalized"
    
    
    out[out$terms == "Soil phosphate P pool","notes"] <- "normalized"
    
    out[out$terms == "Exhanagable Pi pool", "notes"] <- "Based on Hedley frac"
    
    out[out$terms == "Exhanagable Po pool", "notes"] <- "Based on Hedley frac"
    
    out[out$terms == "Moderately labile Po pool", "notes"] <- "Based on Hedley frac"
    
    out[out$terms == "Secondary Pi Fe bound pool", "notes"] <- "Based on Hedley frac"
    
    out[out$terms == "Primary Pi Ca bound pool", "notes"] <- "Based on Hedley frac"
    
    out[out$terms == "Occluded P pool", "notes"] <- "Based on Hedley frac"
    
    out[out$terms == "Total aqua regia P pool", "notes"] <- "normalized"
    
    out[out$terms == "Total soil P pool", "notes"] <- "normalized"
    
    
    return(out)
    
}