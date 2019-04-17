make_summary_table_np_ratios <- function() {
    
    ### add stuffs
    outDF <- canopy_np_ratio
    colnames(outDF) <- c("Ring", "canopy")
    
    outDF$soil <- soil_np_ratio$np_ratio
    outDF$readily_available_soil <- readily_available_soil_np_ratio$np_ratio
    outDF$frass <- frass_np_ratio$np_ratio
    outDF$understorey <- understorey_np_ratio$np_ratio
    
   write.csv(outDF, "plots_tables/summary_table_np_ratio.csv", row.names=F)
}