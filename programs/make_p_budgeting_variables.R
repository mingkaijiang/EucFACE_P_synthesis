make_p_budgeting_variables <- function() {
    #### This function calculates all P budgeting variables
    
    ### leaf p retranslocation coefficient
    source("programs/make_leaf_p_retranslocation_coefficient.R")
    leaf_p_retrans_coefficient <- make_leaf_p_retranslocation_coefficient()
    
    
    ### standing P stock, i.e. canopy P + wood P + fine root P pools
    ### summarize according to year
    source("programs/make_standing_p_stock.R")
    standing_p_stock <- make_standing_p_stock(leaf=canopy_p_pool, 
                                              wood=wood_p_pool, 
                                              froot=fineroot_p_pool, 
                                              croot=coarse_root_p_pool_1)
    
    ### taking the averages 
    standing_p_stock_avg <- summaryBy(total~Ring, data=standing_p_stock, FUN=mean, na.rm=T, keep.names=T)
    
    ### P requirements, i.e. using plant P fluxes 
    source("programs/make_p_requirement.R")
    p_requirement_table <- make_p_requirement_table(summary_table_flux_by_treatment)
    
    
    ### total P retranslocation, i.e. canopy P - litterfall P + wood P increment
    source("programs/make_total_p_retranslocation.R")
    total_p_retranslocation <- make_total_p_retranslocation(summary_table_flux_by_treatment)
    
    ### P uptake from soil, i.e. P requirement - P retranslocation
    source("programs/make_p_uptake_from_soil.R")
    total_p_uptake_from_soil <- make_p_uptake_from_soil(p_req=p_requirement_table,
                                                        p_retrans=total_p_retranslocation)
    
    ### Uptake/requirement
    source("programs/make_p_uptake_over_requirement.R")
    p_uptake_over_requirement <- make_p_uptake_over_requirement(p_up=total_p_uptake_from_soil,
                                                                p_req=p_requirement_table)
    
    
    ### MRT, i.e. Standing P / Uptake
    source("programs/make_p_MRT.R")
    P_mean_residence_time <- make_p_MRT(p_stand=standing_p_stock,
                                        p_up=total_p_uptake_from_soil)
    
    ### averaging
    P_mean_residence_time_avg <- colMeans(P_mean_residence_time)
    
    ### Standing PUE, i.e. NPP / P Uptake
    
    
    ### out df
    terms <- c("overstorey leaf p retrans coef", 
               "total standing p stock", "overstorey standing p stock","understorey standing p stock", 
               "total p requirement", "overstorey p requirement", "understorey p requirement",
               "total p retranslocated", "total p uptake from soil", "total uptake over requirement", 
               "total P MRT in plant", "total standing PUE")
    
    out <- data.frame(terms, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(out) <- c("terms", "R1", "R2", "R3", "R4", "R5", "R6", "aCO2", "eCO2", "notes")
    
    ### assign values
    out[out$terms == "overstorey leaf p retrans coef", 2:7] <- round(leaf_p_retrans_coefficient$retrans_coef * 100, 1)
    out[out$terms == "total standing p stock", 2:7] <- round(standing_p_stock_avg$total,2)
    out[out$terms == "total p requirement", 2:9] <- round(p_requirement_table[1,],2)
    out[out$terms == "total p retranslocated", 2:9] <- round(total_p_retranslocation[1,],3)
    out[out$terms == "total p uptake from soil", 2:9] <- round(total_p_uptake_from_soil[1,],2)
    out[out$terms == "total uptake over requirement", 2:9] <- round(p_uptake_over_requirement[1,], 1)
    out[out$terms == "total P MRT in plant", 2:9] <- round(P_mean_residence_time[1,2:8],2)
    
    
    
    
    return(out)
    
}