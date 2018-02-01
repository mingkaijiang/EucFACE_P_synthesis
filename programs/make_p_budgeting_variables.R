make_p_budgeting_variables <- function() {
    #### This function calculates all P budgeting variables
    
    ### leaf p retranslocation coefficient
    source("programs/make_leaf_p_retranslocation_coefficient.R")
    leaf_p_retrans_coefficient <- make_leaf_p_retranslocation_coefficient()
    
    
    ### standing P stock
    ### summarize according to year
    source("programs/make_overstorey_standing_p_stock.R")
    overstorey_standing_p_stock <- make_overstorey_standing_p_stock(leaf=canopy_p_pool, 
                                                                    wood=wood_p_pool, 
                                                                    froot=fineroot_p_pool, 
                                                                    croot=coarse_root_p_pool_1)
    
    overstorey_standing_p_stock_avg <- summaryBy(total~Ring, data=overstorey_standing_p_stock, 
                                                 FUN=mean, na.rm=T, keep.names=T)
    
    
    source("programs/make_understorey_standing_p_stock.R")
    understorey_standing_p_stock <- make_understorey_standing_p_stock(abg=understorey_p_pool)
    
    ### P requirements, i.e. using plant P fluxes 
    source("programs/make_total_p_requirement.R")
    total_p_requirement_table <- make_total_p_requirement_table(summary_table_flux_by_treatment)
    
    source("programs/make_overstorey_p_requirement.R")
    overstorey_p_requirement_table <- make_overstorey_p_requirement_table(summary_table_flux_by_treatment)
    
    source("programs/make_understorey_p_requirement.R")
    understorey_p_requirement_table <- make_understorey_p_requirement_table(summary_table_flux_by_treatment)
    
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
    out[out$terms == "overstorey standing p stock", 2:7] <- round(overstorey_standing_p_stock_avg$total,2)
    out[out$terms == "understorey standing p stock", 2:7] <- round(understorey_standing_p_stock$understorey_p_pool,2)
    out[out$terms == "total standing p stock", 2:7] <- round(out[out$terms == "understorey standing p stock", 2:7] + 
                                                                 out[out$terms == "overstorey standing p stock", 2:7],2)
    
    out[out$terms == "total p requirement", 2:9] <- round(total_p_requirement_table[1,],2)
    out[out$terms == "overstorey p requirement", 2:9] <- round(overstorey_p_requirement_table[1,],2)
    out[out$terms == "understorey p requirement", 2:9] <- round(understorey_p_requirement_table[1,],2)
    
    out[out$terms == "total p retranslocated", 2:9] <- round(total_p_retranslocation[1,],3)
    out[out$terms == "total p uptake from soil", 2:9] <- round(total_p_uptake_from_soil[1,],2)
    out[out$terms == "total uptake over requirement", 2:9] <- round(p_uptake_over_requirement[1,], 1)
    out[out$terms == "total P MRT in plant", 2:9] <- round(P_mean_residence_time[1,2:8],2)
    
    
    ### aCO2 and eCO2 averages
    out$aCO2 <- round(rowMeans(data.frame(out$R2, out$R3, out$R6)), 2)
    out$eCO2 <- round(rowMeans(data.frame(out$R1, out$R4, out$R5)) , 2)
    
    ### notes
    out[out$terms == "overstorey leaf p retrans coef", "notes"] <- "P concentration leaf - leaflitter"
    out[out$terms == "total standing p stock", "notes"] <- "overstorey + understorey"
    out[out$terms == "overstorey standing p stock", "notes"] <- "canopy, wood, fineroot & coarse root"
    out[out$terms == "understorey standing p stock", "notes"] <- "no data on understorey belowground"
    out[out$terms == "total p requirement", "notes"] <- "NPP by P conc"
    out[out$terms == "overstorey p requirement", "notes"] <- "leaf, wood, roots, frass and other litter"
    out[out$terms == "understorey p requirement", "notes"] <- "only aboveground"
    out[out$terms == "total p retranslocated", "notes"] <- "only leaf, no info on wood and roots and understorey"
    out[out$terms == "total p uptake from soil", "notes"] <- "the diff between req and retrans"
    out[out$terms == "total uptake over requirement", "notes"] <- "very high uptake"
    out[out$terms == "total P MRT in plant", "notes"] <- "standing stock / uptake"
    out[out$terms == "total standing PUE", "notes"] <- "NPP / uptake"
    
    return(out)
    
}