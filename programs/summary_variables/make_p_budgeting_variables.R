make_p_budgeting_variables <- function() {
    #### This function calculates all P budgeting variables
    
    ### leaf p retranslocation coefficient
    source("programs/summary_variables/make_leaf_p_retranslocation_coefficient.R")
    leaf_p_retrans_coefficient <- make_leaf_p_retranslocation_coefficient()
    
    
    ### standing P stock
    ### summarize according to year
    source("programs/summary_variables/make_overstorey_standing_p_stock.R")
    overstorey_standing_p_stock <- make_overstorey_standing_p_stock(leaf=canopy_p_pool, 
                                                                    wood=wood_p_pool, 
                                                                    froot=fineroot_p_pool, 
                                                                    croot=coarse_root_p_pool_1)
    
    overstorey_standing_p_stock_avg <- summaryBy(total~Ring, data=overstorey_standing_p_stock, 
                                                 FUN=mean, na.rm=T, keep.names=T)
    
    
    source("programs/summary_variables/make_understorey_standing_p_stock.R")
    understorey_standing_p_stock <- make_understorey_standing_p_stock(abg=understorey_p_pool)
    
    total_standing_p_stock <- overstorey_standing_p_stock_avg$total + understorey_standing_p_stock$understorey_p_pool
    
    ### P requirements, i.e. using plant P fluxes 
    source("programs/summary_variables/make_total_p_requirement.R")
    total_p_requirement_table <- make_total_p_requirement_table(summary_table_flux_by_treatment)
    
    source("programs/summary_variables/make_overstorey_p_requirement.R")
    overstorey_p_requirement_table <- make_overstorey_p_requirement_table(summary_table_flux_by_treatment)
    
    source("programs/summary_variables/make_understorey_p_requirement.R")
    understorey_p_requirement_table <- make_understorey_p_requirement_table(summary_table_flux_by_treatment)
    
    ### overstorey P retranslocation, i.e. canopy P - litterfall P + wood P increment + fineroot P - fineroot litter P
    source("programs/summary_variables/make_overstorey_p_retranslocation.R")
    overstorey_p_retranslocation <- make_overstorey_p_retranslocation(summary_table_flux_by_treatment)
    
    ### total P retranslocation, i.e. canopy P - litterfall P + wood P increment + fineroot P - fineroot litter P
    source("programs/summary_variables/make_total_p_retranslocation.R")
    total_p_retranslocation <- make_total_p_retranslocation(understorey_retrans_coef=retrans_ud,
                                                            summary_table_flux_by_treatment)
    
    ### understorey P retranslocation, 
    source("programs/summary_variables/make_understorey_p_retranslocation.R")
    understorey_p_retranslocation <- make_understorey_p_retranslocation(understorey_retrans_coef=retrans_ud,
                                                                        summary_table_flux_by_treatment)
    
    ### P uptake from soil, i.e. P requirement - P retranslocation
    source("programs/summary_variables/make_p_uptake_from_soil.R")
    total_p_uptake_from_soil <- make_p_uptake_from_soil(p_req=total_p_requirement_table,
                                                        p_retrans=total_p_retranslocation)
    
    overstorey_p_uptake_from_soil <- make_p_uptake_from_soil(p_req=overstorey_p_requirement_table,
                                                             p_retrans=overstorey_p_retranslocation)

    understorey_p_uptake_from_soil <- make_p_uptake_from_soil(p_req=understorey_p_requirement_table,
                                                              p_retrans=understorey_p_retranslocation)
    
    ### Uptake/requirement
    source("programs/summary_variables/make_p_uptake_over_requirement.R")
    p_uptake_over_requirement <- make_p_uptake_over_requirement(p_up=total_p_uptake_from_soil,
                                                                p_req=total_p_requirement_table)
    
    overstorey_p_uptake_over_requirement <- make_p_uptake_over_requirement(p_up=overstorey_p_uptake_from_soil,
                                                                          p_req=overstorey_p_requirement_table)
    
    understorey_p_uptake_over_requirement <- make_p_uptake_over_requirement(p_up=understorey_p_uptake_from_soil,
                                                                            p_req=understorey_p_requirement_table)
    
    ### MRT, i.e. Standing P / Uptake
    source("programs/summary_variables/make_p_MRT.R")
    P_mean_residence_time <- make_p_MRT(p_stand=total_standing_p_stock,
                                        p_up=total_p_uptake_from_soil)
    
    ### Standing PUE, i.e. NPP / P Uptake
    source("programs/summary_variables/make_standing_pue.R")
    standing_pue <- make_standing_pue(p_up=total_p_uptake_from_soil)
    
    
    ### out df
    terms <- c("overstorey leaf p retrans coef", 
               "total standing p stock", "overstorey standing p stock","understorey standing p stock", 
               "total p requirement", "overstorey p requirement", "understorey p requirement",
               "total p retranslocated", "overstorey p retranslocated", "understorey p retranslocated",
               "total p uptake from soil", "overstorey p uptake from soil", "understorey p uptake from soil",
               "total uptake over requirement", "overstorey uptake over requirement", "understorey uptake over requirement",
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
    out[out$terms == "overstorey p retranslocated", 2:9] <- round(overstorey_p_retranslocation[1,],3)
    out[out$terms == "understorey p retranslocated", 2:9] <- round(understorey_p_retranslocation[1,],3)
    
    out[out$terms == "total p uptake from soil", 2:9] <- round(total_p_uptake_from_soil[1,],2)
    out[out$terms == "overstorey p uptake from soil", 2:9] <- round(overstorey_p_uptake_from_soil[1,],2)
    out[out$terms == "understorey p uptake from soil", 2:9] <- round(understorey_p_uptake_from_soil[1,],2)
    
    out[out$terms == "total uptake over requirement", 2:9] <- round(p_uptake_over_requirement[1,], 1)
    out[out$terms == "overstorey uptake over requirement", 2:9] <- round(overstorey_p_uptake_over_requirement[1,], 1)
    out[out$terms == "understorey uptake over requirement", 2:9] <- round(understorey_p_uptake_over_requirement[1,], 1)
    
    out[out$terms == "total P MRT in plant", 2:9] <- round(P_mean_residence_time[1,1:8],2)
    out[out$terms == "total standing PUE", 2:7] <- round(standing_pue[1:6, "NPP_by_PUP"],4)
    
    
    ### aCO2 and eCO2 averages
    out$aCO2 <- round(rowMeans(data.frame(out$R2, out$R3, out$R6)), 4)
    out$eCO2 <- round(rowMeans(data.frame(out$R1, out$R4, out$R5)) , 4)
    
    ### notes
    out[out$terms == "overstorey leaf p retrans coef", "notes"] <- "P concentration leaf - leaflitter"
    
    out[out$terms == "total standing p stock", "notes"] <- "overstorey + understorey"
    out[out$terms == "overstorey standing p stock", "notes"] <- "canopy, wood, fineroot & coarse root"
    out[out$terms == "understorey standing p stock", "notes"] <- "no data on understorey belowground"
    
    out[out$terms == "total p requirement", "notes"] <- "NPP by P conc"
    out[out$terms == "overstorey p requirement", "notes"] <- "leaf, wood, roots, frass and other litter"
    out[out$terms == "understorey p requirement", "notes"] <- "only aboveground"
    
    out[out$terms == "total p retranslocated", "notes"] <- "guess value for understorey"
    out[out$terms == "overstorey p retranslocated", "notes"] <- "wood increment included, no consideration of coarse root"
    out[out$terms == "understorey p retranslocated", "notes"] <- "50% retranslocated"
    
    out[out$terms == "total p uptake from soil", "notes"] <- "the diff between req and retrans"
    out[out$terms == "overstorey p uptake from soil", "notes"] <- "the diff between req and retrans"
    out[out$terms == "understorey p uptake from soil", "notes"] <- "the diff between req and retrans"
    
    out[out$terms == "total uptake over requirement", "notes"] <- "uncertainty in wood and understorey"
    out[out$terms == "overstorey uptake over requirement", "notes"] <- "no consideration of coarse root"
    out[out$terms == "understorey uptake over requirement", "notes"] <- "50% retranslocation"
    
    out[out$terms == "total P MRT in plant", "notes"] <- "standing stock / uptake"
    out[out$terms == "total standing PUE", "notes"] <- "NPP / uptake"
    
    return(out)
    
}