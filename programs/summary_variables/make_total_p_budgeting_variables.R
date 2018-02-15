make_total_p_budgeting_variables <- function() {
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
    
    ### total P retranslocation, i.e. canopy P - litterfall P + wood P increment + fineroot P - fineroot litter P
    source("programs/summary_variables/make_total_p_retranslocation.R")
    total_p_retranslocation <- make_total_p_retranslocation(understorey_retrans_coef=retrans_ud,
                                                            summary_table_flux_by_treatment)
    
    ### P uptake from soil, i.e. P requirement - P retranslocation
    source("programs/summary_variables/make_p_uptake_from_soil.R")
    total_p_uptake_from_soil <- make_p_uptake_from_soil(p_req=total_p_requirement_table,
                                                        p_retrans=total_p_retranslocation)
    
    
    ### Uptake/requirement
    source("programs/summary_variables/make_p_uptake_over_requirement.R")
    p_uptake_over_requirement <- make_p_uptake_over_requirement(p_up=total_p_uptake_from_soil,
                                                                p_req=total_p_requirement_table)
    
    ### MRT, i.e. Standing P / Uptake
    source("programs/summary_variables/make_p_MRT.R")
    P_mean_residence_time <- make_p_MRT(p_stand=total_standing_p_stock,
                                        p_up=total_p_uptake_from_soil)
    
    ### Standing PUE, i.e. NPP / P Uptake
    source("programs/summary_variables/make_standing_pue.R")
    standing_pue <- make_standing_pue(p_up=total_p_uptake_from_soil)
    
    
    ### out df
    terms <- c("total standing p stock", 
               "total p requirement", 
               "total p retranslocated", 
               "total p uptake from soil", 
               "total uptake over requirement",
               "total P MRT in plant", "total standing PUE")
    
    out <- data.frame(terms, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(out) <- c("terms", "R1", "R2", "R3", "R4", "R5", "R6", "aCO2", "eCO2", "notes")
    
    ### assign values
    out[out$terms == "total standing p stock", 2:7] <- round(total_standing_p_stock,2)
    
    out[out$terms == "total p requirement", 2:9] <- round(total_p_requirement_table[1,],2)

    out[out$terms == "total p retranslocated", 2:9] <- round(total_p_retranslocation[1,],3)

    out[out$terms == "total p uptake from soil", 2:9] <- round(total_p_uptake_from_soil[1,],2)

    out[out$terms == "total uptake over requirement", 2:9] <- round(p_uptake_over_requirement[1,], 1)

    out[out$terms == "total P MRT in plant", 2:9] <- round(P_mean_residence_time[1,1:8],2)
    out[out$terms == "total standing PUE", 2:7] <- round(standing_pue[1:6, "NPP_by_PUP"],4)
    
    
    ### aCO2 and eCO2 averages
    out$aCO2 <- round(rowMeans(data.frame(out$R2, out$R3, out$R6)), 4)
    out$eCO2 <- round(rowMeans(data.frame(out$R1, out$R4, out$R5)) , 4)
    
    ### notes
    out[out$terms == "total standing p stock", "notes"] <- "overstorey + understorey"

    out[out$terms == "total p requirement", "notes"] <- "NPP by P conc"

    out[out$terms == "total p retranslocated", "notes"] <- "guess value for understorey"

    out[out$terms == "total p uptake from soil", "notes"] <- "the diff between req and retrans"

    out[out$terms == "total uptake over requirement", "notes"] <- "uncertainty in wood and understorey"

    out[out$terms == "total P MRT in plant", "notes"] <- "standing stock / uptake"
    out[out$terms == "total standing PUE", "notes"] <- "NPP / uptake"
    
    return(out)
    
}