make_total_p_budgeting_variables_bootstrap <- function() {
    #### This function calculates all P budgeting variables
    
    ### leaf p retranslocation coefficient
    source("programs/summary_variables/bootstrap/make_leaf_p_retranslocation_coefficient_bootstrap.R")
    leaf_p_retrans_coefficient <- make_leaf_p_retranslocation_coefficient_bootstrap()
    
    
    ### standing P stock
    ### summarize according to year - this ignores bark and twigs
    source("programs/summary_variables/bootstrap/make_total_plant_standing_p_stock_bootstrap.R")
    total_standing_p_stock <- make_total_plant_standing_p_stock_bootstrap(inDF=summary_table_pool_by_treatment_bootstrap)
    
 
    ### P requirements, i.e. using plant P fluxes 
    source("programs/summary_variables/bootstrap/make_total_p_requirement_bootstrap.R")
    total_p_requirement_table <- make_total_p_requirement_table_bootstrap(summary_table_flux_by_treatment)
    
    ### total P retranslocation, i.e. canopy P - litterfall P + wood P increment + fineroot P - fineroot litter P
    source("programs/summary_variables/bootstrap/make_total_p_retranslocation_bootstrap.R")
    total_p_retranslocation <- make_total_p_retranslocation_bootstrap(under_retrans_calc_method = "Simple", 
                                                            understorey_retrans_coef=understorey_p_retranslocation_coefficient,
                                                            sumDF=summary_table_flux_by_treatment)
    
    ### P uptake from soil, i.e. P requirement - P retranslocation
    source("programs/summary_variables/bootstrap/make_p_uptake_from_soil_bootstrap.R")
    total_p_uptake_from_soil <- make_p_uptake_from_soil_bootstrap(p_req=total_p_requirement_table,
                                                        p_retrans=total_p_retranslocation)
    
    
    ### Uptake/requirement
    source("programs/summary_variables/bootstrap/make_p_uptake_over_requirement_bootstrap.R")
    p_uptake_over_requirement <- make_p_uptake_over_requirement_bootstrap(p_up=total_p_uptake_from_soil,
                                                                p_req=total_p_requirement_table)
    
    ### MRT, i.e. Standing P / Uptake
    source("programs/summary_variables/bootstrap/make_p_MRT_bootstrap.R")
    P_mean_residence_time <- make_p_MRT_bootstrap(p_stand=total_standing_p_stock,
                                        p_up=total_p_uptake_from_soil)
    
    ### Standing PUE, i.e. NPP / P Uptake
    source("programs/summary_variables/bootstrap/make_standing_pue_bootstrap.R")
    standing_pue <- make_standing_pue_bootstrap(p_up=total_p_uptake_from_soil)
    
    
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

    out[out$terms == "total p retranslocated", 2:9] <- round(total_p_retranslocation[1,],2)

    out[out$terms == "total p uptake from soil", 2:9] <- round(total_p_uptake_from_soil[1,],2)

    out[out$terms == "total uptake over requirement", 2:9] <- round(p_uptake_over_requirement[1,], 2)

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