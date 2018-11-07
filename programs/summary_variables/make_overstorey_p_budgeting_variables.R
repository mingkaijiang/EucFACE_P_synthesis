make_overstorey_p_budgeting_variables <- function() {
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
                                                                    croot=coarse_root_p_pool)
    
    overstorey_standing_p_stock_avg <- summaryBy(total~Ring, data=overstorey_standing_p_stock, 
                                                 FUN=mean, na.rm=T, keep.names=T)
    
    
    ### P requirements, i.e. using plant P fluxes 
 
    source("programs/summary_variables/make_overstorey_p_requirement.R")
    overstorey_p_requirement_table <- make_overstorey_p_requirement_table(summary_table_flux_by_treatment)
    
    ### overstorey P retranslocation, i.e. canopy P - litterfall P + wood P increment + fineroot P - fineroot litter P
    source("programs/summary_variables/make_overstorey_p_retranslocation.R")
    overstorey_p_retranslocation <- make_overstorey_p_retranslocation(summary_table_flux_by_treatment)
    
    ### P uptake from soil, i.e. P requirement - P retranslocation
    source("programs/summary_variables/make_p_uptake_from_soil.R")

    overstorey_p_uptake_from_soil <- make_p_uptake_from_soil(p_req=overstorey_p_requirement_table,
                                                             p_retrans=overstorey_p_retranslocation)

    ### Uptake/requirement
    source("programs/summary_variables/make_p_uptake_over_requirement.R")

    overstorey_p_uptake_over_requirement <- make_p_uptake_over_requirement(p_up=overstorey_p_uptake_from_soil,
                                                                           p_req=overstorey_p_requirement_table)
    
    ### out df
    terms <- c("overstorey leaf p retrans coef", 
               "overstorey standing p stock",
               "overstorey p requirement", 
               "overstorey p retranslocated", 
               "overstorey p uptake from soil", 
               "overstorey uptake over requirement")
    
    out <- data.frame(terms, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(out) <- c("terms", "R1", "R2", "R3", "R4", "R5", "R6", "aCO2", "eCO2", "notes")
    
    ### assign values
    out[out$terms == "overstorey leaf p retrans coef", 2:7] <- round(leaf_p_retrans_coefficient$retrans_coef * 100, 1)
    out[out$terms == "overstorey standing p stock", 2:7] <- round(overstorey_standing_p_stock_avg$total,2)

    out[out$terms == "overstorey p requirement", 2:9] <- round(overstorey_p_requirement_table[1,],2)

    out[out$terms == "overstorey p retranslocated", 2:9] <- round(overstorey_p_retranslocation[1,],2)

    out[out$terms == "overstorey p uptake from soil", 2:9] <- round(overstorey_p_uptake_from_soil[1,],2)

    out[out$terms == "overstorey uptake over requirement", 2:9] <- round(overstorey_p_uptake_over_requirement[1,], 2)

    ### aCO2 and eCO2 averages
    out$aCO2 <- round(rowMeans(data.frame(out$R2, out$R3, out$R6)), 4)
    out$eCO2 <- round(rowMeans(data.frame(out$R1, out$R4, out$R5)) , 4)
    
    ### notes
    out[out$terms == "overstorey leaf p retrans coef", "notes"] <- "P concentration leaf - leaflitter"
    
    out[out$terms == "overstorey standing p stock", "notes"] <- "canopy, wood, fineroot & coarse root"

    out[out$terms == "overstorey p requirement", "notes"] <- "leaf, wood, roots, frass and other litter"

    out[out$terms == "overstorey p retranslocated", "notes"] <- "wood increment included, no consideration of coarse root"

    out[out$terms == "overstorey p uptake from soil", "notes"] <- "the diff between req and retrans"

    out[out$terms == "overstorey uptake over requirement", "notes"] <- "no consideration of coarse root"

    return(out)
    
}