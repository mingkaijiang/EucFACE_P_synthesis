make_understorey_p_budgeting_variables <- function() {
    #### This function calculates all P budgeting variables
    

    source("programs/summary_variables/unnormalized/make_understorey_standing_p_stock.R")
    understorey_standing_p_stock <- make_understorey_standing_p_stock(abg=understorey_p_pool)
    
    source("programs/summary_variables/unnormalized/make_understorey_p_requirement.R")
    understorey_p_requirement_table <- make_understorey_p_requirement_table(summary_table_flux_by_treatment)
    
    ### understorey P retranslocation, 
    source("programs/summary_variables/unnormalized/make_understorey_p_retranslocation.R")
    understorey_p_retranslocation <- make_understorey_p_retranslocation(retrans_calc_method = "Simple",
                                                                        retrans_coef = understorey_p_retranslocation_coefficient,
                                                                        summary_table_flux_by_treatment)
    
    ### P uptake from soil, i.e. P requirement - P retranslocation
    source("programs/summary_variables/make_p_uptake_from_soil.R")
    
    understorey_p_uptake_from_soil <- make_p_uptake_from_soil(p_req=understorey_p_requirement_table,
                                                              p_retrans=understorey_p_retranslocation)
    
    ### Uptake/requirement
    source("programs/summary_variables/make_p_uptake_over_requirement.R")

    understorey_p_uptake_over_requirement <- make_p_uptake_over_requirement(p_up=understorey_p_uptake_from_soil,
                                                                p_req=understorey_p_requirement_table)
    

    ### out df
    terms <- c("understorey standing p stock", 
               "understorey p requirement",
               "understorey p retranslocated",
               "understorey p uptake from soil",
               "understorey uptake over requirement")
    
    out <- data.frame(terms, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(out) <- c("terms", "R1", "R2", "R3", "R4", "R5", "R6", "aCO2", "eCO2", "notes")
    
    ### assign values
    out[out$terms == "understorey standing p stock", 2:7] <- round(understorey_standing_p_stock$understorey_p_pool,2)

    out[out$terms == "understorey p requirement", 2:9] <- round(understorey_p_requirement_table[1,],2)

    out[out$terms == "understorey p retranslocated", 2:9] <- round(understorey_p_retranslocation[1,],2)

    out[out$terms == "understorey p uptake from soil", 2:9] <- round(understorey_p_uptake_from_soil[1,],2)
 
    out[out$terms == "understorey uptake over requirement", 2:9] <- round(understorey_p_uptake_over_requirement[1,], 2)
 
    
    ### aCO2 and eCO2 averages
    out$aCO2 <- round(rowMeans(data.frame(out$R2, out$R3, out$R6)), 4)
    out$eCO2 <- round(rowMeans(data.frame(out$R1, out$R4, out$R5)) , 4)
    
    
    ### sd
    out$aCO2_sd <- rowSds(as.matrix(subset(out, select=c(R2, R3, R6))), na.rm=T)
    out$eCO2_sd <- rowSds(as.matrix(subset(out, select=c(R1, R4, R5))), na.rm=T)
    
    ### notes
    out[out$terms == "understorey standing p stock", "notes"] <- "no data on understorey belowground"
    
    out[out$terms == "understorey p requirement", "notes"] <- "only aboveground"
 
    out[out$terms == "understorey p retranslocated", "notes"] <- "50% retranslocated"
 
    out[out$terms == "understorey p uptake from soil", "notes"] <- "the diff between req and retrans"
 
    out[out$terms == "understorey uptake over requirement", "notes"] <- "50% retranslocation"
 
    return(out)
    
}