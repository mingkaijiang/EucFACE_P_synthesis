make_p_requirement_table <- function(sumDF) {
    #### Produce a table summarizing the total P requirement based on
    #### NPP, leaf P concentrations in canopy, wood, twig, fine root
    #### and understorey,
    #### Also, ignoring inter-annual variability and ring variability
    #### Just focus on aCO2 and eCO2 for now.
    
    ### create npp dataframe (biomass increment data)
    terms <- c("foliage", "wood", "fineroot", "understorey")
    nppDF <- data.frame(terms)
    nppDF$aCO2 <- rep(NA, length(nppDF$terms))
    nppDF$eCO2 <- rep(NA, length(nppDF$terms))
    
    ### create P concentration dataframe
    pconcDF <- nppDF
    
    ### create P requirement dataframe
    preqDF <- pconcDF
    
    ### assign npp values
    ### currently hardwired the data, 
    ### need to later incorporate with C balance results
    nppDF$aCO2[nppDF$terms == "foliage"] <- 291.4
    nppDF$eCO2[nppDF$terms == "foliage"] <- 278.9
    
    nppDF$aCO2[nppDF$terms == "wood"] <- 61.7
    nppDF$eCO2[nppDF$terms == "wood"] <- 58.2
    
    nppDF$aCO2[nppDF$terms == "fineroot"] <- 119.7
    nppDF$eCO2[nppDF$terms == "fineroot"] <- 119.4
    
    nppDF$aCO2[nppDF$terms == "understorey"] <- 0.0
    nppDF$eCO2[nppDF$terms == "understorey"] <- 0.0
    
    ### assign p concentration values
    ### note: foliage biomass includes leaf and twig and bark
    ### here, I am using foliage P concentration to apply to all these
    ### also note: NPP is amount of C increased, not biomass!
    ### Right now, I am simply multiplying %P with C increment. 
    pconcDF$aCO2[pconcDF$terms == "foliage"] <- sumDF$aCO2[sumDF$conc.terms == "Canopy P Conc"]
    pconcDF$eCO2[pconcDF$terms == "foliage"] <- sumDF$eCO2[sumDF$conc.terms == "Canopy P Conc"]
    
    pconcDF$aCO2[pconcDF$terms == "wood"] <- sumDF$aCO2[sumDF$conc.terms == "Wood P Conc"]
    pconcDF$eCO2[pconcDF$terms == "wood"] <- sumDF$eCO2[sumDF$conc.terms == "Wood P Conc"]
    
    pconcDF$aCO2[pconcDF$terms == "fineroot"] <- sumDF$aCO2[sumDF$conc.terms == "Fine Root P Conc"]
    pconcDF$eCO2[pconcDF$terms == "fineroot"] <- sumDF$eCO2[sumDF$conc.terms == "Fine Root P Conc"]
    
    pconcDF$aCO2[pconcDF$terms == "understorey"] <- sumDF$aCO2[sumDF$conc.terms == "Understorey P Conc"]
    pconcDF$eCO2[pconcDF$terms == "understorey"] <- sumDF$eCO2[sumDF$conc.terms == "Understorey P Conc"]
    
    ### make the calculations
    preqDF$aCO2 <- (pconcDF$aCO2 * nppDF$aCO2) / 100 
    preqDF$eCO2 <- (pconcDF$eCO2 * nppDF$eCO2) / 100
    
    ### total requirement
    aCO2 <- sum(preqDF$aCO2)
    eCO2 <- sum(preqDF$eCO2)
    
    preqDF$terms <- as.character(preqDF$terms)
    preqDF[5,"terms"] <- "total"
    preqDF[5,"aCO2"] <- aCO2
    preqDF[5,"eCO2"] <- eCO2
    
    return(preqDF)
    
}