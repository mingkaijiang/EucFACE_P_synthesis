make_understorey_p_requirement_table <- function(sumDF) {
    #### Produce a table summarizing the total P requirement based on
    #### NPP, leaf P concentrations in canopy, wood, twig, fine root
    #### and understorey,
    #### Also, ignoring inter-annual variability and ring variability
    #### Just focus on aCO2 and eCO2 for now.
    
    ### total requirement
    out <- sumDF[sumDF$terms == "Understorey P flux",2:9]
 
    return(out)
    
}