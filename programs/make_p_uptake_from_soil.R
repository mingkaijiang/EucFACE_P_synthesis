make_p_uptake_from_soil <- function(p_req, p_retrans) {
    
    p_up <- p_req[1,] - p_retrans[1,]
    
    return(p_up)
}