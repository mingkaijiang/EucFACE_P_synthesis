make_p_uptake_from_soil <- function(p_req, p_retrans) {
    
    p_up <- p_req - p_retrans
    
    return(p_up)
}