make_understorey_standing_p_stock <- function(abg) {
    ### make standing p stock per year
    out <- summaryBy(understorey_p_pool~Ring, data=abg, FUN=mean, keep.names=T)
     
    return(out)
}