make_understorey_standing_p_stock_normalized <- function(abg) {
    ### make standing p stock per year
    out <- summaryBy(predicted~Ring, data=abg, FUN=mean, keep.names=T)
     
    return(out)
}