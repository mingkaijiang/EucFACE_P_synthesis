make_p_MRT <- function(p_stand, p_up) {
    
    out <- p_up
    year <- c(min(p_stand$Year):max(p_stand$Year))
    out <- data.frame(year, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(out) <- c("year", "R1", "R2", "R3", "R4", "R5", "R6", "aCO2", "eCO2")
    
    for (i in c(1:6)) {
        for (j in year) {
            out[out$year == j, i+1] <- p_stand[p_stand$Year == j & p_stand$Ring == i, "total"] / p_up[1, i]
        }
    }
    
    out$aCO2 <- rowMeans(data.frame(out$R2, out$R3, out$R6))
    out$eCO2 <- rowMeans(data.frame(out$R1, out$R4, out$R5))
    
    return(out)
}