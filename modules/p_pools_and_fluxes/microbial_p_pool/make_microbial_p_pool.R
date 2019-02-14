make_microbial_p_pool <- function(p_conc, bk_density){

    # return depth-averaged bulk density values
    #bk.r<-with(bk_density, tapply(bulk_density_kg_m3, ring, mean))
    bk <- subset(bk_density, Depth == "0-10cm")
    
    for (i in 1:6) {
        p_conc[p_conc$Ring == i, "bk_density"] <- bk[bk$ring == i, "bulk_density_kg_m3"] 
    }
        
    # calculate total P in top 10cm of soil (hence the * 0.1), unit kg m-2
    p_conc$microbial_p_kg_m2 <- p_conc$PercP * p_conc$bk_density * 0.1 / 100
    
    # return in unit of g/m2
    p_conc$microbial_p_g_m2 <-p_conc$microbial_p_kg_m2 * 1000.0
    
    myDF.out <- p_conc[,c("Date", "Ring", "microbial_p_g_m2")]
    
    return(myDF.out)
    
}
