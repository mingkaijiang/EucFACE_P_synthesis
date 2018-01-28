make_microbial_p_pool <- function(p_conc, bk_density){

    # return depth-averaged bulk density values
    bk.r<-with(bk_density, tapply(bulk_density_kg_m3, ring, mean))
    
    for (i in 1:6) {
        df.m[df.m$ring == i, "bk_density"] <- bk.r[i]
    }
        
    # unit conversion: mg/kg to g/m2
    df.m$Pmic_g_m2 <- df.m$bk_density * df.m$Pmic * 0.3 / g_to_mg
    
    # update variables to output Pmic in unit g m-2
    df.out <- df.m[,c("ring", "date", "Pmic_g_m2")]
    
    df.out <- df.out[complete.cases(df.out),]
    
    return(df.out)
    
}
