make_mycorrhizal_c_pool <- function(micDF) {
    
    ### calculate mycorrhizal biomass
    micDF$mycorrhizal_c_pool[micDF$Ring==1] <- micDF$Cmic_g_m2[micDF$Ring==1] * 0.105
    micDF$mycorrhizal_c_pool[micDF$Ring==2] <- micDF$Cmic_g_m2[micDF$Ring==2] * 0.115
    micDF$mycorrhizal_c_pool[micDF$Ring==3] <- micDF$Cmic_g_m2[micDF$Ring==3] * 0.101
    micDF$mycorrhizal_c_pool[micDF$Ring==4] <- micDF$Cmic_g_m2[micDF$Ring==4] * 0.11
    micDF$mycorrhizal_c_pool[micDF$Ring==5] <- micDF$Cmic_g_m2[micDF$Ring==5] * 0.088
    micDF$mycorrhizal_c_pool[micDF$Ring==6] <- micDF$Cmic_g_m2[micDF$Ring==6] * 0.128
    
    outDF <- micDF[,c("Date", "Ring", "Depth", "mycorrhizal_c_pool")]

    
    return(outDF)
    
}