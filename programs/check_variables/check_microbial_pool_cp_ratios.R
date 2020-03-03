check_microbial_pool_CP_ratios <- function(c_pool, p_pool) {

    ### Compute CP ratio for microbial pool inside the ring
    temp <- c_pool[c_pool$terms == "Microbial C Pool", 2:7]/p_pool[p_pool$terms == "Microbial P Pool",2:7]
    i_mean <- mean(as.numeric(temp), na.rm=T)
    i_sd <- sd(as.numeric(temp), na.rm=T)
    
    ### Compute CP ratio for microbial pool outside the ring
    myDF <- read.csv("temp_files/Microbial_Data_Yolima_outring.csv")
    temp <- myDF$Microbial.Biomass.C / myDF$P.Microbial.Biomass
    o_mean <- mean(temp, na.rm=T)
    o_sd <- sd(temp, na.rm=T)
    
    out <- data.frame(i_mean, i_sd, o_mean, o_sd)
    colnames(out) <- c("Inside_avg", "inside_sd", "outside_avg", "outside_sd")
    
    return(out)
    
}