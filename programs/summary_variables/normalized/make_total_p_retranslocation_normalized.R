make_total_p_retranslocation_bootstrap <- function(sumDF) {
    ### Calculate total p retranslocated
    ### (canopy P + wood P increment + fineroot P + understorey P + coarseroot P increment) 
    ### - litterfall P - fineroot litter P - understorey litter P

    
    ### prepare output df
    out <- data.frame(c("Total P retranslocation"), NA, NA, NA, NA)
    colnames(out) <- c("Variable", "aCO2", "eCO2", "aCO2_conf", "eCO2_conf")
    
    ### create dataframe to hold bootstrap results - inout
    bDF1 <- data.frame(c(1:1000), NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(bDF1) <- c("bootID", "Wood", "Canopy", 
                        "FineRoot", "CoarseRoot", "Understorey", "Leaflitter", "Finerootlitter",
                        "Understoreylitter")
    bDF2 <- bDF1
    
    ## set seed
    set.seed(99)
    
    ### ambient
    bDF1$Wood <- rnorm(1000, mean=sumDF$aCO2[sumDF$term=="Wood P flux"],
                       sd=sumDF$aCO2_sd[sumDF$term=="Wood P flux"])
    bDF1$Canopy <- rnorm(1000, mean=sumDF$aCO2[sumDF$term=="Canopy P flux"],
                         sd=sumDF$aCO2_sd[sumDF$term=="Canopy P flux"])
    bDF1$FineRoot <- rnorm(1000, mean=sumDF$aCO2[sumDF$term=="Fine Root P flux"],
                           sd=sumDF$aCO2_sd[sumDF$term=="Fine Root P flux"])
    bDF1$CoarseRoot <- rnorm(1000, mean=sumDF$aCO2[sumDF$term=="Coarse Root P flux"],
                             sd=sumDF$aCO2_sd[sumDF$term=="Coarse Root P flux"])
    bDF1$Understorey <- rnorm(1000, mean=sumDF$aCO2[sumDF$term=="Understorey P flux"],
                              sd=sumDF$aCO2_sd[sumDF$term=="Understorey P flux"])
    bDF1$Leaflitter <- rnorm(1000, mean=sumDF$aCO2[sumDF$term=="Leaflitter P flux"],
                       sd=sumDF$aCO2_sd[sumDF$term=="Leaflitter P flux"])
    bDF1$Finerootlitter <- rnorm(1000, mean=sumDF$aCO2[sumDF$term=="Fineroot Litter P flux"],
                             sd=sumDF$aCO2_sd[sumDF$term=="Fineroot Litter P flux"])
    bDF1$Understoreylitter <- rnorm(1000, mean=sumDF$aCO2[sumDF$term=="Understorey Litter P flux"],
                             sd=sumDF$aCO2_sd[sumDF$term=="Understorey Litter P flux"])
    bDF1$Total <- with(bDF1, Wood+Canopy+CoarseRoot+FineRoot+Understorey-Leaflitter-Finerootlitter-Understoreylitter)
    
    ### elevated
    bDF2$Wood <- rnorm(1000, mean=sumDF$eCO2[sumDF$term=="Wood P flux"],
                       sd=sumDF$eCO2_sd[sumDF$term=="Wood P flux"])
    bDF2$Canopy <- rnorm(1000, mean=sumDF$eCO2[sumDF$term=="Canopy P flux"],
                         sd=sumDF$eCO2_sd[sumDF$term=="Canopy P flux"])
    bDF2$FineRoot <- rnorm(1000, mean=sumDF$eCO2[sumDF$term=="Fine Root P flux"],
                           sd=sumDF$eCO2_sd[sumDF$term=="Fine Root P flux"])
    bDF2$CoarseRoot <- rnorm(1000, mean=sumDF$eCO2[sumDF$term=="Coarse Root P flux"],
                             sd=sumDF$eCO2_sd[sumDF$term=="Coarse Root P flux"])
    bDF2$Understorey <- rnorm(1000, mean=sumDF$eCO2[sumDF$term=="Understorey P flux"],
                              sd=sumDF$eCO2_sd[sumDF$term=="Understorey P flux"])
    bDF2$Leaflitter <- rnorm(1000, mean=sumDF$eCO2[sumDF$term=="Leaflitter P flux"],
                             sd=sumDF$eCO2_sd[sumDF$term=="Leaflitter P flux"])
    bDF2$Finerootlitter <- rnorm(1000, mean=sumDF$eCO2[sumDF$term=="Fineroot Litter P flux"],
                                 sd=sumDF$eCO2_sd[sumDF$term=="Fineroot Litter P flux"])
    bDF2$Understoreylitter <- rnorm(1000, mean=sumDF$eCO2[sumDF$term=="Understorey Litter P flux"],
                                    sd=sumDF$eCO2_sd[sumDF$term=="Understorey Litter P flux"])
    bDF2$Total <- with(bDF2, Wood+Canopy+CoarseRoot+FineRoot+Understorey-Leaflitter-Finerootlitter-Understoreylitter)
    
    ### Calculate total
    out$aCO2[out$Variable=="Total P retranslocation"] <- mean(bDF1$Total)
    out$aCO2_conf[out$Variable=="Total P retranslocation"] <- sd(bDF1$Total)
    out$eCO2[out$Variable=="Total P retranslocation"] <- mean(bDF2$Total)
    out$eCO2_conf[out$Variable=="Total P retranslocation"] <- sd(bDF2$Total)
    
    return(out)
}
