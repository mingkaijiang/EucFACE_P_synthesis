make_p_uptake_from_soil_bootstrap <- function(sumDF) {
    
    
    ### total requirement
    ### prepare output df
    out <- data.frame(c("Total P requirement", "Total P retranslocation", "Total P uptake",
                        "Uptake over Requirement"), NA, NA, NA, NA)
    colnames(out) <- c("Variable", "aCO2", "eCO2", "aCO2_conf", "eCO2_conf")
    
    ### create dataframe to hold bootstrap results - inout
    bDF1 <- data.frame(c(1:1000), NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(bDF1) <- c("bootID", "Wood", "Canopy", 
                        "FineRoot", "CoarseRoot", "Understorey", "Twig", "Bark",
                        "Seed", "Frass")
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
    bDF1$Twig <- rnorm(1000, mean=sumDF$aCO2[sumDF$term=="Twig litter P flux"],
                       sd=sumDF$aCO2_sd[sumDF$term=="Twig litter P flux"])
    bDF1$Bark <- rnorm(1000, mean=sumDF$aCO2[sumDF$term=="Bark litter P flux"],
                       sd=sumDF$aCO2_sd[sumDF$term=="Bark litter P flux"])
    bDF1$Seed <- rnorm(1000, mean=sumDF$aCO2[sumDF$term=="Seed litter P flux"],
                       sd=sumDF$aCO2_sd[sumDF$term=="Seed litter P flux"])
    bDF1$Frass <- rnorm(1000, mean=sumDF$aCO2[sumDF$term=="Frass P flux"],
                        sd=sumDF$aCO2_sd[sumDF$term=="Frass P flux"])
    bDF1$Total <- with(bDF1, Wood+Canopy+CoarseRoot+FineRoot+Understorey+Twig+Bark+Seed+Frass)
    
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
    bDF2$Twig <- rnorm(1000, mean=sumDF$eCO2[sumDF$term=="Twig litter P flux"],
                       sd=sumDF$eCO2_sd[sumDF$term=="Twig litter P flux"])
    bDF2$Bark <- rnorm(1000, mean=sumDF$eCO2[sumDF$term=="Bark litter P flux"],
                       sd=sumDF$eCO2_sd[sumDF$term=="Bark litter P flux"])
    bDF2$Seed <- rnorm(1000, mean=sumDF$eCO2[sumDF$term=="Seed litter P flux"],
                       sd=sumDF$eCO2_sd[sumDF$term=="Seed litter P flux"])
    bDF2$Frass <- rnorm(1000, mean=sumDF$eCO2[sumDF$term=="Frass P flux"],
                        sd=sumDF$eCO2_sd[sumDF$term=="Frass P flux"])
    bDF2$Total <- with(bDF2, Wood+Canopy+CoarseRoot+FineRoot+Understorey+Twig+Bark+Seed+Frass)
    
    ### Calculate total
    out$aCO2[out$Variable=="Total P requirement"] <- mean(bDF1$Total)
    out$aCO2_conf[out$Variable=="Total P requirement"] <- sd(bDF1$Total)
    out$eCO2[out$Variable=="Total P requirement"] <- mean(bDF2$Total)
    out$eCO2_conf[out$Variable=="Total P requirement"] <- sd(bDF2$Total)
    
    ### Calculate total p retranslocated
    ### (canopy P + wood P increment + fineroot P + understorey P + coarseroot P increment) 
    ### - litterfall P - fineroot litter P - understorey litter P
    
    ### create dataframe to hold bootstrap results - inout
    bDF3 <- data.frame(c(1:1000), NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(bDF3) <- c("bootID", "Wood", "Canopy", 
                        "FineRoot", "CoarseRoot", "Understorey", "Leaflitter", "Finerootlitter",
                        "Understoreylitter")
    bDF4 <- bDF3
    
    ### ambient
    bDF3$Wood <- rnorm(1000, mean=sumDF$aCO2[sumDF$term=="Wood P flux"],
                       sd=sumDF$aCO2_sd[sumDF$term=="Wood P flux"])
    bDF3$Canopy <- rnorm(1000, mean=sumDF$aCO2[sumDF$term=="Canopy P flux"],
                         sd=sumDF$aCO2_sd[sumDF$term=="Canopy P flux"])
    bDF3$FineRoot <- rnorm(1000, mean=sumDF$aCO2[sumDF$term=="Fine Root P flux"],
                           sd=sumDF$aCO2_sd[sumDF$term=="Fine Root P flux"])
    bDF3$CoarseRoot <- rnorm(1000, mean=sumDF$aCO2[sumDF$term=="Coarse Root P flux"],
                             sd=sumDF$aCO2_sd[sumDF$term=="Coarse Root P flux"])
    bDF3$Understorey <- rnorm(1000, mean=sumDF$aCO2[sumDF$term=="Understorey P flux"],
                              sd=sumDF$aCO2_sd[sumDF$term=="Understorey P flux"])
    bDF3$Leaflitter <- rnorm(1000, mean=sumDF$aCO2[sumDF$term=="Leaflitter P flux"],
                             sd=sumDF$aCO2_sd[sumDF$term=="Leaflitter P flux"])
    bDF3$Finerootlitter <- rnorm(1000, mean=sumDF$aCO2[sumDF$term=="Fineroot Litter P flux"],
                                 sd=sumDF$aCO2_sd[sumDF$term=="Fineroot Litter P flux"])
    bDF3$Understoreylitter <- rnorm(1000, mean=sumDF$aCO2[sumDF$term=="Understorey Litter P flux"],
                                    sd=sumDF$aCO2_sd[sumDF$term=="Understorey Litter P flux"])
    bDF3$Total <- with(bDF3, Wood+Canopy+CoarseRoot+FineRoot+Understorey-Leaflitter-Finerootlitter-Understoreylitter)
    
    ### elevated
    bDF4$Wood <- rnorm(1000, mean=sumDF$eCO2[sumDF$term=="Wood P flux"],
                       sd=sumDF$eCO2_sd[sumDF$term=="Wood P flux"])
    bDF4$Canopy <- rnorm(1000, mean=sumDF$eCO2[sumDF$term=="Canopy P flux"],
                         sd=sumDF$eCO2_sd[sumDF$term=="Canopy P flux"])
    bDF4$FineRoot <- rnorm(1000, mean=sumDF$eCO2[sumDF$term=="Fine Root P flux"],
                           sd=sumDF$eCO2_sd[sumDF$term=="Fine Root P flux"])
    bDF4$CoarseRoot <- rnorm(1000, mean=sumDF$eCO2[sumDF$term=="Coarse Root P flux"],
                             sd=sumDF$eCO2_sd[sumDF$term=="Coarse Root P flux"])
    bDF4$Understorey <- rnorm(1000, mean=sumDF$eCO2[sumDF$term=="Understorey P flux"],
                              sd=sumDF$eCO2_sd[sumDF$term=="Understorey P flux"])
    bDF4$Leaflitter <- rnorm(1000, mean=sumDF$eCO2[sumDF$term=="Leaflitter P flux"],
                             sd=sumDF$eCO2_sd[sumDF$term=="Leaflitter P flux"])
    bDF4$Finerootlitter <- rnorm(1000, mean=sumDF$eCO2[sumDF$term=="Fineroot Litter P flux"],
                                 sd=sumDF$eCO2_sd[sumDF$term=="Fineroot Litter P flux"])
    bDF4$Understoreylitter <- rnorm(1000, mean=sumDF$eCO2[sumDF$term=="Understorey Litter P flux"],
                                    sd=sumDF$eCO2_sd[sumDF$term=="Understorey Litter P flux"])
    bDF4$Total <- with(bDF4, Wood+Canopy+CoarseRoot+FineRoot+Understorey-Leaflitter-Finerootlitter-Understoreylitter)
    
    ### Calculate total
    out$aCO2[out$Variable=="Total P retranslocation"] <- mean(bDF3$Total)
    out$aCO2_conf[out$Variable=="Total P retranslocation"] <- sd(bDF3$Total)
    out$eCO2[out$Variable=="Total P retranslocation"] <- mean(bDF4$Total)
    out$eCO2_conf[out$Variable=="Total P retranslocation"] <- sd(bDF4$Total)
    
    ### Total P uptake
    ### create dataframe to hold bootstrap results - inout
    bDF5 <- data.frame(bDF1$Total, bDF3$Total)
    bDF6 <- data.frame(bDF2$Total, bDF4$Total)
    
    colnames(bDF5) <- colnames(bDF6) <- c("Requirement", "Retranslocation")
    
    bDF5$Uptake <- with(bDF5, Requirement - Retranslocation)
    bDF6$Uptake <- with(bDF6, Requirement - Retranslocation)
    
    bDF5$UptakeOverRequ <- with(bDF5, Uptake / Requirement)
    bDF6$UptakeOverRequ <- with(bDF6, Uptake / Requirement)
    
    ### Calculate total
    out$aCO2[out$Variable=="Total P uptake"] <- mean(bDF5$Uptake)
    out$aCO2_conf[out$Variable=="Total P uptake"] <- sd(bDF5$Uptake)
    out$eCO2[out$Variable=="Total P uptake"] <- mean(bDF6$Uptake)
    out$eCO2_conf[out$Variable=="Total P uptake"] <- sd(bDF6$Uptake)
    
    ### uptake over requirement
    out$aCO2[out$Variable=="Uptake over Requirement"] <- mean(bDF5$UptakeOverRequ)
    out$aCO2_conf[out$Variable=="Uptake over Requirement"] <- sd(bDF5$UptakeOverRequ)
    out$eCO2[out$Variable=="Uptake over Requirement"] <- mean(bDF6$UptakeOverRequ)
    out$eCO2_conf[out$Variable=="Uptake over Requirement"] <- sd(bDF6$UptakeOverRequ)
    
    return(out)
}