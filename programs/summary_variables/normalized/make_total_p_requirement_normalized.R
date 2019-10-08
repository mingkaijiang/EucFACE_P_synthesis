make_total_p_requirement_table_bootstrap <- function(sumDF) {
    
    ### total requirement
    myDF <- sumDF[sumDF$terms != "Leaflitter P flux",]
    myDF <- myDF[myDF$terms != "Mineralization P flux",]
    myDF <- myDF[myDF$terms != "Understorey Litter P flux",]
    myDF <- myDF[myDF$terms != "Fineroot Litter P flux",]
    
    ### prepare output df
    out <- data.frame(c("Total P requirement"), NA, NA, NA, NA)
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
    
    return(out)
    
}