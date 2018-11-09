make_total_p_budgeting_variables_bootstrap <- function() {
    #### This function calculates all P budgeting variables
    
    ### total requirement
    ### prepare output df
    out <- data.frame(c("Total P requirement", "Total P retranslocation", "Total P uptake",
                        "Uptake over Requirement", "Stand P OA", 
                        "Stand P UA", "Stand P Belowground", "Total plant standing P", 
                        "MRT", "Standing PUE", "Soil P mineralization"), NA, NA, NA, NA)
    colnames(out) <- c("Variable", "aCO2", "eCO2", "aCO2_conf", "eCO2_conf")
    
    ### create dataframe to hold bootstrap results - inout
    bDF1 <- data.frame(c(1:1000), NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(bDF1) <- c("bootID", "Wood", "Canopy", 
                        "FineRoot", "CoarseRoot", "Understorey", "Twig", "Bark",
                        "Seed", "Frass")
    bDF2 <- bDF1
    
    ## set seed
    set.seed(123)
    
    sumDF <- summary_table_flux_by_treatment_bootstrap
    
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
    
    
    ### Standing P
    inDF <- summary_table_pool_by_treatment_bootstrap
    
    ### create dataframe to hold bootstrap results - inout
    bDF7 <- data.frame(c(1:1000), NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(bDF7) <- c("bootID", "Wood", "Canopy", 
                        "FineRoot", "CoarseRoot", "Understorey", "OverstoreyAboveground", "UnderstoreyAboveground",
                        "Belowground", "Total")
    bDF8 <- bDF7

    
    ## ambient rings
    bDF7$Wood <- rnorm(1000, mean=inDF$aCO2[inDF$term=="Wood P Pool"],
                       sd=inDF$aCO2_sd[inDF$term=="Wood P Pool"])
    bDF7$Canopy <- rnorm(1000, mean=inDF$aCO2[inDF$term=="Canopy P Pool"],
                         sd=inDF$aCO2_sd[inDF$term=="Canopy P Pool"])
    bDF7$FineRoot <- rnorm(1000, mean=inDF$aCO2[inDF$term=="Fine Root P Pool"],
                           sd=inDF$aCO2_sd[inDF$term=="Fine Root P Pool"])
    bDF7$CoarseRoot <- rnorm(1000, mean=inDF$aCO2[inDF$term=="Coarse Root P Pool"],
                             sd=inDF$aCO2_sd[inDF$term=="Coarse Root P Pool"])
    bDF7$Understorey <- rnorm(1000, mean=inDF$aCO2[inDF$term=="Understorey P Pool"],
                              sd=inDF$aCO2_sd[inDF$term=="Understorey P Pool"])
    bDF7$OverstoreyAboveground <- with(bDF7, (Wood+Canopy))
    bDF7$Belowground <- with(bDF7, (FineRoot+CoarseRoot))
    bDF7$UnderstoreyAboveground <- bDF7$Understorey
    bDF7$Total <- with(bDF7, Wood+Canopy+CoarseRoot+FineRoot+Understorey)
    
    ## elevated rings
    bDF8$Wood <- rnorm(1000, mean=inDF$eCO2[inDF$term=="Wood P Pool"],
                       sd=inDF$eCO2_sd[inDF$term=="Wood P Pool"])
    bDF8$Canopy <- rnorm(1000, mean=inDF$eCO2[inDF$term=="Canopy P Pool"],
                         sd=inDF$eCO2_sd[inDF$term=="Canopy P Pool"])
    bDF8$FineRoot <- rnorm(1000, mean=inDF$eCO2[inDF$term=="Fine Root P Pool"],
                           sd=inDF$eCO2_sd[inDF$term=="Fine Root P Pool"])
    bDF8$CoarseRoot <- rnorm(1000, mean=inDF$eCO2[inDF$term=="Coarse Root P Pool"],
                             sd=inDF$eCO2_sd[inDF$term=="Coarse Root P Pool"])
    bDF8$Understorey <- rnorm(1000, mean=inDF$eCO2[inDF$term=="Understorey P Pool"],
                              sd=inDF$eCO2_sd[inDF$term=="Understorey P Pool"])
    bDF8$OverstoreyAboveground <- with(bDF8, (Wood+Canopy))
    bDF8$Belowground <- with(bDF8, (FineRoot+CoarseRoot))
    bDF8$UnderstoreyAboveground <- bDF8$Understorey
    bDF8$Total <- with(bDF8, Wood+Canopy+CoarseRoot+FineRoot+Understorey)
    
    ### Calculate total
    out$aCO2[out$Variable=="Stand P OA"] <- mean(bDF7$OverstoreyAboveground)
    out$aCO2_conf[out$Variable=="Stand P OA"] <- sd(bDF7$OverstoreyAboveground)
    out$eCO2[out$Variable=="Stand P OA"] <- mean(bDF8$OverstoreyAboveground)
    out$eCO2_conf[out$Variable=="Stand P OA"] <- sd(bDF8$OverstoreyAboveground)
    
    out$aCO2[out$Variable=="Stand P UA"] <- mean(bDF7$UnderstoreyAboveground)
    out$aCO2_conf[out$Variable=="Stand P UA"] <- sd(bDF7$UnderstoreyAboveground)
    out$eCO2[out$Variable=="Stand P UA"] <- mean(bDF8$UnderstoreyAboveground)
    out$eCO2_conf[out$Variable=="Stand P UA"] <- sd(bDF8$UnderstoreyAboveground)
    
    out$aCO2[out$Variable=="Stand P Belowground"] <- mean(bDF7$Belowground)
    out$aCO2_conf[out$Variable=="Stand P Belowground"] <- sd(bDF7$Belowground)
    out$eCO2[out$Variable=="Stand P Belowground"] <- mean(bDF8$Belowground)
    out$eCO2_conf[out$Variable=="Stand P Belowground"] <- sd(bDF8$Belowground)
    
    out$aCO2[out$Variable=="Total plant standing P"] <- mean(bDF7$Total)
    out$aCO2_conf[out$Variable=="Total plant standing P"] <- sd(bDF7$Total)
    out$eCO2[out$Variable=="Total plant standing P"] <- mean(bDF8$Total)
    out$eCO2_conf[out$Variable=="Total plant standing P"] <- sd(bDF8$Total)
    
    
    ### Calculate MRT, standing biomass / uptake rate
    bDF7$Uptake <- bDF5$Uptake
    bDF8$Uptake <- bDF6$Uptake
    
    bDF7$MRT <- with(bDF7, Total/Uptake)
    bDF8$MRT <- with(bDF8, Total/Uptake)
    
    out$aCO2[out$Variable=="MRT"] <- mean(bDF7$MRT)
    out$aCO2_conf[out$Variable=="MRT"] <- sd(bDF7$MRT)
    out$eCO2[out$Variable=="MRT"] <- mean(bDF8$MRT)
    out$eCO2_conf[out$Variable=="MRT"] <- sd(bDF8$MRT)
    
    ### Calculate standing PUE, i.e. NPP / P uptake
    cDF <- summary_table_c_flux_by_treatment_bootstrap
 
    bDF9 <- data.frame(c(1:1000), NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(bDF9) <- c("bootID", "Wood", "Canopy", 
                        "FineRoot", "CoarseRoot", "Understorey", "Twig", "Bark",
                        "Seed", "Frass")
    bDF10 <- bDF9
    
    ## ambient rings
    bDF9$Wood <- rnorm(1000, mean=cDF$aCO2[cDF$term=="Wood C flux"],
                       sd=cDF$aCO2_sd[cDF$term=="Wood C flux"])
    bDF9$Canopy <- rnorm(1000, mean=cDF$aCO2[cDF$term=="Canopy C flux"],
                       sd=cDF$aCO2_sd[cDF$term=="Canopy C flux"])
    bDF9$FineRoot <- rnorm(1000, mean=cDF$aCO2[cDF$term=="Fine Root C flux"],
                       sd=cDF$aCO2_sd[cDF$term=="Fine Root C flux"])
    bDF9$CoarseRoot <- rnorm(1000, mean=cDF$aCO2[cDF$term=="Coarse Root C flux"],
                       sd=cDF$aCO2_sd[cDF$term=="Coarse Root C flux"])
    bDF9$Understorey <- rnorm(1000, mean=cDF$aCO2[cDF$term=="Understorey C flux"],
                       sd=cDF$aCO2_sd[cDF$term=="Understorey C flux"])
    bDF9$Twig <- rnorm(1000, mean=cDF$aCO2[cDF$term=="Twiglitter C flux"],
                       sd=cDF$aCO2_sd[cDF$term=="Twiglitter C flux"])
    bDF9$Bark <- rnorm(1000, mean=cDF$aCO2[cDF$term=="Barklitter C flux"],
                       sd=cDF$aCO2_sd[cDF$term=="Barklitter C flux"])
    bDF9$Seed <- rnorm(1000, mean=cDF$aCO2[cDF$term=="Seedlitter C flux"],
                       sd=cDF$aCO2_sd[cDF$term=="Seedlitter C flux"])
    bDF9$Frass <- rnorm(1000, mean=cDF$aCO2[cDF$term=="Frass C flux"],
                       sd=cDF$aCO2_sd[cDF$term=="Frass C flux"])
    bDF9$Total <- with(bDF9, Wood+Canopy+CoarseRoot+FineRoot+Understorey+Twig+Bark+Seed+Frass)
    
    ## elevated rings
    bDF10$Wood <- rnorm(1000, mean=cDF$eCO2[cDF$term=="Wood C flux"],
                       sd=cDF$eCO2_sd[cDF$term=="Wood C flux"])
    bDF10$Canopy <- rnorm(1000, mean=cDF$eCO2[cDF$term=="Canopy C flux"],
                         sd=cDF$eCO2_sd[cDF$term=="Canopy C flux"])
    bDF10$FineRoot <- rnorm(1000, mean=cDF$eCO2[cDF$term=="Fine Root C flux"],
                           sd=cDF$eCO2_sd[cDF$term=="Fine Root C flux"])
    bDF10$CoarseRoot <- rnorm(1000, mean=cDF$eCO2[cDF$term=="Coarse Root C flux"],
                             sd=cDF$eCO2_sd[cDF$term=="Coarse Root C flux"])
    bDF10$Understorey <- rnorm(1000, mean=cDF$eCO2[cDF$term=="Understorey C flux"],
                              sd=cDF$eCO2_sd[cDF$term=="Understorey C flux"])
    bDF10$Twig <- rnorm(1000, mean=cDF$eCO2[cDF$term=="Twiglitter C flux"],
                       sd=cDF$eCO2_sd[cDF$term=="Twiglitter C flux"])
    bDF10$Bark <- rnorm(1000, mean=cDF$eCO2[cDF$term=="Barklitter C flux"],
                       sd=cDF$eCO2_sd[cDF$term=="Barklitter C flux"])
    bDF10$Seed <- rnorm(1000, mean=cDF$eCO2[cDF$term=="Seedlitter C flux"],
                       sd=cDF$eCO2_sd[cDF$term=="Seedlitter C flux"])
    bDF10$Frass <- rnorm(1000, mean=cDF$eCO2[cDF$term=="Frass C flux"],
                        sd=cDF$eCO2_sd[cDF$term=="Frass C flux"])
    bDF10$Total <- with(bDF10, Wood+Canopy+CoarseRoot+FineRoot+Understorey+Twig+Bark+Seed+Frass)
    
    bDF9$Puptake <- bDF7$Uptake
    bDF10$Puptake <- bDF8$Uptake
    
    bDF9$PUE <- with(bDF9, Total/Puptake)
    bDF10$PUE <- with(bDF10, Total/Puptake)
    
    out$aCO2[out$Variable=="Standing PUE"] <- mean(bDF9$PUE)
    out$aCO2_conf[out$Variable=="Standing PUE"] <- sd(bDF9$PUE)
    out$eCO2[out$Variable=="Standing PUE"] <- mean(bDF10$PUE)
    out$eCO2_conf[out$Variable=="Standing PUE"] <- sd(bDF10$PUE)
    
    
    ### Soil P mineralization flux
    mDF1 <- summaryBy(predicted~Trt, data=soil_p_mineralization_pred, FUN=mean, keep.names=T)
    mDF2 <- summaryBy(predicted~Trt, data=soil_p_mineralization_pred, FUN=sd, keep.names=T)
    mDF1$sd <- mDF2$predicted
    colnames(mDF1) <- c("Trt", "mean", "sd")
    
    bDF11 <- data.frame(c(1:1000), NA, NA)
    colnames(bDF11) <- c("bootID", "aCO2", "eCO2")

    bDF11$aCO2 <- rnorm(1000, mean=mDF1$mean[mDF1$Trt=="amb"],
                        sd=mDF1$sd[mDF1$Trt=="amb"])
    bDF11$eCO2 <- rnorm(1000, mean=mDF1$mean[mDF1$Trt=="ele"],
                        sd=mDF1$sd[mDF1$Trt=="ele"])
    
    out$aCO2[out$Variable=="Soil P mineralization"] <- mean(bDF11$aCO2)
    out$aCO2_conf[out$Variable=="Soil P mineralization"] <- sd(bDF11$aCO2)
    out$eCO2[out$Variable=="Soil P mineralization"] <- mean(bDF11$eCO2)
    out$eCO2_conf[out$Variable=="Soil P mineralization"] <- sd(bDF11$eCO2)
    
    
    return(out)
    
}