make_total_plant_standing_p_stock_bootstrap <- function(inDF) {

    ### prepare output df
    out <- data.frame(c("Overstorey aboveground", "Understorey aboveground", "Belowground", "Total plant standing"), NA, NA, NA, NA)
    colnames(out) <- c("Variable", "aCO2", "eCO2", "aCO2_conf", "eCO2_conf")
    
    ### create dataframe to hold bootstrap results - inout
    bDF1 <- data.frame(c(1:1000), NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(bDF1) <- c("bootID", "Wood", "Canopy", 
                        "FineRoot", "CoarseRoot", "Understorey", "OverstoreyAboveground", "UnderstoreyAboveground",
                        "Belowground", "Total")
    bDF2 <- bDF1
    
    ## set seed
    set.seed(99)
    
    ## ambient rings
    bDF1$Wood <- rnorm(1000, mean=inDF$aCO2[inDF$term=="Wood P Pool"],
                             sd=inDF$aCO2_sd[inDF$term=="Wood P Pool"])
    bDF1$Canopy <- rnorm(1000, mean=inDF$aCO2[inDF$term=="Canopy P Pool"],
                       sd=inDF$aCO2_sd[inDF$term=="Canopy P Pool"])
    bDF1$FineRoot <- rnorm(1000, mean=inDF$aCO2[inDF$term=="Fine Root P Pool"],
                       sd=inDF$aCO2_sd[inDF$term=="Fine Root P Pool"])
    bDF1$CoarseRoot <- rnorm(1000, mean=inDF$aCO2[inDF$term=="Coarse Root P Pool"],
                       sd=inDF$aCO2_sd[inDF$term=="Coarse Root P Pool"])
    bDF1$Understorey <- rnorm(1000, mean=inDF$aCO2[inDF$term=="Understorey P Pool"],
                       sd=inDF$aCO2_sd[inDF$term=="Understorey P Pool"])
    bDF1$OverstoreyAboveground <- with(bDF1, (Wood+Canopy))
    bDF1$Belowground <- with(bDF1, (FineRoot+CoarseRoot))
    bDF1$UnderstoreyAboveground <- bDF1$Understorey
    bDF1$Total <- with(bDF1, Wood+Canopy+CoarseRoot+FineRoot+Understorey)
    
    ## elevated rings
    bDF2$Wood <- rnorm(1000, mean=inDF$eCO2[inDF$term=="Wood P Pool"],
                       sd=inDF$eCO2_sd[inDF$term=="Wood P Pool"])
    bDF2$Canopy <- rnorm(1000, mean=inDF$eCO2[inDF$term=="Canopy P Pool"],
                         sd=inDF$eCO2_sd[inDF$term=="Canopy P Pool"])
    bDF2$FineRoot <- rnorm(1000, mean=inDF$eCO2[inDF$term=="Fine Root P Pool"],
                           sd=inDF$eCO2_sd[inDF$term=="Fine Root P Pool"])
    bDF2$CoarseRoot <- rnorm(1000, mean=inDF$eCO2[inDF$term=="Coarse Root P Pool"],
                             sd=inDF$eCO2_sd[inDF$term=="Coarse Root P Pool"])
    bDF2$Understorey <- rnorm(1000, mean=inDF$eCO2[inDF$term=="Understorey P Pool"],
                              sd=inDF$eCO2_sd[inDF$term=="Understorey P Pool"])
    bDF2$OverstoreyAboveground <- with(bDF2, (Wood+Canopy))
    bDF2$Belowground <- with(bDF2, (FineRoot+CoarseRoot))
    bDF2$UnderstoreyAboveground <- bDF2$Understorey
    bDF2$Total <- with(bDF2, Wood+Canopy+CoarseRoot+FineRoot+Understorey)
    
    ### Calculate total
    out$aCO2[out$Variable=="Overstorey aboveground"] <- mean(bDF1$OverstoreyAboveground)
    out$aCO2_conf[out$Variable=="Overstorey aboveground"] <- sd(bDF1$OverstoreyAboveground)
    out$eCO2[out$Variable=="Overstorey aboveground"] <- mean(bDF2$OverstoreyAboveground)
    out$eCO2_conf[out$Variable=="Overstorey aboveground"] <- sd(bDF2$OverstoreyAboveground)
    
    out$aCO2[out$Variable=="Understorey aboveground"] <- mean(bDF1$UnderstoreyAboveground)
    out$aCO2_conf[out$Variable=="Understorey aboveground"] <- sd(bDF1$UnderstoreyAboveground)
    out$eCO2[out$Variable=="Understorey aboveground"] <- mean(bDF2$UnderstoreyAboveground)
    out$eCO2_conf[out$Variable=="Understorey aboveground"] <- sd(bDF2$UnderstoreyAboveground)
    
    out$aCO2[out$Variable=="Belowground"] <- mean(bDF1$Belowground)
    out$aCO2_conf[out$Variable=="Belowground"] <- sd(bDF1$Belowground)
    out$eCO2[out$Variable=="Belowground"] <- mean(bDF2$Belowground)
    out$eCO2_conf[out$Variable=="Belowground"] <- sd(bDF2$Belowground)
    
    out$aCO2[out$Variable=="Total plant standing"] <- mean(bDF1$Total)
    out$aCO2_conf[out$Variable=="Total plant standing"] <- sd(bDF1$Total)
    out$eCO2[out$Variable=="Total plant standing"] <- mean(bDF2$Total)
    out$eCO2_conf[out$Variable=="Total plant standing"] <- sd(bDF2$Total)
    
    
    
    return(out)
}