estimate_fineroot_fractions <- function(bkDF) {
    
    
    ### use Johanna's data to estimate the relative contribution
    ### of fineroot (< 2mm) coarseroot (2-3 mm) to total root
    myDF <- read.csv("temp_files/EucFACE_P0091_roots_SEP2017.csv")
    myDF$depth <- gsub("0-10 cm", "0_10", myDF$depth)
    myDF$depth <- gsub("10-30 cm", "10_30", myDF$depth)
    myDF$depth <- gsub("30-x", "transition", myDF$depth)
    names(myDF)[names(myDF)=="depth"] <- "Depth"
    
    myDF <- merge(myDF, bkDF, by.x=c("ring", "Depth"),
                  by.y=c("Ring","Depth"))
    
    myDF$root1 <- myDF$root.smal.2.mm.mg.g * myDF$bulk_density_kg_m3
    myDF$root2 <- myDF$root.2t3.mm.mg.g * myDF$bulk_density_kg_m3
    myDF$root3 <- myDF$root.large.3.mm.mg.g * myDF$bulk_density_kg_m3
    
    myDF$root1 <- ifelse(myDF$Depth=="0_10", myDF$root1*0.1,
                         ifelse(myDF$Depth=="10_30", myDF$root1*0.2, myDF$root1*0.3))
    
    myDF$root2 <- ifelse(myDF$Depth=="0_10", myDF$root2*0.1,
                         ifelse(myDF$Depth=="10_30", myDF$root2*0.2, myDF$root2*0.3))
    
    myDF$root3 <- ifelse(myDF$Depth=="0_10", myDF$root3*0.1,
                         ifelse(myDF$Depth=="10_30", myDF$root3*0.2, myDF$root3*0.3))
    

    outDF1 <- summaryBy(root1+root2+root3~ring+Depth, data=myDF, FUN=mean,
                        na.rm=T, keep.names=T)
    outDF1$Date <- as.Date("2017-09-01")
    names(outDF1)[names(outDF1)=="ring"] <- "Ring"
    names(outDF1)[names(outDF1)=="root1"] <- "FRB_small"
    names(outDF1)[names(outDF1)=="root2"] <- "FRB_intermediate"
    names(outDF1)[names(outDF1)=="root3"] <- "FRB_large"
    
    
    ### calculate fraction
    outDF1$frac_intermediate <- outDF1$FRB_intermediate/outDF1$FRB_small
    outDF1$frac_large <- outDF1$FRB_large/outDF1$FRB_small
    
    ### return
    return(outDF1)
    
    
}