#- Make the microbial P concentration
make_microbial_p_concentration <- function() {
    # return ring-specific, continuous microbial P concentration

    #Pmic,ug g-1,"Fumigation extraction with Bray P AQ2 determination of PO4.
    
    # download the data
    download_microbial_p_data()
    
    df1 <- read.csv(file.path(getToPath(), 
                             "FACE_P0014_RA_MicrobialBiomassCNP_L1_20120613-20151130.csv"))
    df <- read.csv("temp_files/FACE_P0014_RA_MicrobialBiomassCNP_L1_20120613-20151130_V2.csv")
    df$Pmic <- as.numeric(as.character(df$Pmic_2_noEF))
    df <- df[complete.cases(df$ring),]
    
    df$Date <- as.character(df$date)
    df$date <- as.Date(df$Date, format="%d/%m/%Y")    
        
    # first, averaged across depths, unit: ug g-1
    df.m <- summaryBy(Pmic~ring+date,
                      data=df,FUN=mean,keep.names=T,na.rm=T)
    
    
    df.m$PercP <- df.m$Pmic * 10^-4
    
    df.m <- df.m[!is.infinite(df.m$Pmic),] 
    df.m <- df.m[complete.cases(df.m),]
    
    df.out <- df.m[,c("date", "ring", "PercP")]
    colnames(df.out) <- c("Date", "Ring", "PercP")
    
    df.out$Depth <- "0_10"
    
    
    ### this is august/sep 2017 data
    newdf <- read.csv("temp_files/MASTER_Mic_P_biomass_P0091.csv")
    newdf$Date <- "2017-09-01"
    
    # to account for the original conversion factor of 0.4
    newdf$mic_P_mg.kg <- newdf$mic_P_mg.kg/0.4
    
    ### 
    newdf.m <- summaryBy(mic_P_mg.kg~ring+depth+Date, data=newdf, 
                         FUN=mean, na.rm=T, keep.names=T)
    #newdf.s <- subset(newdf.m, depth == "0-10")
    newdf.m$PercP <- newdf.m$mic_P_mg.kg * 10^-4
    newdf.out <- newdf.m[,c("Date", "ring", "PercP", "depth")]
    colnames(newdf.out) <- c("Date", "Ring", "PercP", "Depth")
    
    newdf.out$Depth <- gsub("0-10", "0_10", newdf.out$Depth)
    newdf.out$Depth <- gsub("10-30", "10_30", newdf.out$Depth)
    
    df.out <- rbind(df.out, newdf.out)
    
    
    ### plot
    p1 <- ggplot(df.out, aes(Date, PercP, group=Ring))+
        geom_point(aes(pch=Depth, col=Ring, fill=Ring))
    
    ### calculate average PercP per depth
    plotDF <- summaryBy(PercP~Depth, data=df.out, FUN=mean, keep.names=T, na.rm=T)
    
    p2 <- ggplot(plotDF, aes(Depth, PercP))+
        geom_point()
    
    ### subtract PercP in Johanna's dataset
    subDF <- subset(df.out, Date=="2017-09-01")
    
    p3 <- ggplot(subDF, aes(Depth, PercP))+
        geom_point()
    
    pdf("plots_tables/checks/MicP_check.pdf")
    plot(p1)
    plot(p2)
    plot(p3)
    dev.off()
    
    
    return(df.out)
    
}
