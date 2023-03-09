#- Make the microbial P concentration
make_microbial_p_concentration <- function() {
    # return ring-specific, continuous microbial P concentration

    #Pmic,ug g-1,"Fumigation extraction with Bray P AQ2 determination of PO4.
    
    # download the data
    download_microbial_p_data()
    
    #df1 <- read.csv(file.path(getToPath(), 
    #                         "FACE_P0014_RA_MicrobialBiomassCNP_L1_20120613-20151130.csv"))
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
    newdf.out$Depth <- gsub(" 10-30", "10_30", newdf.out$Depth)
    
    
    ### calculate trt-averaged depth reduction
    newdf.out$Trt[newdf.out$Ring%in%c(1,4,5)] <- "eCO2"
    newdf.out$Trt[newdf.out$Ring%in%c(2,3,6)] <- "aCO2"
    
    #tmpDF2 <- summaryBy(PercP~Depth+Trt, FUN=mean, data=newdf.out, keep.names=T)
    #
    #tmpDF2$Red[tmpDF2$Trt=="aCO2"&tmpDF2$Depth=="10_30"] <- tmpDF2$PercP[tmpDF2$Trt=="aCO2"&tmpDF2$Depth=="10_30"]/tmpDF2$PercP[tmpDF2$Trt=="aCO2"&tmpDF2$Depth=="0_10"]
    #tmpDF2$Red[tmpDF2$Trt=="eCO2"&tmpDF2$Depth=="10_30"] <- tmpDF2$PercP[tmpDF2$Trt=="eCO2"&tmpDF2$Depth=="10_30"]/tmpDF2$PercP[tmpDF2$Trt=="eCO2"&tmpDF2$Depth=="0_10"]
    #
    #tmpDF2$Red[tmpDF2$Trt=="aCO2"&tmpDF2$Depth=="transition"] <- tmpDF2$PercP[tmpDF2$Trt=="aCO2"&tmpDF2$Depth=="transition"]/tmpDF2$PercP[tmpDF2$Trt=="aCO2"&tmpDF2$Depth=="0_10"]
    #tmpDF2$Red[tmpDF2$Trt=="eCO2"&tmpDF2$Depth=="transition"] <- tmpDF2$PercP[tmpDF2$Trt=="eCO2"&tmpDF2$Depth=="transition"]/tmpDF2$PercP[tmpDF2$Trt=="eCO2"&tmpDF2$Depth=="0_10"]
    
    
    tmpDF2 <- summaryBy(PercP~Depth+Ring, FUN=mean, data=newdf.out, keep.names=T)
    
    for (i in 1:6) {
        tmpDF2$Red[tmpDF2$Ring==i&tmpDF2$Depth=="10_30"] <- tmpDF2$PercP[tmpDF2$Ring==i&tmpDF2$Depth=="10_30"]/tmpDF2$PercP[tmpDF2$Ring==i&tmpDF2$Depth=="0_10"]
        tmpDF2$Red[tmpDF2$Ring==i&tmpDF2$Depth=="transition"] <- tmpDF2$PercP[tmpDF2$Ring==i&tmpDF2$Depth=="transition"]/tmpDF2$PercP[tmpDF2$Ring==i&tmpDF2$Depth=="0_10"]
        
    }
    
    
    # update earlier datasets
    df.out$Trt[df.out$Ring%in%c(1,4,5)] <- "eCO2"
    df.out$Trt[df.out$Ring%in%c(2,3,6)] <- "aCO2"
    
    df.out2 <- df.out3 <- df.out
    df.out2$Depth <- "10_30"
    df.out3$Depth <- "transition"
    
    #df.out2$Rev[df.out2$Trt=="aCO2"] <- df.out2$PercP[df.out2$Trt=="aCO2"] * tmpDF2$Red[tmpDF2$Trt=="aCO2"&tmpDF2$Depth=="10_30"]
    #df.out2$Rev[df.out2$Trt=="eCO2"] <- df.out2$PercP[df.out2$Trt=="eCO2"] * tmpDF2$Red[tmpDF2$Trt=="eCO2"&tmpDF2$Depth=="10_30"]
    #
    #df.out3$Rev[df.out3$Trt=="aCO2"] <- df.out3$PercP[df.out3$Trt=="aCO2"] * tmpDF2$Red[tmpDF2$Trt=="aCO2"&tmpDF2$Depth=="transition"]
    #df.out3$Rev[df.out3$Trt=="eCO2"] <- df.out3$PercP[df.out3$Trt=="eCO2"] * tmpDF2$Red[tmpDF2$Trt=="eCO2"&tmpDF2$Depth=="transition"]
    
    for (i in 1:6) {
        df.out2$Rev[df.out2$Ring==i] <- df.out2$PercP[df.out2$Ring==i] * tmpDF2$Red[tmpDF2$Ring==i&tmpDF2$Depth=="10_30"]
        df.out3$Rev[df.out3$Ring==i] <- df.out3$PercP[df.out3$Ring==i] * tmpDF2$Red[tmpDF2$Ring==i&tmpDF2$Depth=="transition"]
        
    }
    
    
    df.out2 <- df.out2[,c("Date", "Ring", "Depth", "Rev")]
    df.out3 <- df.out3[,c("Date", "Ring", "Depth", "Rev")]
    colnames(df.out2) <- colnames(df.out3) <- c("Date", "Ring", "Depth", "PercP")
    
    df.out <- df.out[,c("Date", "Ring", "Depth", "PercP")]
    
    updDF <- rbind(df.out, rbind(df.out2, df.out3))
    
    newdf.out <- newdf.out[,c("Date", "Ring", "Depth", "PercP")]
    
    df.out <- rbind(updDF, newdf.out)
    
    df.out <- df.out[order(df.out$Date, df.out$Ring, df.out$Depth),]
    
    
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
