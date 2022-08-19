make_yearly_delta_pool_with_depth_function_soil_p <- function(inDF,var.col) {
    
    ### extract start and end date
    s.date <- min(inDF$Date)
    e.date <- max(inDF$Date)
    
    ### Change column name of value variable
    colnames(inDF)[var.col] <- "Value"
    
    ### Add year
    inDF$Year <- year(inDF$Date)
    
    
    ### subset two datasets
    inDF1 <- inDF[inDF$Date%in%c(as.Date("2013-03-11"), as.Date("2014-03-10")),]
    inDF2 <- inDF[inDF$Date%in%c(as.Date("2012-06-17"), as.Date("2013-06-11")),]
    inDF3 <- inDF[inDF$Date%in%c(as.Date("2012-09-03"), as.Date("2013-09-03"),as.Date("2018-10-01")),]
    
    myDF1 <- summaryBy(Value~Year+Ring+Depth, FUN=mean, data=inDF1, keep.names=T)
    myDF2 <- summaryBy(Value~Year+Ring+Depth, FUN=mean, data=inDF2, keep.names=T)
    myDF3 <- summaryBy(Value~Year+Ring+Depth, FUN=mean, data=inDF3, keep.names=T)
    
    ### date list
    d.list1 <- unique(myDF1$Year)
    d.list1 <- d.list1[order(d.list1)]
    
    d.list2 <- unique(myDF2$Year)
    d.list2 <- d.list2[order(d.list2)]
    
    d.list3 <- unique(myDF3$Year)
    d.list3 <- d.list3[order(d.list3)]
    
    
    ### create depth profiles
    subDF1.1 <- subset(myDF1, Depth=="0_10")
    subDF1.2 <- subset(myDF1, Depth=="10_30")
    subDF1.3 <- subset(myDF1, Depth=="transition")
    
    subDF2.1 <- subset(myDF2, Depth=="0_10")
    subDF2.2 <- subset(myDF2, Depth=="10_30")
    subDF2.3 <- subset(myDF2, Depth=="transition")
    
    subDF3.1 <- subset(myDF3, Depth=="0_10")
    subDF3.2 <- subset(myDF3, Depth=="10_30")
    subDF3.3 <- subset(myDF3, Depth=="transition")
    
    
    ## unique list
    d.list1.1 <- unique(subDF1.1$Year)
    d.list1.2 <- unique(subDF1.2$Year)
    d.list1.3 <- unique(subDF1.3$Year)
    
    d.list2.1 <- unique(subDF2.1$Year)
    d.list2.2 <- unique(subDF2.2$Year)
    d.list2.3 <- unique(subDF2.3$Year)
    
    d.list3.1 <- unique(subDF3.1$Year)
    d.list3.2 <- unique(subDF3.2$Year)
    d.list3.3 <- unique(subDF3.3$Year)
    
    
    
    ### create delta df
    delta1.1 <- subset(subDF1.1, Year != d.list1.1[1])
    delta1.1$Start_year <- delta1.1$Year  
    
    delta1.2 <- subset(subDF1.2, Year != d.list1.2[1])
    delta1.2$Start_year <- delta1.2$Year  
    
    delta1.3 <- subset(subDF1.3, Year != d.list1.3[1])
    delta1.3$Start_year <- delta1.3$Year  
    
    
    delta2.1 <- subset(subDF2.1, Year != d.list2.1[1])
    delta2.1$Start_year <- delta2.1$Year  
    
    delta2.2 <- subset(subDF2.2, Year != d.list2.2[1])
    delta2.2$Start_year <- delta2.2$Year  
    
    delta2.3 <- subset(subDF2.3, Year != d.list2.3[1])
    delta2.3$Start_year <- delta2.3$Year  
    
    
    delta3.1 <- subset(subDF3.1, Year != d.list3.1[1])
    delta3.1$Start_year <- delta3.1$Year  
    
    delta3.2 <- subset(subDF3.2, Year != d.list3.2[1])
    delta3.2$Start_year <- delta3.2$Year  
    
    delta3.3 <- subset(subDF3.3, Year != d.list3.3[1])
    delta3.3$Start_year <- delta3.3$Year  
    
    
    #### calculate differences
    for (i in 1:length(delta1.1$Year)) {
        delta1.1$Start_year[i] <- d.list1.1[which(d.list1.1 == delta1.1$Year[i]) - 1]
        delta1.1$prev_biom[i] <- subDF1.1$Value[subDF1.1$Ring == delta1.1$Ring[i] &
                                             as.numeric(subDF1.1$Year-delta1.1$Start_year[i])==0]
    }
    
    
    for (i in 1:length(delta1.2$Year)) {
        delta1.2$Start_year[i] <- d.list1.2[which(d.list1.2 == delta1.2$Year[i]) - 1]
        delta1.2$prev_biom[i] <- subDF1.2$Value[subDF1.2$Ring == delta1.2$Ring[i] &
                                                as.numeric(subDF1.2$Year-delta1.2$Start_year[i])==0]
    }
    
    for (i in 1:length(delta1.3$Year)) {
        delta1.3$Start_year[i] <- d.list1.3[which(d.list1.3 == delta1.3$Year[i]) - 1]
        delta1.3$prev_biom[i] <- subDF1.3$Value[subDF1.3$Ring == delta1.3$Ring[i] &
                                                as.numeric(subDF1.3$Year-delta1.3$Start_year[i])==0]
    }
    
    
    
    for (i in 1:length(delta2.1$Year)) {
        delta2.1$Start_year[i] <- d.list2.1[which(d.list2.1 == delta2.1$Year[i]) - 1]
        delta2.1$prev_biom[i] <- subDF2.1$Value[subDF2.1$Ring == delta2.1$Ring[i] &
                                                    as.numeric(subDF2.1$Year-delta2.1$Start_year[i])==0]
    }
    
    
    for (i in 1:length(delta2.2$Year)) {
        delta2.2$Start_year[i] <- d.list2.2[which(d.list2.2 == delta2.2$Year[i]) - 1]
        delta2.2$prev_biom[i] <- subDF2.2$Value[subDF2.2$Ring == delta2.2$Ring[i] &
                                                    as.numeric(subDF2.2$Year-delta2.2$Start_year[i])==0]
    }
    
    for (i in 1:length(delta2.3$Year)) {
        delta2.3$Start_year[i] <- d.list2.3[which(d.list2.3 == delta2.3$Year[i]) - 1]
        delta2.3$prev_biom[i] <- subDF2.3$Value[subDF2.3$Ring == delta2.3$Ring[i] &
                                                    as.numeric(subDF2.3$Year-delta2.3$Start_year[i])==0]
    }
    
    
    
    for (i in 1:length(delta3.1$Year)) {
        delta3.1$Start_year[i] <- d.list3.1[which(d.list3.1 == delta3.1$Year[i]) - 1]
        delta3.1$prev_biom[i] <- subDF3.1$Value[subDF3.1$Ring == delta3.1$Ring[i] &
                                                    as.numeric(subDF3.1$Year-delta3.1$Start_year[i])==0]
    }
    
    
    for (i in 1:length(delta3.2$Year)) {
        delta3.2$Start_year[i] <- d.list2.2[which(d.list3.2 == delta3.2$Year[i]) - 1]
        delta3.2$prev_biom[i] <- subDF2.2$Value[subDF3.2$Ring == delta3.2$Ring[i] &
                                                    as.numeric(subDF3.2$Year-delta3.2$Start_year[i])==0]
    }
    
    for (i in 1:length(delta3.3$Year)) {
        delta3.3$Start_year[i] <- d.list2.3[which(d.list3.3 == delta3.3$Year[i]) - 1]
        delta3.3$prev_biom[i] <- subDF2.3$Value[subDF3.3$Ring == delta3.3$Ring[i] &
                                                    as.numeric(subDF3.3$Year-delta3.3$Start_year[i])==0]
    }
    
    # add depth
    delta1.1$Depth <- "0_10"
    delta1.2$Depth <- "10_30"
    delta1.3$Depth <- "transition"
    
    delta2.1$Depth <- "0_10"
    delta2.2$Depth <- "10_30"
    delta2.3$Depth <- "transition"
    
    delta3.1$Depth <- "0_10"
    delta3.2$Depth <- "10_30"
    delta3.3$Depth <- "transition"
    
    delta1 <- rbind(delta1.1, rbind(delta1.2, delta1.3))
    delta2 <- rbind(delta2.1, rbind(delta2.2, delta2.3))
    delta3 <- rbind(delta3.1, rbind(delta3.2, delta3.3))
    
    delta <- rbind(delta1, rbind(delta2, delta3))
    
    ### Length of period
    delta$length <- as.numeric(delta$Year - delta$Start_year)
    
    ### annualize the difference
    delta$diff_g_yr <- (delta$Value - delta$prev_biom)
    
    ### calculate annual averages
    outDF <- summaryBy(diff_g_yr~Start_year+Year+Ring+Depth, FUN=mean,
                       data=delta, na.rm=T, keep.names=T)
    
    #- format dataframe to return
    out <- outDF[,c("Start_year", "Year", "Year", "Ring", "diff_g_yr", "Depth")]
    
    names(out) <- c("Start_date", "End_date", "Date", "Ring", "delta", "Depth")
    
    
    out$Trt <- "aCO2"
    out$Trt[out$Ring%in%c(1,4,5)] <- "eCO2"
    test <- summaryBy(delta~Trt+Depth, FUN=c(mean,sd), data=out, na.rm=T, keep.names=T)
    
    return(out)
    
}