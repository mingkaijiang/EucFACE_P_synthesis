make_yearly_delta_pool_with_depth_function_microbe2 <- function(inDF,var.col) {
    
    ### extract start and end date
    s.date <- min(inDF$Date)
    e.date <- max(inDF$Date)
    
    ### Change column name of value variable
    colnames(inDF)[var.col] <- "Value"
    
    ### Add year
    inDF$Year <- year(inDF$Date)
    
    myDF1 <- inDF
    
    
    ### subset two datasets
    #inDF1 <- inDF[inDF$Date%in%c(as.Date("2014-09-09"), as.Date("2015-09-09"), 
    #                             as.Date("2017-09-01")),]
    #inDF2 <- inDF[inDF$Date%in%c(as.Date("2014-11-17"), as.Date("2015-11-30")),]
    
    #myDF1 <- summaryBy(Value~Year+Ring+Depth, FUN=mean, data=inDF1, keep.names=T)
    #myDF2 <- summaryBy(Value~Year+Ring+Depth, FUN=mean, data=inDF2, keep.names=T)
    
    ### date list
    d.list1 <- unique(myDF1$Date)
    d.list1 <- d.list1[order(d.list1)]
    
    #d.list2 <- unique(myDF2$Year)
    #d.list2 <- d.list2[order(d.list2)]
    
    
    ### create depth profiles
    subDF1.1 <- subset(myDF1, Depth=="0_10")
    subDF1.2 <- subset(myDF1, Depth=="10_30")
    subDF1.3 <- subset(myDF1, Depth=="transition")
    
    #subDF2.1 <- subset(myDF2, Depth=="0_10")
    #subDF2.2 <- subset(myDF2, Depth=="10_30")
    #subDF2.3 <- subset(myDF2, Depth=="transition")
    
    d.list1.1 <- unique(subDF1.1$Date)
    d.list1.2 <- unique(subDF1.2$Date)
    d.list1.3 <- unique(subDF1.3$Date)
    
    d.list1.1 <- d.list1.1[order(d.list1.1)]
    d.list1.2 <- d.list1.2[order(d.list1.2)]
    d.list1.3 <- d.list1.3[order(d.list1.3)]
    
    #d.list2.1 <- unique(subDF2.1$Year)
    #d.list2.2 <- unique(subDF2.2$Year)
    #d.list2.3 <- unique(subDF2.3$Year)
    
    
    ### create delta df
    delta1.1 <- subset(subDF1.1, Date != d.list1.1[1])
    delta1.1$Start_Date <- delta1.1$Date  
    
    delta1.2 <- subset(subDF1.2, Date != d.list1.2[1])
    delta1.2$Start_Date <- delta1.2$Date  
    
    delta1.3 <- subset(subDF1.3, Date != d.list1.3[1])
    delta1.3$Start_Date <- delta1.3$Date  
    
    
    #delta2.1 <- subset(subDF2.1, Year != d.list2.1[1])
    #delta2.1$Start_year <- delta2.1$Year  
    #
    #delta2.2 <- subset(subDF2.2, Year != d.list2.2[1])
    #delta2.2$Start_year <- delta2.2$Year  
    #
    #delta2.3 <- subset(subDF2.3, Year != d.list2.3[1])
    #delta2.3$Start_year <- delta2.3$Year  
    
    
    #### calculate differences
    for (i in 1:length(delta1.1$Date)) {
        delta1.1$Start_Date[i] <- d.list1.1[which(d.list1.1 == delta1.1$Date[i]) - 1]
        delta1.1$prev_biom[i] <- subDF1.1$Value[subDF1.1$Ring == delta1.1$Ring[i] &
                                             as.numeric(subDF1.1$Date-delta1.1$Start_Date[i])==0]
    }
    
    
    for (i in 1:length(delta1.2$Date)) {
        delta1.2$Start_Date[i] <- d.list1.2[which(d.list1.2 == delta1.2$Date[i]) - 1]
        delta1.2$prev_biom[i] <- subDF1.2$Value[subDF1.2$Ring == delta1.2$Ring[i] &
                                                as.numeric(subDF1.2$Date-delta1.2$Start_Date[i])==0]
    }
    
    for (i in 1:length(delta1.3$Date)) {
        delta1.3$Start_Date[i] <- d.list1.3[which(d.list1.3 == delta1.3$Date[i]) - 1]
        delta1.3$prev_biom[i] <- subDF1.3$Value[subDF1.3$Ring == delta1.3$Ring[i] &
                                                as.numeric(subDF1.3$Date-delta1.3$Start_Date[i])==0]
    }
    
    
    
    #for (i in 1:length(delta2.1$Year)) {
    #    delta2.1$Start_year[i] <- d.list2.1[which(d.list2.1 == delta2.1$Year[i]) - 1]
    #    delta2.1$prev_biom[i] <- subDF2.1$Value[subDF2.1$Ring == delta2.1$Ring[i] &
    #                                                as.numeric(subDF2.1$Year-delta2.1$Start_year[i])==0]
    #}
    #
    #
    #for (i in 1:length(delta2.2$Year)) {
    #    delta2.2$Start_year[i] <- d.list2.2[which(d.list2.2 == delta2.2$Year[i]) - 1]
    #    delta2.2$prev_biom[i] <- subDF2.2$Value[subDF2.2$Ring == delta2.2$Ring[i] &
    #                                                as.numeric(subDF2.2$Year-delta2.2$Start_year[i])==0]
    #}
    #
    #for (i in 1:length(delta2.3$Year)) {
    #    delta2.3$Start_year[i] <- d.list2.3[which(d.list2.3 == delta2.3$Year[i]) - 1]
    #    delta2.3$prev_biom[i] <- subDF2.3$Value[subDF2.3$Ring == delta2.3$Ring[i] &
    #                                                as.numeric(subDF2.3$Year-delta2.3$Start_year[i])==0]
    #}
    
    # add depth
    delta1.1$Depth <- "0_10"
    delta1.2$Depth <- "10_30"
    delta1.3$Depth <- "transition"
    
    #delta2.1$Depth <- "0_10"
    #delta2.2$Depth <- "10_30"
    #delta2.3$Depth <- "transition"
    
    delta <- rbind(delta1.1, rbind(delta1.2, delta1.3))
    #delta2 <- rbind(delta2.1, rbind(delta2.2, delta2.3))
    
    #delta <- rbind(delta1, delta2)
    
    ### Length of period
    delta$length <- as.numeric(delta$Date - delta$Start_Date)
    
    ### annualize the difference
    delta$diff_g_period <- (delta$Value - delta$prev_biom)
    delta$diff_g_yr <- with(delta, diff_g_period / length * 365.0)
    
    
    
    ### calculate annual averages
    outDF <- summaryBy(diff_g_yr~Start_Date+Year+Ring+Depth, FUN=mean,
                       data=delta, na.rm=T, keep.names=T)
    
    #- format dataframe to return
    out <- outDF[,c("Start_Date", "Year", "Year", "Ring", "diff_g_yr", "Depth")]
    
    names(out) <- c("Start_date", "End_date", "Date", "Ring", "delta", "Depth")
    
    
    out$Trt <- "aCO2"
    out$Trt[out$Ring%in%c(1,4,5)] <- "eCO2"
    test <- summaryBy(delta~Trt+Depth, FUN=c(mean,sd), data=out, na.rm=T, keep.names=T)
    
    return(out)
    
}