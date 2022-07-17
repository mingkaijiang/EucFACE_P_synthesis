make_yearly_delta_pool_without_depth_function <- function(inDF,var.col) {
    
    ### extract start and end date
    s.date <- min(inDF$Date)
    e.date <- max(inDF$Date)
    
    ### Change column name of value variable
    colnames(inDF)[var.col] <- "Value"
    
    ### Add year
    inDF$Year <- year(inDF$Date)
    
    myDF <- summaryBy(Value~Year+Ring+Depth, FUN=mean, data=inDF, keep.names=T)
    
    ### date list
    d.list <- unique(myDF$Year)
    d.list <- d.list[order(d.list)]
    
    
    ### create depth profiles
    subDF1 <- subset(myDF, Depth=="0_10")
    subDF2 <- subset(myDF, Depth=="10_30")
    subDF3 <- subset(myDF, Depth=="transition")
    
    d.list1 <- unique(subDF1$Year)
    d.list2 <- unique(subDF2$Year)
    d.list3 <- unique(subDF3$Year)
    
    
    
    ### create delta df
    delta1 <- subset(subDF1, Year != d.list1[1])
    delta1$Start_year <- delta1$Year  
    
    delta2 <- subset(subDF2, Year != d.list2[1])
    delta2$Start_year <- delta2$Year  
    
    delta3 <- subset(subDF3, Year != d.list3[1])
    delta3$Start_year <- delta3$Year  
    
    
    #### calculate differences
    for (i in 1:length(delta1$Year)) {
        delta1$Start_year[i] <- d.list[which(d.list1 == delta1$Year[i]) - 1]
        delta1$prev_biom[i] <- subDF1$Value[subDF1$Ring == delta1$Ring[i] &
                                             as.numeric(subDF1$Year-delta1$Start_year[i])==0]
    }
    
    
    for (i in 1:length(delta2$Year)) {
        delta2$Start_year[i] <- d.list[which(d.list2 == delta2$Year[i]) - 1]
        delta2$prev_biom[i] <- subDF2$Value[subDF2$Ring == delta2$Ring[i] &
                                                as.numeric(subDF2$Year-delta2$Start_year[i])==0]
    }
    
    #for (i in 1:length(delta3$Year)) {
    #    delta3$Start_year[i] <- d.list[which(d.list3 == delta3$Year[i]) - 1]
    #    delta3$prev_biom[i] <- subDF3$Value[subDF3$Ring == delta3$Ring[i] &
    #                                            as.numeric(subDF3$Year-delta3$Start_year[i])==0]
    #}
    
    
    # add depth
    delta1$Depth <- "0_10"
    delta2$Depth <- "10_30"
    
    #delta <- rbind(delta1, rbind(delta2, delta3))
    delta <- rbind(delta1, delta2)
    
    
    ### Length of period
    delta$length <- as.numeric(delta$Year - delta$Start_year)
    
    ### annualize the difference
    delta$diff_g_yr <- (delta$Value - delta$prev_biom)
    
    #- format dataframe to return
    out <- delta[,c("Start_year", "Year", "Year", "Ring", "diff_g_yr", "Depth")]
    
    names(out) <- c("Start_date", "End_date", "Date", "Ring", "delta", "Depth")
    
    return(out)
    
}