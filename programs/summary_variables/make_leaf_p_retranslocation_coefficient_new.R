#- Make the retranslocation coefficient
make_leaf_p_retranslocation_coefficient_new <- function(){
 
    df <- read.csv("temp_files/GreenLeaves_allP-clean_Mingkai.csv")
    
    ### setting up the date
    df$Date <- paste0("01-", as.character(df$Campaign))
    df$Date <- as.Date(df$Date, "%d-%b-%y")
    
    df.old <- subset(df, AGE == "old")
    
    ### Leaf green p, average across rings, ignore dates, unit = %
    df.old.p <- summaryBy(Perc.P~Ring,
                             data=df.old,FUN=mean,keep.names=T,na.rm=T)
    
    # read in litter
    df2 <- read.csv("temp_files/Litter_Data_Mingkai.csv")
    
    #### setting up the date
    df2$Date <- gsub("-Feb", "-02", df2$Campaign)
    df2$Date <- paste0(as.character(df2$Date), "-01")
    df2$Date <- as.Date(df2$Date, "%Y-%m-%d")
    
    df.sen <- subset(df2, Origin == "Senesced leaf")
    
    df.sen.p <- summaryBy(Perc.P~Ring,
                          data=df.sen,FUN=mean,keep.names=T,na.rm=T)
    
    ### calculate leaf P retranslocation rate based on dead and green leaf
    retransDF <- cbind(df.old.p, df.sen.p$Perc.P)
    colnames(retransDF) <- c("Ring", "old", "senesced")
    retransDF$retrans_coef <- (retransDF$old - retransDF$senesced) / retransDF$old 

    ### Plot eCO2 effect on retranslocation coefficient
    retransDF$CO2 <- c("eCO2", "aCO2", "aCO2", "eCO2", "eCO2", "aCO2")


    outDF <- retransDF[,c("Ring", "retrans_coef", "CO2")]

    return(outDF)
    
}


