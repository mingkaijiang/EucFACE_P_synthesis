#- Make the understorey P concentration
make_understorey_p_concentration <- function(){
    ### download the data 
    # currently there is a data format issue, so read in data from local directory
    download_understorey_p_data()
    
    ### read in the file
    df <- read.csv("download/FACE_P0019_RA_leafP-understory_20130509-20151030_L1.csv")
    df$Date <- as.Date(df$Date, "%d-%b-%y")
    df1 <- df[,c("Date", "Ring", "PercP")]
    
    ### read in the most recent harvest data
    df2 <- read.csv("temp_files/understorey_P_concentration_data_2017_06.csv")
    df2 <- subset(df2, Component == "Understorey")
    df2 <- subset(df2, Subclass == "Live")
    df2$PercP <- df2$P_conc_mg_g / 10
    df3 <- df2[,c("Date", "Ring", "PercP")]
    df3$Date <- as.Date(df3$Date, format="%d/%m/%y")
    ### prepare the data
    myDF <- rbind(df1, df3)
    
    
    ### ring and date specific data
    outDF <- summaryBy(PercP~Ring+Date,
                             data=myDF,FUN=mean,keep.names=T,na.rm=T)

    return(outDF)
    
}
