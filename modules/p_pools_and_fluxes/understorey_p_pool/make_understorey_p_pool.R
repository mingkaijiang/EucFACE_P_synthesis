#- Make the understorey P pool
make_understorey_p_pool <- function(){
    ### download the data 
    # currently there is a data format issue, so read in data from local directory
    # download_understorey_p_data()
    
    ### read in the file
    df <- read.csv("download/FACE_P0019_RA_leafP-understory_20130509-20151030_L1.csv")
    
    ### Assign date format
    df$Date <- as.Date(df$Date, "%d-%b-%y")
    
    ### ring and date specific data
    outDF <- summaryBy(PercP~Ring+Date,
                             data=df,FUN=mean,keep.names=T,na.rm=T)
    # currently the output is percent P, not P mass pool!!!
    
    return(outDF)
    
}
