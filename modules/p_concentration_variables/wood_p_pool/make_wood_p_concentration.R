make_wood_p_concentration <- function() {
    
    #### New file
    infile <- "FACE_P0020_RA_NPwood_2015-L2.csv"
    
    if(!file.exists(paste0("download/", infile))) {
        downloadHIEv(hiev=searchHIEv(infile))
    }
    
    df <- read.csv("download/FACE_P0020_RA_NPwood_2015-L2.csv")
    
    df$Date <- paste0("1-", as.character(df$Campaign))
    df$Date <- gsub("_", "-", df$Date)
    df$Date <- as.Date(df$Date, "%d-%b-%Y")
    
    
    ### out
    df.out <- df[,c("Date", "Ring", "Perc.P.mean", "Perc.P.std.error")]
    colnames(df.out) <- c("Date", "Ring", "PercP", "PercP.sd")
    
    
    
    return(df.out)
    

}