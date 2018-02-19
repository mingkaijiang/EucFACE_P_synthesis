#- Make the canopy N concentration
make_canopy_n_concentration <- function() {
    ### return ring-specific canopy n data (mg/kg)

    ### download the data
    download_canopy_n_data()
    
    indf1 <- read.csv("download/FACE_P0020_RA_LeafN_L2_20130213-20131115.csv")
    indf2 <- read.csv("download/FACE_P0020_RA_LeafN_L2_20140130-20140502.csv")
    
    df <- rbind(indf1, indf2)
    df <- as.data.frame(df)
    
    ### setting up the date
    df$Date <- as.Date(df$Date, "%d/%m/%Y")
    df <- df[complete.cases(df),]
    
    ### Assign ring information based on tree number
    treeDF <- read.csv("download/FACE_AUX_RA_TREE-DESCRIPTIONS_R_20130201.csv",stringsAsFactors = FALSE)
    tree.list <- unique(df$TREE)
    tree.list <- tree.list[!is.na(tree.list)]
    
    for (i in tree.list) {
        df[df$TREE == i, "Ring"] <- treeDF[treeDF$Tree == i, "Ring"]
    }
    
    ### leaf n, average across rings and date, unit = %
    out <- summaryBy(PercN~Ring+Date,
                      data=df,FUN=mean,keep.names=T,na.rm=T)

    return(out)

}


