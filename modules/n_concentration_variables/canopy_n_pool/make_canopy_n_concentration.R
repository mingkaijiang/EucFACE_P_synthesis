#- Make the canopy N concentration
make_canopy_n_concentration <- function() {
    ### return ring-specific canopy n data (mg/kg)

    ### download the data
    download_canopy_n_data()
    
    indf1 <- read.csv("download/FACE_P0020_RA_LeafN_L2_20130213-20131115.csv")
    indf2 <- read.csv("download/FACE_P0020_RA_LeafN_L2_20140130-20140502.csv")
    indf3 <- read.csv("download/FACE_P0020_RA_LeafN_L2_20150201-20180201.csv.csv")
    
    indf3 <- indf3[,c("Campaign", "Campaign", "Tree", "Age_class", "Leaf_Cohort", "LeafN_concentration")]
    colnames(indf3) <- c("Campaign", "Date", "TREE", "AGE", "LEAF", "PercN")
    indf3$Date <- paste0("01/", indf3$Date)
    indf3$Date <- gsub("_", "/", indf3$Date)
    indf3$Date <- as.Date(indf3$Date, format="%d/%b/%Y")
    indf3$Campaign <- as.character(indf3$Campaign)
    indf3$AGE <- as.character(indf3$AGE)
    indf3$LEAF <- as.character(indf3$LEAF)
    indf3$PercN <- indf3$PercN/10
    
    df <- rbind(indf1, indf2)
    df <- as.data.frame(df)
    
    df$Campaign <- as.character(df$Campaign)
    df$AGE <- as.character(df$AGE)
    df$LEAF <- as.character(df$LEAF)
    df$Date <- as.Date(df$Date, "%d/%m/%Y")
    
    df <- rbind(df, indf3)
    
    ### setting up the date
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


