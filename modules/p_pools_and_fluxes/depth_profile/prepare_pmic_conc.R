prepare_pmic_conc <- function() {
    #Pmic,ug g-1,"Fumigation extraction with Bray P AQ2 determination of PO4.
    
    # download the data
    download_microbial_p_data()
    
    df1 <- read.csv(file.path(getToPath(), 
                              "FACE_P0014_RA_MicrobialBiomassCNP_L1_20120613-20151130.csv"))
    df <- read.csv("temp_files/FACE_P0014_RA_MicrobialBiomassCNP_L1_20120613-20151130_V2.csv")
    df$Pmic <- as.numeric(as.character(df$Pmic_2_noEF))
    df <- df[complete.cases(df$ring),]
    
    df$Date <- as.character(df$date)
    df$date <- as.Date(df$Date, format="%d/%m/%Y")    
    
    # first, averaged across depths, unit: ug g-1
    df.m <- summaryBy(Pmic+Cmic~ring+date,
                      data=df,FUN=mean,keep.names=T,na.rm=T)
    
    df.m$depth <- "0_10"
    
    df.m <- df.m[,c("ring", "date", "depth", "Pmic", "Cmic")]
    
    
    ### this is august/sep 2017 data
    tmpDF <- read.csv("temp_files/MASTER_Mic_P_biomass_P0091.csv")
    tmpDF$date <- "2017-09-01"
    
    sumDF1 <- summaryBy(mic_P_mg.kg~ring+date+depth, FUN=mean,
                       keep.names=T, na.rm=T, data=tmpDF)
    
    colnames(sumDF1) <- c("ring", "date", "depth", "Pmic")
    sumDF1$Cmic <- NA
    
    sumDF1$depth <- gsub("0-10", "0_10", sumDF1$depth)
    sumDF1$depth <- gsub("10-30", "10_30", sumDF1$depth)
    
    ### Oct - 2018 data
    tmpDF <- read.csv("temp_files/belowground_P_working_sheet.csv")
    tmpDF$date <- "2018-10-01"
    sumDF2 <- summaryBy(Pmic+Cmic~ring+date+depth, FUN=mean,
                        keep.names=T, na.rm=T, data=tmpDF)
    
    
    
    myDF <- rbind(df.m, rbind(sumDF1, sumDF2))
    
    ### complete.cases
    outDF <- myDF[complete.cases(myDF$Pmic),]
    
    return(outDF)
    
}