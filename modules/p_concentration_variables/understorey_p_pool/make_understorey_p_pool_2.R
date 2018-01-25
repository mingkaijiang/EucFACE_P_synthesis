#- Make the understorey P pool from Sally's data
make_understorey_p_pool_2 <- function(){
    # return ring-specific canopy P data (mg/kg)

    # download the data
    df <- read.csv("download/POW151FR.csv")

    # average across rings
    df.m <- summaryBy(P~Ring,
                      data=df,FUN=mean,keep.names=T,na.rm=T)
    
    df.m <- df.m[1:6,]
    
    # get sla and lai data from C balance project
    lai_variable <- download_lai_variable()
    
    lai_variable <- subset(lai_variable, select=c(Date, Ring, LAI))
    names(lai_variable)[3] <- "lai_variable"
    
    #- return a number for ring
    lai_variable$Ring <- as.numeric(lai_variable$Ring)
    
    
    lma_raw <- download_lma_data()
    lma_raw$Date <- as.Date(lma_raw$Date, format="%d/%m/%Y")
    lma <- droplevels(subset(lma_raw, TREE != "outs R6"))  # outside ring trees
    lma <- mutate(lma, 
                  Ring = as.numeric(substr(TREE,1,1)),
                  LMA = as.numeric(LMA),
                  SLA = 10000 / LMA)  # cm2 g-1
    lma <- subset(lma, !is.na(Ring), select =c(Date,Ring, SLA))  # missing ring is for tree 'outs R6' - ignore
    
    lma_a <- summaryBy(SLA ~ Ring + Date, FUN=mean, na.rm=TRUE, data=lma, keep.names=TRUE)
    
    lma <- dplyr::rename(lma, sla_variable = SLA)
    
    SLA <- mean(lma$sla_variable, na.rm=TRUE)
    
    dfr <- lai_variable[,c("Date","Ring")]
    dfr$leaf_pool <- lai_variable$lai_variable / (10^-4 * SLA)
    
    dfr$ring <- rep(c(1:6), dim(dfr)[1]/6)
    
    for (i in 1:6) {
        dfr[dfr$ring == i, "leaf_p_pool"] <- dfr[dfr$ring == i, "leaf_pool"]/1000 * df.m[df.m$Ring == i, "P"]/1000
    }
    
    # leaf p pool in unit of g/m2
    df.out <- dfr[, c("Date", "ring", "leaf_p_pool")]
    
    
    return(df.out)
    
}

#download_lai_variable <- function(){
#    
#    downloadTOA5("FACE_P0037_RA_GAPFRACLAI_OPEN_L2.dat", quiet=TRUE)
#    
#}

#download_lma_data <- function(){
#    downloadCSV("FACE_P0020_RA_LMA_20150129-20150416_L2.csv")
#}
