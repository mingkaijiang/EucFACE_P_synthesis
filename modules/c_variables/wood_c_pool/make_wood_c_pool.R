
# Function to calculate biomass (kg) corresponding to diameter (cm)
# Using relationship taken from Paul et al. (2013) Forest Ecol. Manag. Volume 310.
allom_agb <- function(diamcm) {
    exp(-2.15 + 2.34*log(diamcm))
}

# Make the live wood C pool
make_wood_c_pool <- function(ring_area, c_frac, 
                             return_tree_level){
    
    ### download the data from HIEv
    download_diameter_data()
    
    ### read in 2012-15 data sets
    f13 <- read.csv(file.path(getToPath(), "FACE_P0025_RA_TREEMEAS_2012-13_RAW-V1.csv"))
    f14 <- read.csv(file.path(getToPath(), "FACE_P0025_RA_TREEMEAS_2013-14_RAW_V1.csv"))
    f15 <- read.csv(file.path(getToPath(), "FACE_P0025_RA_TREEMEAS_2015_RAW_V1.csv"))
    ### this file is not on HIEv yet!
    f12 <- read.csv("~/Documents/Research/Projects/EucFACE_C_Balance/R_repo/data/EucFACE_dendrometers2011-12_RAW.csv")
    
    ########################
    ### Read in additional files
    classif <- read.csv("download/FACE_AUX_RA_TREE-DESCRIPTIONS_R_20130201.csv",stringsAsFactors = FALSE)
    classif$Active.FALSE.means.dead.[classif$Tree == 608] <- FALSE  # This tree dead too
    
    ########################
    
    ### Merge the files
    all <- merge(classif,f12,by=c("Tree","Ring","CO2.trt"))
    all <- merge(all,f13,by=c("Tree","Ring","CO2.trt")) 
    all <- merge(all,f14,by=c("Tree","Ring","CO2.trt"))  
    all <- merge(all,f15,by=c("Tree","Ring","CO2.trt"))
    
    ### remove dead trees
    all[is.na(all)] <- "TRUE"
    all <- subset(all, Active.FALSE.means.dead.== TRUE)
    
    ### remove "CORR" columns and dead column
    uncorr <- all[,-grep("CORR",names(all))]
    uncorr <- uncorr[,-grep("Coor",names(uncorr))]
    
    uncorr <- uncorr[,names(uncorr) != "Active.FALSE.means.dead."]
    
    ### make a long-form version of dataframe
    long <- reshape(uncorr,idvar="Tree",varying=list(7:51),direction="long")
    dates <- names(uncorr)[7:51]
    long$Date <- c(rep(Sys.Date(),length(long$time)))  #wasn't sure how else to make this column date type
    for (i in (1:length(long$time))) {
        long$Date[i] <- as.Date(dates[long$time[i]],format="X%d.%m.%Y")
    }
    long <- renameCol(long,c("X17.02.2011"),c("diam"))
    
    long$diam <- as.numeric(long$diam)
    
    ### add biomass to long-form dataframe
    long$biom <- allom_agb(long$diam)  # in kg DM
    
    ### The bark removal affects the diameters mid-year. 
    ### Hence, just calculate biomass once per year 
    ### Specify dates here - may update this to March in future
    dates <- c(as.Date("2012-12-20"),as.Date("2013-12-20"),
               as.Date("2014-12-23"),as.Date("2015-12-14"))
    data <- long[long$Date %in% dates,]
    
    if(return_tree_level)return(data)
    
    ### sum across rings and dates
    data.m <- summaryBy(biom~Date+Ring,data=data,FUN=sum,keep.names=T,na.rm=T)
    
    ### sapwood to heartwood ratio:
    ### This is based on Table 3 of 
    ### Morais and Pereira, 2012. Wood Sci. Technol. Vol 46.
    ### in the future, we need to consider rlt between 
    ### tree diameter and heartwood height, and 
    ### tree diameter and heartwood diameter. 
    ### for now, this is an easy approach
    sap_to_heart <- 1.6
    
    ### calculate sapwood and heartwood biomass
    data.m$sap_biom <- data.m$biom * sap_to_heart / (sap_to_heart + 1.0)
    data.m$hea_biom <- data.m$biom - data.m$sap_biom
    
    ## divide by ring area to get biomass per m2
    data.m$wood_pool <- data.m$biom / ring_area
    data.m$sap_pool <- data.m$sap_biom /ring_area
    data.m$heart_pool <- data.m$hea_biom /ring_area
    
    ### convert from kg DM m-2 to g C m-2
    data.m$wood_pool <- data.m$wood_pool * c_frac * 1000
    data.m$sap_pool <- data.m$sap_pool * c_frac * 1000
    data.m$heart_pool <- data.m$heart_pool * c_frac * 1000
    
    ### format dataframe to return
    wood_pool <- data.m[,c("Date", "Ring", "wood_pool", "sap_pool", "heart_pool")]
    
    return(wood_pool)
}