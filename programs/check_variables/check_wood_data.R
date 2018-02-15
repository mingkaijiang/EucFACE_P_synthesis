check_wood_data <- function() {
    ### This script is to check whether wood pool increase or decrease over time
    
    
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
    all$Active.FALSE.means.dead.[is.na(all$Active.FALSE.means.dead.)] <- "TRUE"
    all <- subset(all, Active.FALSE.means.dead.== TRUE)
    all <- all[complete.cases(all),]
    
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
    
    ### Calculate per ring sum
    data.tot <- summaryBy(biom~Date+Ring,data=long,FUN=sum,keep.names=T,na.rm=T)
    
    ### calculate biomass once per year approach
    dates <- c(as.Date("2012-12-20"),as.Date("2013-12-20"),
               as.Date("2014-12-23"),as.Date("2015-12-14"))
    data <- long[long$Date %in% dates,]
    data.tot2 <- summaryBy(biom~Date+Ring,data=data,FUN=sum,keep.names=T,na.rm=T)
    
    
    ### check per tree diameter change information
    ck1 <- data.frame(unique(long$Tree), NA, NA)
    colnames(ck1) <- c("Tree", "Coef", "Intercept")
    
    for (i in ck1$Tree) {
        test <- subset(long, Tree==i) 
        lm.rlt <- lm(diam~Date, test)
        ck1[ck1$Tree == i, "Coef"] <- lm.rlt$coefficients[2]
        ck1[ck1$Tree == i, "Intercept"] <- lm.rlt$coefficients[1]
    }
    
    sigma <- round(mean(ck1$Coef),4)
    
    pdf("plots_tables/check_wood_data.pdf")
    
    ## Plot distribution of change in diameter
    hist(ck1$Coef, xlab="Coefficient", ylab = "Count",
         main="Linear fit curve coefficient Distribution for individual trees")
    text(0.001, 35, labels=bquote(sigma == .(sigma)), size=2)
    
    p1 <- ggplot(long, aes(Date, diam, color=factor(Ring))) +
        geom_point(size = 0.5) + 
        xlab("Date") + ylab("Diameter (cm)") + 
        scale_color_manual(values=c("#FF7F50", "#00FFFF", "#6495ED",
                                    "#FF4040", "#8B0000", "#0000FF"))
    plot(p1)
    
    p2 <- ggplot(data.tot, aes(Date, biom, color=factor(Ring))) +
        geom_point(size = 0.5) + 
        xlab("Date") + ylab("Biomass (kg/ring)") + 
        scale_color_manual(values=c("#FF7F50", "#00FFFF", "#6495ED",
                                    "#FF4040", "#8B0000", "#0000FF"))
    plot(p2)
    
    p3 <- ggplot(data.tot2, aes(Date, biom, color=factor(Ring))) +
        geom_point(size = 2) + 
        xlab("Date") + ylab("Biomass (kg/ring)") + 
        scale_color_manual(values=c("#FF7F50", "#00FFFF", "#6495ED",
                                    "#FF4040", "#8B0000", "#0000FF"))
    plot(p3)
    
    p4 <- ggplot() +
        geom_line(data=data.tot, aes(x=Date, y=biom, color=factor(Ring)), size = 0.5) + 
        geom_point(data=data.tot2, aes(x=Date, y=biom, color=factor(Ring)), size = 2) + 
        xlab("Date") + ylab("Biomass (kg/ring)") + 
        scale_color_manual(values=c("#FF7F50", "#00FFFF", "#6495ED",
                                    "#FF4040", "#8B0000", "#0000FF"))
    plot(p4)
    
    dev.off()
    
}