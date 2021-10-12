#- Make the fineroot c production flux
make_fineroot_c_production_flux <- function(root.size,
                                            f.root,
                                            tot.root){
    
    
    # read in the csv
    frb1 <- read.csv("temp_files/EucFACERootsRingDateDepth.csv")
    frb1$Date <- as.Date(frb1$Dateform, format="%d-%m-%Y")
    
    # fineroot C pool
    frb1$frb_top <- with(frb1, FRP0d_0.10cm * C0_0.10cm / 100, na.rm=T)
    frb1$frb_bot <- with(frb1, FRP0d_10.30cm * C0_10.30cm / 100, na.rm=T)
    frb1$frb_tot <- with(frb1, frb_top + frb_bot, na.rm=T)
    
    # average across rings and dates
    frb.m <- summaryBy(frb_tot+frb_top+frb_bot~Date+Ring,data=frb1,FUN=mean,keep.names=T)
    
    ## colnames
    colnames(frb.m) <- c("Date","Ring","fineroot_production_flux", 
                         "fineroot_production_flux_0_10_cm", 
                         "fineroot_production_flux_10_30_cm")
    
    s.date <- unique(frb.m$Date)
    s.date <- s.date[-7]
    
    ### prepare output
    outDF <- subset(frb.m, Date > "2014-02-18")
    
    outDF$Start_date <- rep(s.date, each=6)
    outDF$End_date <- outDF$Date
    outDF$Days <- as.numeric(outDF$End_date - outDF$Start_date) + 1
    
    # format dataframe to return
    outDF <- outDF[,c("Date","Start_date", "End_date", "Ring","fineroot_production_flux", "Days")]
    
    # convert to mg C m-2 yr-1
    outDF$fineroot_production_flux <- outDF$fineroot_production_flux * 1000
    
    
    if (root.size == "small") {
        out <- outDF
    } else if (root.size == "intermediate") {
        
        
        ### obtain global coarseroot turnover rate from 
        ### Zhang and Wang, 2015. The decomposition of fine and coarse roots, global patterns and their controlling factors
        ### Scientific Reports, 5: 09940.
        gDF <- read.csv("temp_files/Zhang_Wang_2015_Global_Coarseroot.csv")
        
        subDF <- subset(gDF, Size%in%c("coarse root", "coarse root 2-5 mm"))
        subDF <- subset(subDF, Life.form == "Evergreen broadleaf")
        
        tau.croot <- mean(subDF$K.value_yr.1_)
        
        ### calculate the difference between total root and fineroot pool 
        ### to estimate the quantity of intermediate c pool
        f.root <- f.root[,c("Ring", "Date", "fineroot_pool")]
        tot.root <- tot.root[,c("Ring", "Date", "fineroot_pool")]
        names(tot.root)[names(tot.root)=="fineroot_pool"] <- "tot_pool"
        
        inDF <- merge(tot.root, f.root, by=c("Date", "Ring"))
        inDF$intermediate_root_pool <- with(inDF, tot_pool-fineroot_pool)
        
        ### assign tau to coarseroot DF
        inDF$tau <- tau.croot
        
        inDF$intermediate_root_production_flux <- with(inDF, intermediate_root_pool * tau) / 365 * 1000
        
        inDF$Start_date <- inDF$Date - 365 + 1
        
        inDF$Days <- 365
        
        ### prepare output
        tmpDF <- inDF[,c("Date", "Start_date", "Date", "Ring", "intermediate_root_production_flux", "Days")]
        colnames(tmpDF) <- c("Date", "Start_date", "End_date", "Ring", "intermediate_root_production_flux", "Days")
        
        ### calculate yearly production flux
        outDF$Yr <- year(outDF$Date)

        for (i in 1:6) {
            for (j in unique(outDF$Yr)) {
                outDF$fineroot_production_ann[outDF$Ring==i&outDF$Yr==j] <- with(outDF[outDF$Ring ==i&outDF$Yr==j,],
                                                                                 sum(fineroot_production_flux*Days)/sum(Days)) 
            }
        }
        
        sumDF1 <- summaryBy(fineroot_production_ann~Yr+Ring, data=outDF, na.rm=T,
                            keep.names=T, FUN=mean)
        
        ### the newDF
        tmpDF$Yr <- year(tmpDF$Date)
        for (i in 1:6) {
            for (j in unique(tmpDF$Yr)) {
                tmpDF$intermediate_root_production_ann[tmpDF$Ring==i&outDF$Yr==j] <- with(tmpDF[tmpDF$Ring ==i&tmpDF$Yr==j,],
                                                                                 sum(intermediate_root_production_flux*Days)/sum(Days)) 
            }
        }
        
        sumDF2 <- summaryBy(intermediate_root_production_ann~Yr+Ring, data=tmpDF, na.rm=T,
                            keep.names=T, FUN=mean)
        
        
        ### merge the fineroot production flux and the intermediate root production flux
        mgDF <- merge(sumDF1, sumDF2, by=c("Yr", "Ring"),
                      all=F)
        
        mgDF$fineroot_production_flux <- with(mgDF, fineroot_production_ann+intermediate_root_production_ann)
        
        ### add information
        mgDF$Date <- paste0(mgDF$Yr, "-01-01")
        mgDF$Start_date <- mgDF$Date
        mgDF$End_date <- paste0(mgDF$Yr, "-12-31")
        mgDF$Days <- 365
        
        out <- mgDF[,c("Date", "Start_date", "End_date", "Ring", "fineroot_production_flux", "Days")]
        
    } else {
        print("no root size available")
    }
    
    
    ### return
    return(out)
    
}