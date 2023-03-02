compare_soil_p_mineralization_fluxes <- function (flux1, 
                                                  flux2, 
                                                  flux3,
                                                  flux4,
                                                  name1,
                                                  name2,
                                                  name3,
                                                  name4,
                                                  bkDF,
                                                  ref1,
                                                  ref2,
                                                  ref3,
                                                  ref4) {
    
    
    ### prepare plotDF
    plotDF <- data.frame("Ring" = rep(c(1:6), each=3*4),
                         "Method" = rep(c(name1, name2, name3, name4), each=3),
                         "Depth" = rep(c("0_10", "10_30", "transition"), 24),
                         "Flux" = NA)
    
    ### convert daily flux in mg P m2 d-1 to g P m-2 yr-1
    conv <- 365 / 1000
    
    ###  P mineralization flux
    for (i in c(1:6)) {
        for (j in c("0_10", "10_30", "transition"))
        plotDF$Flux[plotDF$Ring==i&plotDF$Method==name1&plotDF$Depth==j] <- with(flux1[flux1$Ring ==i&flux1$Depth ==j,],
                                                                              sum(p_mineralization_mg_m2_d*Days)/sum(Days)) * conv
    }
    
    for (i in c(1:6)) {
        for (j in c("0_10", "10_30", "transition"))
            plotDF$Flux[plotDF$Ring==i&plotDF$Method==name2&plotDF$Depth==j] <- with(flux2[flux2$Ring ==i&flux2$Depth ==j,],
                                                                                     sum(p_mineralization_mg_m2_d*Days)/sum(Days)) * conv
    }
    
    
    for (i in c(1:6)) {
        for (j in c("0_10", "10_30", "transition"))
            plotDF$Flux[plotDF$Ring==i&plotDF$Method==name3&plotDF$Depth==j] <- with(flux3[flux3$Ring ==i&flux3$Depth ==j,],
                                                                                     sum(p_mineralization_mg_m2_d*Days)/sum(Days)) * conv
    }
    
    
    for (i in c(1:6)) {
        for (j in c("0_10", "10_30", "transition"))
            plotDF$Flux[plotDF$Ring==i&plotDF$Method==name4&plotDF$Depth==j] <- with(flux4[flux4$Ring ==i&flux4$Depth ==j,],
                                                                                     sum(p_mineralization_mg_m2_d*Days)/sum(Days)) * conv
    }
    
    
    plotDF$Trt[plotDF$Ring%in%c(1,4,5)] <- "eCO2"
    plotDF$Trt[plotDF$Ring%in%c(2,3,6)] <- "aCO2"
    
    
    plotDF2 <- summaryBy(Flux~Trt+Method+Depth, FUN=c(mean,sd), data=plotDF,
                         na.rm=T, keep.names=T)
    
    write.csv(plotDF2, "plots_tables/output/unnormalized/soil_P_min_flux_comparison.csv",
              row.names=F)
    
    ### prepare bulk density plot
    bkDF$Trt[bkDF$Ring%in%c(1,4,5)] <- "eCO2"
    bkDF$Trt[bkDF$Ring%in%c(2,3,6)] <- "aCO2"
    
    plotDF3 <- summaryBy(bulk_density_kg_m3~Trt+Depth, FUN=c(mean, sd), data=bkDF,
                         na.rm=T, keep.names=T)
    
    # read in tmpDF
    # read in the Oct 2018 Johanna data at deeper depths
    tmpDF <- read.csv("temp_files/belowground_P_working_sheet.csv")
    tmpDF$Date <- as.Date(tmpDF$Date, format="%d/%m/%y")
    
    ### there are two NANs in the Cmic dataset, fill the gap
    v <- mean(tmpDF$Cmic[tmpDF$Depth=="transition"], na.rm=T)
    tmpDF$Cmic[tmpDF$Depth=="transition"&tmpDF$Ring%in%c(6,2)] <- v
    
    ### add fineroot c pool
    frDF <- summaryBy(fineroot_0_10_cm+fineroot_10_30_cm+fineroot_30_60_cm~Ring,
                      FUN=mean, data=fineroot_c_pool, keep.names=T, na.rm=T)
    frDF1 <- frDF[,c("Ring", "fineroot_0_10_cm")]
    colnames(frDF1) <- c("Ring", "FRC")
    frDF1$Depth <- "0_10"
    frDF2 <- frDF[,c("Ring", "fineroot_10_30_cm")]
    colnames(frDF2) <- c("Ring", "FRC")
    frDF2$Depth <- "10_30"
    frDF3 <- frDF[,c("Ring", "fineroot_30_60_cm")]
    colnames(frDF3) <- c("Ring", "FRC")
    frDF3$Depth <- "transition"
    frDF <- rbind(frDF1, rbind(frDF2, frDF3))
    
    tmpDF <- merge(tmpDF, frDF, by=c("Ring", "Depth"))
    
    
    ### prepare pool dataset
    ## fineroot C
    #tmpDF1 <- ref4[,c("Ring", "Date", "fineroot_0_10_cm")]
    #tmpDF2 <- ref4[,c("Ring", "Date", "fineroot_10_30_cm")]
    #tmpDF3 <- ref4[,c("Ring", "Date", "fineroot_30_60_cm")]
    #
    #tmpDF1$Depth <- "0_10"
    #tmpDF2$Depth <- "10_30"
    #tmpDF3$Depth <- "transition"
    #
    #colnames(tmpDF1) <- colnames(tmpDF2) <- colnames(tmpDF3) <- c("Ring", "Date", "Pool", "Depth")
    #
    #ref4 <- rbind(tmpDF1, rbind(tmpDF2, tmpDF3))
    #ref4 <- ref4[,c("Date", "Ring", "Depth", "Pool")]
    #
    #ref4$Trt[ref4$Ring%in%c(1,4,5)] <- "eCO2"
    #ref4$Trt[ref4$Ring%in%c(2,3,6)] <- "aCO2"
    #
    #ref4$Method <- "FinerootC"
    #
    ### soil C
    #colnames(ref1) <- c("Date", "Ring", "Depth", "Pool")
    #
    #ref1$Trt[ref1$Ring%in%c(1,4,5)] <- "eCO2"
    #ref1$Trt[ref1$Ring%in%c(2,3,6)] <- "aCO2"
    #
    #ref1$Method <- "SoilC"
    #
    ### Cmic
    #colnames(ref2) <- c("Date", "Ring", "Depth", "Pool")
    #
    #ref2$Trt[ref2$Ring%in%c(1,4,5)] <- "eCO2"
    #ref2$Trt[ref2$Ring%in%c(2,3,6)] <- "aCO2"
    #
    #ref2$Method <- "Cmic"
    #
    ### Pmic
    #colnames(ref3) <- c("Date", "Ring", "Depth", "Pool")
    #
    #ref3$Trt[ref3$Ring%in%c(1,4,5)] <- "eCO2"
    #ref3$Trt[ref3$Ring%in%c(2,3,6)] <- "aCO2"
    #
    #ref3$Method <- "Pmic"
    
    ### normalize all pool
    
    
    ### myDF
    #myDF <- rbind(ref1, rbind(ref2, rbind(ref3, ref4)))
    #
    #poolDF <- summaryBy(Pool~Trt+Depth+Method, FUN=c(mean, sd), data=myDF,
    #                     na.rm=T, keep.names=T)
    
    tmpDF$Date <- NULL
    plotDF4 <- reshape2::melt(tmpDF, id.var=c("Ring", "Depth", "Trt"))
    
    poolDF <- summaryBy(value~Depth+Trt+variable, FUN=c(mean, sd), data=plotDF4,
                        na.rm=T, keep.names=T)
    
    
    ### making plot
    p1 <- ggplot(plotDF2[plotDF2$Method=="SoilC",], 
                 aes(Trt, Flux.mean, group=Depth)) + 
        geom_bar(stat = "identity", aes(fill=Depth), position="dodge") +
        geom_errorbar(aes(ymax=Flux.mean+Flux.sd, ymin=Flux.mean-Flux.sd), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        xlab("Soil C Pool") + ylab(expression("Net " * P[min] * " (g " * m^-2 * " " * yr^-1 * ")"))+
        theme_linedraw() +
        #facet_wrap( ~ Method, nrow = 1)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_x_discrete(limits=c("aCO2", "eCO2"),
                         labels=c("aCO2"=expression(aCO[2]), 
                                  "eCO2"=expression(eCO[2])))+
        scale_fill_manual(name="Depth", 
                          values=c("0_10"=set3Palette[1],
                                   "10_30"=set3Palette[3],
                                   "transition"=set3Palette[5]),
                          labels=c("0_10"="0-10cm", 
                                   "10-30"="10-30cm",
                                   "transition"="30-60cm"))
    
    
    p2 <- ggplot(plotDF2[plotDF2$Method=="Cmic",], 
                 aes(Trt, Flux.mean, group=Depth)) + 
        geom_bar(stat = "identity", aes(fill=Depth), position="dodge") +
        geom_errorbar(aes(ymax=Flux.mean+Flux.sd, ymin=Flux.mean-Flux.sd), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        xlab("Microbial C Pool") + ylab(expression("Net " * P[min] * " (g " * m^-2 * " " * yr^-1 * ")"))+
        theme_linedraw() +
        #facet_wrap( ~ Method, nrow = 1)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_x_discrete(limits=c("aCO2", "eCO2"),
                         labels=c("aCO2"=expression(aCO[2]), 
                                  "eCO2"=expression(eCO[2])))+
        scale_fill_manual(name="Depth", 
                          values=c("0_10"=set3Palette[1],
                                   "10_30"=set3Palette[3],
                                   "transition"=set3Palette[5]),
                          labels=c("0_10"="0-10cm", 
                                   "10-30"="10-30cm",
                                   "transition"="30-60cm"))
    
    
    p3 <- ggplot(plotDF2[plotDF2$Method=="Pmic",], 
                 aes(Trt, Flux.mean, group=Depth)) + 
        geom_bar(stat = "identity", aes(fill=Depth), position="dodge") +
        geom_errorbar(aes(ymax=Flux.mean+Flux.sd, ymin=Flux.mean-Flux.sd), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        xlab("Microbial P Pool") + ylab(expression("Net " * P[min] * " (g " * m^-2 * " " * yr^-1 * ")"))+
        theme_linedraw() +
        #facet_wrap( ~ Method, nrow = 1)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_x_discrete(limits=c("aCO2", "eCO2"),
                         labels=c("aCO2"=expression(aCO[2]), 
                                  "eCO2"=expression(eCO[2])))+
        scale_fill_manual(name="Depth", 
                          values=c("0_10"=set3Palette[1],
                                   "10_30"=set3Palette[3],
                                   "transition"=set3Palette[5]),
                          labels=c("0_10"="0-10cm", 
                                   "10-30"="10-30cm",
                                   "transition"="30-60cm"))
    
    p4 <- ggplot(plotDF2[plotDF2$Method=="FinerootC",], 
                 aes(Trt, Flux.mean, group=Depth)) + 
        geom_bar(stat = "identity", aes(fill=Depth), position="dodge") +
        geom_errorbar(aes(ymax=Flux.mean+Flux.sd, ymin=Flux.mean-Flux.sd), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        xlab("Fineroot C Pool") + ylab(expression("Net " * P[min] * " (g " * m^-2 * " " * yr^-1 * ")"))+
        theme_linedraw() +
        #facet_wrap( ~ Method, nrow = 1)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_x_discrete(limits=c("aCO2", "eCO2"),
                         labels=c("aCO2"=expression(aCO[2]), 
                                  "eCO2"=expression(eCO[2])))+
        scale_fill_manual(name="Depth", 
                          values=c("0_10"=set3Palette[1],
                                   "10_30"=set3Palette[3],
                                   "transition"=set3Palette[5]),
                          labels=c("0_10"="0-10cm", 
                                   "10-30"="10-30cm",
                                   "transition"="30-60cm"))
    
    
    ### bulk density
    p5 <- ggplot(plotDF3, 
                 aes(Trt, bulk_density_kg_m3.mean, group=Depth)) + 
        geom_bar(stat = "identity", aes(fill=Depth), position="dodge") +
        geom_errorbar(aes(x=Trt, ymax=bulk_density_kg_m3.mean+bulk_density_kg_m3.sd, 
                          ymin=bulk_density_kg_m3.mean-bulk_density_kg_m3.sd), 
                      position = position_dodge(0.9), 
                      width=0.2, size=0.4) +
        xlab("") + ylab(expression("Bulk density (kg " * m^-3 * ")"))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="Depth", 
                          values=c("0_10"=set3Palette[1],
                                   "10_30"=set3Palette[3],
                                   "transition"=set3Palette[5]),
                          labels=c("0_10"="0-10cm", 
                                   "10-30"="10-30cm",
                                   "transition"="30-60cm"))
    
    
    p6 <- ggplot(poolDF[poolDF$variable=="SoilC",], 
                 aes(Trt, value.mean, group=Depth)) + 
        geom_bar(stat = "identity", aes(fill=Depth), position="dodge") +
        geom_errorbar(aes(ymax=value.mean+value.sd, ymin=value.mean-value.sd), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        xlab("") + 
        #ylab(expression("Soil C pool (g C " * m^-2 * ")"))+
        ylab(expression("Soil C (%)"))+
        theme_linedraw() +
        #facet_wrap( ~ Method, nrow = 1)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_x_discrete(limits=c("Ambient", "Elevated"),
                         labels=c("Ambient"=expression(aCO[2]), 
                                  "Elevated"=expression(eCO[2])))+
        scale_fill_manual(name="Depth", 
                          values=c("0_10"=set3Palette[1],
                                   "10_30"=set3Palette[3],
                                   "transition"=set3Palette[5]),
                          labels=c("0_10"="0-10cm", 
                                   "10-30"="10-30cm",
                                   "transition"="30-60cm"))
    
    
    p7 <- ggplot(poolDF[poolDF$variable=="Cmic",], 
                 aes(Trt, value.mean, group=Depth)) + 
        geom_bar(stat = "identity", aes(fill=Depth), position="dodge") +
        geom_errorbar(aes(ymax=value.mean+value.sd, ymin=value.mean-value.sd), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        xlab("") + #ylab(expression("Microbial C pool (g C " * m^-2 * ")"))+
        ylab(expression("Microbial C (mg C " * kg^-1 * ")"))+
        theme_linedraw() +
        #facet_wrap( ~ Method, nrow = 1)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_x_discrete(limits=c("Ambient", "Elevated"),
                         labels=c("Ambient"=expression(aCO[2]), 
                                  "Elevated"=expression(eCO[2])))+
        scale_fill_manual(name="Depth", 
                          values=c("0_10"=set3Palette[1],
                                   "10_30"=set3Palette[3],
                                   "transition"=set3Palette[5]),
                          labels=c("0_10"="0-10cm", 
                                   "10-30"="10-30cm",
                                   "transition"="30-60cm"))
    
    
    p8 <- ggplot(poolDF[poolDF$variable=="Pmic",], 
                 aes(Trt, value.mean, group=Depth)) + 
        geom_bar(stat = "identity", aes(fill=Depth), position="dodge") +
        geom_errorbar(aes(ymax=value.mean+value.sd, ymin=value.mean-value.sd), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        xlab("") + #ylab(expression("Soil P pool (g P " * m^-2 * ")"))+
        ylab(expression("Microbial P (mg P " * kg^-1 * ")"))+
        theme_linedraw() +
        #facet_wrap( ~ Method, nrow = 1)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_x_discrete(limits=c("Ambient", "Elevated"),
                         labels=c("Ambient"=expression(aCO[2]), 
                                  "Elevated"=expression(eCO[2])))+
        scale_fill_manual(name="Depth", 
                          values=c("0_10"=set3Palette[1],
                                   "10_30"=set3Palette[3],
                                   "transition"=set3Palette[5]),
                          labels=c("0_10"="0-10cm", 
                                   "10-30"="10-30cm",
                                   "transition"="30-60cm"))
    
    
    p9 <- ggplot(poolDF[poolDF$variable=="FRC",], 
                 aes(Trt, value.mean, group=Depth)) + 
        geom_bar(stat = "identity", aes(fill=Depth), position="dodge") +
        geom_errorbar(aes(ymax=value.mean+value.sd, ymin=value.mean-value.sd), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        xlab("") + ylab(expression("Fineroot C pool (g C " * m^-2 * ")"))+
        theme_linedraw() +
        #facet_wrap( ~ Method, nrow = 1)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_x_discrete(limits=c("Ambient", "Elevated"),
                         labels=c("Ambient"=expression(aCO[2]), 
                                  "Elevated"=expression(eCO[2])))+
        scale_fill_manual(name="Depth", 
                          values=c("0_10"=set3Palette[1],
                                   "10_30"=set3Palette[3],
                                   "transition"=set3Palette[5]),
                          labels=c("0_10"="0-10cm", 
                                   "10-30"="10-30cm",
                                   "transition"="30-60cm"))
    
    
    p10 <- ggplot(poolDF[poolDF$variable=="DOC",], 
                 aes(Trt, value.mean, group=Depth)) + 
        geom_bar(stat = "identity", aes(fill=Depth), position="dodge") +
        geom_errorbar(aes(ymax=value.mean+value.sd, ymin=value.mean-value.sd), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        xlab("") + ylab(expression("DOC (mg C " * kg^-1 * ")"))+
        theme_linedraw() +
        #facet_wrap( ~ Method, nrow = 1)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_x_discrete(limits=c("Ambient", "Elevated"),
                         labels=c("Ambient"=expression(aCO[2]), 
                                  "Elevated"=expression(eCO[2])))
    
    
    
    p11 <- ggplot(poolDF[poolDF$variable=="BrayP",], 
                  aes(Trt, value.mean, group=Depth)) + 
        geom_bar(stat = "identity", aes(fill=Depth), position="dodge") +
        geom_errorbar(aes(ymax=value.mean+value.sd, ymin=value.mean-value.sd), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        xlab("") + ylab(expression("Bray P (mg P " * kg^-1 * ")"))+
        theme_linedraw() +
        #facet_wrap( ~ Method, nrow = 1)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_x_discrete(limits=c("Ambient", "Elevated"),
                         labels=c("Ambient"=expression(aCO[2]), 
                                  "Elevated"=expression(eCO[2])))
    
    
    
    p12 <- ggplot(poolDF[poolDF$variable=="InorgP",], 
                  aes(Trt, value.mean, group=Depth)) + 
        geom_bar(stat = "identity", aes(fill=Depth), position="dodge") +
        geom_errorbar(aes(ymax=value.mean+value.sd, ymin=value.mean-value.sd), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        xlab("") + ylab(expression("Inorganic P (mg P " * kg^-1 * ")"))+
        theme_linedraw() +
        #facet_wrap( ~ Method, nrow = 1)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_x_discrete(limits=c("Ambient", "Elevated"),
                         labels=c("Ambient"=expression(aCO[2]), 
                                  "Elevated"=expression(eCO[2])))
    
    
    p13 <- ggplot(poolDF[poolDF$variable=="OrgP",], 
                  aes(Trt, value.mean, group=Depth)) + 
        geom_bar(stat = "identity", aes(fill=Depth), position="dodge") +
        geom_errorbar(aes(ymax=value.mean+value.sd, ymin=value.mean-value.sd), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        xlab("") + ylab(expression("Organic P (mg P " * kg^-1 * ")"))+
        theme_linedraw() +
        #facet_wrap( ~ Method, nrow = 1)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_x_discrete(limits=c("Ambient", "Elevated"),
                         labels=c("Ambient"=expression(aCO[2]), 
                                  "Elevated"=expression(eCO[2])))
    
    
    
    p14 <- ggplot(poolDF[poolDF$variable=="TotalP",], 
                  aes(Trt, value.mean, group=Depth)) + 
        geom_bar(stat = "identity", aes(fill=Depth), position="dodge") +
        geom_errorbar(aes(ymax=value.mean+value.sd, ymin=value.mean-value.sd), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        xlab("") + ylab(expression("Total P (mg P " * kg^-1 * ")"))+
        theme_linedraw() +
        #facet_wrap( ~ Method, nrow = 1)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_x_discrete(limits=c("Ambient", "Elevated"),
                         labels=c("Ambient"=expression(aCO[2]), 
                                  "Elevated"=expression(eCO[2])))
    
    grid.labs <- c("(a)", "(b)", "(c)", "(d)", 
                   "(e)",
                   "(f)", "(g)", "(h)", "(i)")
    
    legend_bot_row <- get_legend(p1 + theme(legend.position="bottom",
                                            legend.box = 'horizontal',
                                            legend.box.just = 'left'))
    
    ## plot 
    pdf("plots_tables/output/unnormalized/soil_P_mineralization_flux_method_comparison.pdf", width=10, height=10)
    bot_row <- plot_grid(p1, p2, p3, p4, ncol=4)
    mid_row <- plot_grid(p5, NA, NA, NA, ncol=4)
    top_row <- plot_grid(p6, p7, p8, p9, ncol=4)
    #end_row <- plot_grid(p10, p11, p12, p13, ncol=4)
    
    plot_grid(top_row, 
              mid_row,
              bot_row, 
              #end_row,
              legend_bot_row,
              ncol = 1, rel_widths = c(1, 1, 1, 1),
              rel_heights=c(1, 1, 1, 0.2))
    grid.text(grid.labs,
              x = c(0.21, 0.47, 0.72, 0.96,
                    0.21,
                    0.21, 0.47, 0.72, 0.96), 
              y = c(0.98, 0.98, 0.98, 0.98,
                    0.67, 
                    0.35, 0.35, 0.35, 0.35),
              gp=gpar(fontsize=16, col="black", fontface="bold"))
    dev.off()
    
    
}
