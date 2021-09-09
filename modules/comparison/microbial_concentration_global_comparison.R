microbial_concentration_global_comparison <- function(microbial_p_concentration) {
    
    ### calculate averages and standard deviations
    microbial_p_concentration$Trt[microbial_p_concentration$Ring%in%c(1,4,5)] <- "ele"
    microbial_p_concentration$Trt[microbial_p_concentration$Ring%in%c(2,3,6)] <- "amb"
    
    myDF <- summaryBy(PercP~Trt, FUN=c(mean, sd),
                      data=microbial_p_concentration,
                      na.rm=T, keep.names=T)
    
    ### read in Xu et al. dataset
    xuDF <- read.csv("temp_files/Xu_etal_2013/Soil_Microbial_Biomass_C_N_P_spatial.csv",
                     header=T)
    xuDF <- xuDF[-1,]
    
    xuDF$Soil_microbial_biomass_phosphorus <- as.numeric(xuDF$Soil_microbial_biomass_phosphorus)
    xuDF$Total_organic_phosphorus <- as.numeric(xuDF$Total_organic_phosphorus)
    
    ### prepare unit from mmol kg-1 to %
    xuDF$Soil_microbial_biomass_phosphorus <- xuDF$Soil_microbial_biomass_phosphorus * 31.0 / 10000
    xuDF$Total_organic_phosphorus <- xuDF$Total_organic_phosphorus * 31.0 / 10000
    
    ### remove not mentioned
    sumDF <- summaryBy(Soil_microbial_biomass_phosphorus+Total_organic_phosphorus~Biome, 
                      FUN=c(mean, sd), data=xuDF, na.rm=T, keep.names=T)
    
    sumDF <- sumDF[complete.cases(sumDF$Soil_microbial_biomass_phosphorus.mean),]
    colnames(sumDF) <- c("Biome", "Pmic.mean", "Porg.mean",
                         "Pmic.sd", "Porg.sd")
    
    
    ### prepare eucface
    colnames(myDF) <- c("Biome", "Pmic.mean", "Pmic.sd")
    myDF$Porg.mean <- NA
    myDF$Porg.sd <- NA
    myDF <- myDF[,c("Biome", "Pmic.mean", "Porg.mean",
                    "Pmic.sd", "Porg.sd")]
    
    plotDF <- rbind(myDF, sumDF)
    
    p1 <- ggplot(data=plotDF, aes(Biome, Pmic.mean)) +
        geom_point(aes(fill=Biome), pch=21, size=3)+
        geom_errorbar(aes(ymin=Pmic.mean-Pmic.sd, ymax=Pmic.mean+Pmic.sd),
                      width=0.5)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=14),
              axis.text.y=element_text(size=14),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=14),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_x_discrete(limits=c("amb",
                                  "ele",
                                  "Bare soil",
                                  "Boreal Forest",
                                  "Cropland",
                                  "Desert",
                                  "Grassland",
                                  "Natural Wetland",
                                  "Pasture",
                                  "Savanna",
                                  "Shrub",
                                  "Temperate Broadleaf Forest",
                                  "Temperate Coniferous Forest",
                                  "Tropical/Subtropical Forest",
                                  "Tundra"),
                         label=c(expression(EucFACE[amb]),
                                 expression(EucFACE[ele]),
                                 "Bare Soil",
                                 "Boreal Forest",
                                 "Cropland",
                                 "Desert",
                                 "Grassland",
                                 "Natural Wetland",
                                 "Pasture",
                                 "Savanna",
                                 "Shrub",
                                 "Temperate Broadleaf Forest",
                                 "Temperate Coniferous Forest",
                                 "Tropical/Subtropical Forest",
                                 "Tundra"))+
        coord_flip()+
        xlab("")+
        ylab("Microbial P concentration (%)")
    
    pdf("plots_tables/microbial_P_global_comparison.pdf", width=8, height=5)
    plot(p1)
    dev.off()
    
    
    
    
}