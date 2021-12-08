microbial_concentration_global_comparison <- function(norm,
                                                      microbial_p_concentration) {
    
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
    
    xuDF$Soil_organic_carbon <- as.numeric(xuDF$Soil_organic_carbon)
    xuDF$Soil_microbial_biomass_carbon <- as.numeric(xuDF$Soil_microbial_biomass_carbon)
    xuDF$Soil_microbial_biomass_phosphorus <- as.numeric(xuDF$Soil_microbial_biomass_phosphorus)
    xuDF$Total_organic_phosphorus <- as.numeric(xuDF$Total_organic_phosphorus)
    
    ### prepare unit from mmol kg-1 to %
    xuDF$Soil_organic_carbon <- xuDF$Soil_organic_carbon * 12.0 / 10000
    xuDF$Soil_microbial_biomass_carbon <- xuDF$Soil_microbial_biomass_carbon * 12.0 / 10000
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
    
    pdf(paste0("plots_tables/output/", norm, "/microbial_P_global_comparison.pdf"), 
        width=8, height=5)
    plot(p1)
    dev.off()
    
    
    
    ############################## check depth profile in xu et al. 2013
    ### select key variables
    subDF <- xuDF[,c("Biome", "Vegetation", "Depth", "pH",
                     "Soil_microbial_biomass_carbon",
                     "Soil_microbial_biomass_phosphorus",
                     "Total_organic_phosphorus",
                     "Soil_organic_carbon")]
    
    ### subset
    #xuDF <- subset(xuDF, Biome%in%c("Temperate Broadleaf Forest",
    #                                "Tropical/Subtropical Forest"))
    
    subDF1 <- subset(subDF, Depth%in%c("0~10", #"0~20","0~23",
                                       "0~3", "0~2.8", "0~2.7",
                                       "0~2.3", "0~5", "4~12",
                                       #"6~14", "6~16", 
                                       "3~10",
                                       "6~12", "3~11", #"0~15",
                                       "2~6", #"1.5~16", 
                                       "2~13",
                                       "1.5~5", #"5~16", 
                                       "0~2.5",
                                       "2.5~5", "5~10", "2.5~7.5",
                                       "0~2", "2~10", #"0~30",
                                       #"5~15", 
                                       "0~7.5", #"0~18",
                                       #"0~25", "0~24", 
                                       "0~13",
                                       #"3~15", 
                                       "Surface (0~10)",
                                       "1~3", #"0~22", 
                                       "0~8",
                                       "0~12", "0~6", "5~7.5", "7.5~10",
                                       "3.6~5", "2~3.6", "3~4", "0~11",
                                       "0~12.5", #"3~25", 
                                       "4~10",
                                       #"0~16", "2~19", "4~22",
                                       "0.5~11", #"4~14", 
                                       "0~9", "0~4", "1",
                                       "3", "8"))
    
    subDF1$Depth <- "0~10"
    
    subDF2 <- subset(subDF, Depth%in%c("10~20", #"0~20", "0~23",
                                       #"6~16", "0~15", "1.5~16",
                                       "13~25",
                                       "10~14", "10~16", "12~19", "14~22",
                                       "13~25", "10~14", "10~16", "12~19",
                                       "14~22", "5~16", "7.5~25", #"0~30",
                                       #"5~15", "0~18", "0~25", "10~27",
                                       #"15~30", "0~24", "13~26", 
                                       "10~23",
                                       "7.5~15", #"3~15", "0~22", "15~15",
                                       #"6~17", "3~19", "2~16", 
                                       "10~15",
                                       "15~20", "7.5~20", "10~12.5",
                                       "12.5~15", #"10~30", 
                                       "11~23"))#, #"3~25",
                                       #"0~16", "2~19", "4~22"))
    
    subDF2 <- subset(subDF, Depth%in%c("10~20", #"0~20", "0~23",
                                       "6~16", #"0~15", "1.5~16",
                                       "13~25",
                                       "10~14", "10~16", "12~19", "14~22",
                                       "13~25", "10~14", "10~16", "12~19",
                                       "14~22", "5~16", "7.5~25", #"0~30",
                                       "5~15", #"0~18", "0~25", 
                                       "10~27",
                                       "15~30", #"0~24", 
                                       "13~26", 
                                       "10~23",
                                       "7.5~15", #"3~15", "0~22", "15~15",
                                       #"6~17", "3~19", "2~16", 
                                       "10~15",
                                       "15~20", "7.5~20", "10~12.5",
                                       "12.5~15", "10~30", 
                                       "11~23",
                                       "20~30", "13~25",
                                       "7.5~25", #"0~30", "0~25",
                                       "10~27", "15~30", 
                                       "20~40"))
    
    subDF2$Depth <- "10~30"
    
    #subDF3 <- subset(subDF, Depth%in%c("20~30", #"13~25",
    #                                   #"7.5~25", "0~30", "0~25",
    #                                   #"10~27", "15~30", 
    #                                   "20~40"))#,
    #                                   #"13~26", "15~25", "10~30",
    #                                   #"3~25"))
    
    #subDF3$Depth <- "20~30"
    
    subDF4 <- subset(subDF, Depth%in%c("30~40", "40~50", "50~60"))
    
    subDF4$Depth <- "30~60"
    
    #mgDF <- rbind(subDF1, rbind(subDF2, rbind(subDF3, subDF4)))
    mgDF <- rbind(subDF1, rbind(subDF2,  subDF4))
    
    sumDF <- summaryBy(Soil_microbial_biomass_carbon+Soil_microbial_biomass_phosphorus+Total_organic_phosphorus~Depth, 
                       FUN=c(mean, sd), data=mgDF, na.rm=T, keep.names=T)

    
    ### Eucface
    eucDF <- summaryBy(PercP~Depth, FUN=c(mean, sd),
                      data=microbial_p_concentration,
                      na.rm=T, keep.names=T)
    
    
    ### Klaus's data
    # read in tmpDF
    # read in the Oct 2018 Johanna data at deeper depths
    tmpDF <- read.csv("temp_files/belowground_P_working_sheet.csv")
    tmpDF$Date <- as.Date(tmpDF$Date, format="%d/%m/%y")
    
    ### there are two NANs in the Cmic dataset, fill the gap
    v <- mean(tmpDF$Cmic[tmpDF$Depth=="transition"], na.rm=T)
    tmpDF$Cmic[tmpDF$Depth=="transition"&tmpDF$Ring%in%c(6,2)] <- v
    
    
    
    
}