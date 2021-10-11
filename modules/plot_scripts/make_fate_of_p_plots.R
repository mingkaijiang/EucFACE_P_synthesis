make_fate_of_p_plots <- function(inDF, norm) {
    
  
     ### calculate the litter P fluxes
     subDF1 <- subset(inDF, terms%in%c("Leaflitter P over P mineralization",
                                       "Fineroot litter P over P mineralization",
                                       "Twig litter P over P mineralization",
                                       "Bark litter P over P mineralization",
                                       "Seed litter P over P mineralization",
                                       "Frass litter P over P mineralization",
                                       "Understorey litter P over P mineralization",
                                       "Leaching P over P mineralization"))
     
     subDF2 <- subset(inDF, terms=="Soil P mineralization flux")
     subDF2$terms <- "Unaccounted P over P mineralization"
     subDF2[,2:7] <- 1.0 - colSums(subDF1[,2:7])
     
     ### calculate means and sd
     subDF2$aCO2 <- mean(c(subDF2$R2, subDF2$R3, subDF2$R6), na.rm=T)
     subDF2$eCO2 <- mean(c(subDF2$R1, subDF2$R4, subDF2$R5), na.rm=T)
     
     subDF2$aCO2_sd <- sd(c(subDF2$R2, subDF2$R3, subDF2$R6), na.rm=T)
     subDF2$eCO2_sd <- sd(c(subDF2$R1, subDF2$R4, subDF2$R5), na.rm=T)
     
     ### merge
     subDF1 <- rbind(subDF1, subDF2)
     
     ### calculate the real magnitude in unit of g P m-2 yr-1
     myDF2 <- subDF1
     
     ### calculation
     for (i in 1:nrow(myDF2)) {
       myDF2[i,2:7] <- subDF1[i,2:7]*inDF[inDF$terms=="Soil P mineralization flux",2:7]
       
     }
     
     myDF2$terms <- gsub(" over P mineralization" , "", myDF2$terms)
     
     myDF2$aCO2 <- rowMeans(data.frame(myDF2$R2, myDF2$R3, myDF2$R6), na.rm=T)
     myDF2$eCO2 <- rowMeans(data.frame(myDF2$R1, myDF2$R4, myDF2$R5), na.rm=T)
     
     myDF2$aCO2_sd <- rowSds(as.matrix(data.frame(myDF2$R2, myDF2$R3, myDF2$R6)), na.rm=T)
     myDF2$eCO2_sd <- rowSds(as.matrix(data.frame(myDF2$R1, myDF2$R4, myDF2$R5)), na.rm=T)

     
     
     ### prepare plotDF - %
     tmpDF1 <- subDF1[,c("terms", "aCO2", "eCO2")]
     tmpDF2 <- subDF1[,c("terms", "aCO2_sd", "eCO2_sd")]
     
     tmpDF3 <- reshape2::melt(tmpDF1, id.vars=c("terms"))
     tmpDF4 <- reshape2::melt(tmpDF2, id.vars=c("terms"))
     
     colnames(tmpDF3) <- c("Terms", "Trt", "meanvalue")
     colnames(tmpDF4) <- c("Terms", "Trt", "sdvalue")
     tmpDF4$Trt <- gsub("_sd", "", tmpDF4$Trt)
     
     plotDF1 <- merge(tmpDF3, tmpDF4, by=c("Terms", "Trt"))
     plotDF1$Terms <- gsub(" P over P mineralization", "", plotDF1$Terms)
     
     
    ### prepare plotDF real magnitude of CO2 effect  
    myDF2 <- rbind(myDF2, inDF[inDF$terms=="Soil P mineralization flux", 1:11]) 
    
    tmpDF1 <- myDF2[,c("terms", "aCO2", "eCO2", "aCO2_sd", "eCO2_sd")]
    tmpDF1$Effect_size <- with(tmpDF1, eCO2 - aCO2)
    
    for (i in 1:nrow(tmpDF1)) {
      tmpDF1$Effect_sd[i] <- sqrt(sum(c(tmpDF1$eCO2_sd[i]^2, tmpDF1$aCO2_sd[i]^2), na.rm=T)/2)
    }

    
    plotDF2 <- tmpDF1[,c("terms", "Effect_size", "Effect_sd")]
    
    ### assign column
    plotDF2$col <- as.character("2")
    plotDF2$col[plotDF2$terms=="Soil P mineralization flux"] <- as.character("1")
    
    ### calculate the sums mean and sds
    plotDF3 <- summaryBy(Effect_size~col, data=plotDF2, FUN=sum,
                         keep.names=T, na.rm=T)
    
    plotDF3$Effect_sd[plotDF3$col=="1"] <- plotDF2$Effect_sd[plotDF2$col=="1"]
    
    plotDF3$Effect_sd[plotDF3$col=="2"] <- sqrt(sum(c(plotDF2$Effect_sd[plotDF2$terms=="Leaflitter P"]^2,
                                                      plotDF2$Effect_sd[plotDF2$terms=="Fineroot litter P"]^2,
                                                      plotDF2$Effect_sd[plotDF2$terms=="Twig litter P"]^2,
                                                      plotDF2$Effect_sd[plotDF2$terms=="Bark litter P"]^2,
                                                      plotDF2$Effect_sd[plotDF2$terms=="Seed litter P"]^2,
                                                      plotDF2$Effect_sd[plotDF2$terms=="Frass litter P"]^2,
                                                      plotDF2$Effect_sd[plotDF2$terms=="Understorey litter P"]^2,
                                                      plotDF2$Effect_sd[plotDF2$terms=="Leaching P"]^2,
                                                      plotDF2$Effect_sd[plotDF2$terms=="Unaccounted P"]^2), na.rm=T)/(nrow(plotDF2)-1))
    
    
    ### Plotting
    cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    
    #set3Palette <- brewer.pal(n = 10, name = "Set3")
    
    #YlOrRdPalette <- rev(brewer.pal(n = 9, name = "YlOrRd"))
    
    #GreensPalette <- rev(brewer.pal(n = 9, name = "Greens"))
    
    #SpectralPalette <- brewer.pal(n = 8, name = "Spectral")
    
    ### % of fluxes as part of mineralization flux, aCO2 and eCO2
    p1 <- ggplot(plotDF1, aes(x=Trt, y=meanvalue))+
      geom_bar(stat = "identity", aes(fill=Terms), 
               position="stack")+
      labs(x="", 
           y="Allocation fraction")+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.title.x = element_text(size=10), 
            axis.text.x = element_text(size=10),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="none")+
      scale_x_discrete(labels=c("aCO2"=expression(aCO[2]),
                                "eCO2"=expression(eCO[2])))+
      scale_fill_manual(name="Fluxes", 
                        values = c("Leaflitter" = cbbPalette[2],
                                   "Twig litter" = cbbPalette[3],
                                   "Bark litter" = cbbPalette[3],
                                   "Seed litter" = cbbPalette[3],
                                   "Fineroot litter" = cbbPalette[4],
                                   "Frass litter" = cbbPalette[5],
                                   "Understorey litter" = cbbPalette[6],
                                   "Leaching" = cbbPalette[7],
                                   "Unaccounted" = cbbPalette[8]),
                        labels=c("Leaflitter" = expression(P[can]),
                                 "Twig litter" = expression(P[twig]),
                                 "Bark litter" = expression(P[bark]),
                                 "Seed litter" = expression(P[seed]),
                                 "Fineroot litter" = expression(P[froot]),
                                 "Frass litter" = expression(P[frass]),
                                 "Understorey litter" = expression(P[ua]),
                                 "Leaching" = expression(P[leach]),
                                 "Unaccounted" = expression(P[unacc])))
    
    
    
    ### CO2 effect
    p2 <- ggplot(plotDF2, aes(x=col, y=Effect_size))+
      geom_abline(slope=0, intercept=0, lty=1, col="black")+
      geom_bar(stat = "identity", aes(fill=terms), 
               position="stack")+
      geom_point(data=plotDF3, aes(x=col, y=Effect_size), 
                 pch=21, size=4, col="black", fill="red")+
      geom_errorbar(data=plotDF3,
                    aes(x=col, ymax=Effect_size+Effect_sd, 
                        ymin=Effect_size-Effect_sd), 
                    position = position_dodge(0.9), width=0.2, size=0.4) +
      labs(x="", 
           y=expression(CO[2] * " effect (g P " * m^-2 * " " * yr^-1 * ")"))+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.title.x = element_text(size=10), 
            axis.text.x = element_text(size=10),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="bottom")+
      scale_x_discrete(labels=c("1"=expression(P[min]),
                                "2"="P fluxes"))+
      scale_fill_manual(name="Fluxes", 
                        values = c("Soil P mineralization flux" = cbbPalette[1], 
                                   "Leaflitter P" = cbbPalette[2],
                                   "Twig litter P" = cbbPalette[3],
                                   "Bark litter P" = cbbPalette[3],
                                   "Seed litter P" = cbbPalette[3],
                                   "Fineroot litter P" = cbbPalette[4],
                                   "Frass litter P" = cbbPalette[5],
                                   "Understorey litter P" = cbbPalette[6],
                                   "Leaching P" = cbbPalette[7],
                                   "Unaccounted P" = cbbPalette[8]),
                        labels=c("Soil P mineralization flux" = expression(P[min]), 
                                 "Leaflitter P" = expression(P[can]),
                                 "Twig litter P" = expression(P[twig]),
                                 "Bark litter P" = expression(P[bark]),
                                 "Seed litter P" = expression(P[seed]),
                                 "Fineroot litter P" = expression(P[froot]),
                                 "Frass litter P" = expression(P[frass]),
                                 "Understorey litter P" = expression(P[ua]),
                                 "Leaching P" = expression(P[leach]),
                                 "Unaccounted P" = expression(P[unacc])))
    
    
    
    pdf(paste0("plots_tables/output/", norm, "/Fate_of_P_", norm, ".pdf"),
        width=6,height=8)
    plot_grid(p1, p2, labels="", ncol=1, align="v", axis = "l",
              rel_heights = c(1, 1.2))
    grid.text(c("(a)", "(b)"), x = c(0.15, 0.15),
              y = c(0.96, 0.5), 
              gp=gpar(fontsize=14, col="black", fontface="bold"))
    dev.off()
    
}


