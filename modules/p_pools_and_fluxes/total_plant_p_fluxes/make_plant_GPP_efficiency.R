make_plant_GPP_efficiency <- function(norm, p_pool, p_flux, 
                                      can_gpp, und_gpp) {
    
    
    ### calculate total production P flux for canopy
    can.p.flux <- p_flux[p_flux$terms%in%c("Canopy P flux"),]
    tot1 <- as.numeric(colSums(can.p.flux[,2:7]))
    
    ### calculate total production P flux for understorey 
    und.p.flux <- p_flux[p_flux$terms%in%c("Understorey P flux"),]
    tot2 <- as.numeric(colSums(und.p.flux[,2:7]))
    
    
    ### get leaf P pool for overstorey and understorey
    can.p.pool <- p_pool[p_pool$terms%in%c("Canopy P Pool"),]
    tot3 <- as.numeric(colSums(can.p.pool[,2:7]))
    
    ## get leaf P pool for overstorey and understorey
    und.p.pool <- p_pool[p_pool$terms%in%c("Understorey P Pool"),]
    tot4 <- as.numeric(colSums(und.p.pool[,2:7]))
    
    
    ### prepare GPP 
    can.gpp <- summaryBy(GPP~Ring, FUN=mean, data=can_gpp,
                         na.rm=T, keep.names=T)
    und.gpp <- summaryBy(GPP~Ring, FUN=mean, data=und_gpp,
                         na.rm=T, keep.names=T)
    
    names(can.gpp)[names(can.gpp)=="GPP"] <- "overGPP"
    names(und.gpp)[names(und.gpp)=="GPP"] <- "undGPP"
    
    ### merge all dataframs
    myDF <- merge(can.gpp, und.gpp, by=c("Ring"))
    
    ### add
    myDF$canPflux <- tot1
    myDF$undPflux <- tot2
    myDF$canPpool <- tot3
    myDF$undPpool <- tot4
    
    ### calculate efficiencies
    myDF$over_P_effi_flux <- with(myDF, overGPP/canPflux)
    myDF$und_P_effi_flux <- with(myDF, undGPP/undPflux)
    
    myDF$over_P_effi_pool <- with(myDF, overGPP/canPpool)
    myDF$und_P_effi_pool <- with(myDF, undGPP/undPpool)
    
    
    myDF$Trt <- "aCO2"
    myDF$Trt[myDF$Ring%in%c("1","4","5")] <- "eCO2"
    
    #summaryBy(undGPP+undPflux+und_P_effi_flux+und_P_effi_pool~Trt, data=myDF, FUN=c(mean,sd))
    
    
    ### outDF
    tmpDF <- myDF[,c("Ring", "over_P_effi_flux", "und_P_effi_flux")]
    tmpDF$Trt <- "aCO2"
    tmpDF$Trt[tmpDF$Ring%in%c("1","4","5")] <- "eCO2"
    
    ### convert into long
    tmpDF2 <- reshape2::melt(tmpDF, id.var=c("Ring", "Trt"))
    tmpDF2$variable <- gsub("over_P_effi_flux", "overstorey", tmpDF2$variable)
    tmpDF2$variable <- gsub("und_P_effi_flux", "understorey", tmpDF2$variable)
    
    
    ### plot DF
    plotDF1 <- summaryBy(value~Trt+variable,
                        data=tmpDF2, FUN=mean, na.rm=T,
                        keep.names=T)
    
    plotDF2 <- summaryBy(value~Trt+variable,
                         data=tmpDF2, FUN=sd, na.rm=T,
                         keep.names=T)
    
    names(plotDF1)[names(plotDF1)=="value"] <- "meanvalue"
    names(plotDF2)[names(plotDF2)=="value"] <- "sdvalue"
    
    plotDF <- merge(plotDF1, plotDF2, by=c("Trt", "variable"))
    
    ### plotting script
    p1 <- ggplot(plotDF, aes(x=variable, y=meanvalue, group=Trt))+
        geom_bar(stat = "identity", aes(fill=Trt), 
                 position="dodge")+
        geom_errorbar(aes(ymax=meanvalue+sdvalue, 
                          ymin=meanvalue-sdvalue), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        labs(x="", 
             y=expression("GPP / " * P[leaf] * " flux (g C " * g^-1 * " P)"))+
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
        scale_fill_manual(name="",
                          values=c("aCO2"="blue2", "eCO2"="red3"),
                          labels=c("aCO2"=expression(aCO[2]),
                                  "eCO2"=expression(eCO[2])))
    
    ### pdf
    pdf(paste0("plots_tables/output/", norm, "/GPP_efficiency_", norm, ".pdf"),
        width=6,height=8)
    
    plot(p1)
    
    dev.off()
    
    
    ### prepare outDF
    out <- tmpDF2
    
    names(out)[names(out)=="value"] <- "GPP_efficiency_gC_gP"
    
    
    ### return
    return(out)
}