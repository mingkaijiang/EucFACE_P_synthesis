plot_CO2_effect_on_the_same_figure <- function(budgetDF,
                                               concDF,
                                               poolDF,
                                               fluxDF,
                                               deltaDF,
                                               cpDF) {
    
    ### data cleaning
    budgetDF <- budgetDF[,c("terms", "aCO2", "eCO2", "aCO2_sd", "eCO2_sd", "diff", "diff_se", "diff_cf", "percent_diff")]
    concDF <- concDF[,c("conc.terms", "aCO2", "eCO2", "aCO2_sd", "eCO2_sd", "diff", "diff_se", "diff_cf", "percent_diff")]
    poolDF <- poolDF[,c("terms", "aCO2", "eCO2", "aCO2_sd", "eCO2_sd", "diff", "diff_se", "diff_cf", "percent_diff")]
    fluxDF <- fluxDF[,c("terms", "aCO2", "eCO2", "aCO2_sd", "eCO2_sd", "diff", "diff_se", "diff_cf", "percent_diff")]
    deltaDF <- deltaDF[,c("terms", "aCO2", "eCO2", "aCO2_sd", "eCO2_sd", "diff", "diff_se", "diff_cf", "percent_diff")]
    
    colnames(concDF) <- c("terms", "aCO2", "eCO2", "aCO2_sd", "eCO2_sd", "diff", "diff_se", "diff_cf", "percent_diff")
    
    ### add group column
    budgetDF$Group <- "1_budget"
    concDF$Group <- "2_concentration"
    poolDF$Group <- "3_pool"
    fluxDF$Group <- "4_flux"
    deltaDF$Group <- "5_delta"
    
    
    ### add GPP PUE into budgetDF
    tmpDF <- budgetDF[budgetDF$terms=="Overstorey GPP efficiency",]
    tmpDF$terms[tmpDF$terms=="Overstorey GPP efficiency"] <- "Plant GPP efficiency"
    tmpDF$aCO2 <- budgetDF$aCO2[budgetDF$terms=="Overstorey GPP efficiency"] + budgetDF$aCO2[budgetDF$terms=="Understorey GPP efficiency"] 
    tmpDF$eCO2 <- budgetDF$eCO2[budgetDF$terms=="Overstorey GPP efficiency"] + budgetDF$eCO2[budgetDF$terms=="Understorey GPP efficiency"] 
    tmpDF$aCO2_sd <- sqrt((budgetDF$aCO2_sd[budgetDF$terms=="Overstorey GPP efficiency"]^2
                           + budgetDF$aCO2_sd[budgetDF$terms=="Understorey GPP efficiency"]^2)/2)
    tmpDF$eCO2_sd <- sqrt((budgetDF$eCO2_sd[budgetDF$terms=="Overstorey GPP efficiency"]^2
                           + budgetDF$eCO2_sd[budgetDF$terms=="Understorey GPP efficiency"]^2)/2)
    tmpDF$diff <- tmpDF$eCO2 - tmpDF$aCO2
    tmpDF$diff_se <- sqrt((tmpDF$aCO2_sd^2+tmpDF$eCO2_sd^2)/2) * (sqrt(2/3))
    tmpDF$diff_cf <- qt(0.975, 4) * tmpDF$diff_se
    tmpDF$percent_diff <- (tmpDF$eCO2 - tmpDF$aCO2)/tmpDF$aCO2 * 100.0
    
    budgetDF <- rbind(budgetDF, tmpDF)
    
    ### add CP ratio DF
    cpDF2 <- reshape2::melt(cpDF, id.vars=c("Ring"))
    cpDF2$Trt <- "aCO2"
    cpDF2$Trt[cpDF2$Ring%in%c(1,4,5)] <- "eCO2"
    
    cpDF3 <- summaryBy(value~Trt+variable, data=cpDF2, FUN=c(mean,sd), na.rm=T, keep.names=T)
    
    cpDF4 <- cpDF3[cpDF3$Trt=="aCO2",]
    cpDF5 <- cpDF3[cpDF3$Trt=="eCO2",]
    
    cpDF4$Trt <- NULL
    cpDF5$Trt <- NULL
    
    colnames(cpDF4) <- c("terms", "aCO2", "aCO2_sd")
    colnames(cpDF5) <- c("terms", "eCO2", "eCO2_sd")
    
    cpDF6 <- merge(cpDF4, cpDF5, by="terms")
    
    cpDF6$diff <- with(cpDF6, eCO2 - aCO2)
    cpDF6$percent_diff <- with(cpDF6, (eCO2-aCO2)/aCO2 * 100)
    cpDF6$diff_se <- with(cpDF6, sqrt((aCO2_sd^2+eCO2_sd^2)/2)*sqrt(2/3))
    cpDF6$diff_cf <- with(cpDF6, qt(0.975, 4) * diff_se)
    
    cpDF6 <- cpDF6[cpDF6$terms%in%c("canopy", "fineroot", "frass", 
                                    "leaflitter", "microbe", "sapwood",
                                    "soil", "understorey", "understorey_litter"),]
    
    cpDF6$terms <- as.character(cpDF6$terms)
    
    cpDF6$terms[cpDF6$terms=="canopy"] <- "canopy CP"
    cpDF6$terms[cpDF6$terms=="fineroot"] <- "fineroot CP"
    cpDF6$terms[cpDF6$terms=="frass"] <- "frass CP"
    cpDF6$terms[cpDF6$terms=="leaflitter"] <- "leaflitter CP"
    cpDF6$terms[cpDF6$terms=="microbe"] <- "microbe CP"
    cpDF6$terms[cpDF6$terms=="sapwood"] <- "sapwood CP"
    cpDF6$terms[cpDF6$terms=="understorey"] <- "understorey CP"
    cpDF6$terms[cpDF6$terms=="understorey_litter"] <- "understorey litter CP"
    cpDF6$terms[cpDF6$terms=="soil"] <- "soil CP"
    
    cpDF <- cpDF6[,c("terms", "aCO2", "eCO2", "aCO2_sd", "eCO2_sd", "diff", "diff_se", "diff_cf", "percent_diff")]
    cpDF$Group <- "6_cp"

    ### merge all
    myDF <- rbind(budgetDF, rbind(concDF, rbind(poolDF, rbind(fluxDF, rbind(deltaDF, cpDF)))))
    
    
    ### calculate absolute difference and sd
    #myDF$diff <- myDF$eCO2 - myDF$aCO2
    #myDF$diff_se <- sqrt((myDF$aCO2_sd^2 + myDF$eCO2_sd^2)/2)
    
    
    ### calculate 80% confidence interval, etc. 
    myDF$diff_cf_90 <- with(myDF, diff_cf / qt(0.975, 4) * qt(0.95, 4))
    myDF$diff_cf_85 <- with(myDF, diff_cf / qt(0.975, 4) * qt(0.925, 4))
    myDF$diff_cf_80 <- with(myDF, diff_cf / qt(0.975, 4) * qt(0.90, 4))
    myDF$diff_cf_75 <- with(myDF, diff_cf / qt(0.975, 4) * qt(0.875, 4))
    myDF$diff_cf_68 <- with(myDF, diff_cf / qt(0.975, 4) * qt(0.84, 4))
    
    
    ### remove null
    myDF <- myDF[complete.cases(myDF$diff),]
    
 
    #### plotting preparation
    plotDF1 <- subset(myDF, Group=="1_budget")
    plotDF2 <- subset(myDF, Group=="2_concentration")
    plotDF3 <- subset(myDF, Group=="3_pool")
    plotDF4 <- subset(myDF, Group=="4_flux")
    plotDF5 <- subset(myDF, Group=="5_delta")
    plotDF6 <- subset(myDF, Group=="6_cp")
    
    
    plotDF21 <- subset(plotDF2, terms%in%c("Canopy P Conc", "Sapwood P Conc",
                                           #"Heartwood P Conc", 
                                           "Fine Root P Conc",
                                           "Coarse Root P Conc", "Leaflitter P Conc",
                                           "Leaflitter P Conc", "Understorey P Conc",
                                           "Frass P Conc"))
    
    plotDF22 <- subset(plotDF2, terms%in%c("Microbial P Conc 0-10cm", "Microbial P Conc 10-30cm",
                                           "Microbial P Conc 30-60cm", "Soil P Conc 0-10cm",
                                           "Soil P Conc 10-30cm", "Soil P Conc 30-60cm",
                                           "Soil Inorg P Conc 0-10cm", "Soil Inorg P Conc 10-30cm", 
                                           "Soil Inorg P Conc 30-60cm", "Soil Org P Conc 0-10cm", 
                                           "Soil Org P Conc 10-30cm", "Soil Org P Conc 30-60cm",
                                           "Soil Phosphate P Conc 0-10cm", "Soil Phosphate P Conc 10-30cm",
                                           "Soil Phosphate P Conc 30-60cm"))
    
    plotDF23 <- subset(plotDF2, terms%in%c("Exchangeable Pi Conc 0-10cm",
                                           "Exchangeable Po Conc 0-10cm",
                                           "Moderately labile Po Conc 0-10cm",
                                           "Secondary Fe bound Pi Conc 0-10cm",
                                           "Primary Ca bound Pi Conc 0-10cm",
                                           "Occluded P Conc 0-10cm"))
    
    
    plotDF21$collab <- ifelse(plotDF21$diff > 0.0, "pos", 
                              ifelse(plotDF21$diff < 0.0, "neg", "neut"))
    
    plotDF22$collab <- ifelse(plotDF22$diff > 0.0, "pos", 
                              ifelse(plotDF22$diff < 0.0, "neg", "neut"))
    
    
    ## special case for primary Ca Pi pool, 6 digits after decimal places, so round it up to positive
    plotDF23$collab <- ifelse(plotDF23$diff >= 0.0, "pos", 
                              ifelse(plotDF23$diff < 0.0, "neg", "neut"))
    
    
    
    
    plotDF31 <- subset(plotDF3, terms%in%c("Canopy P Pool", "Sapwood P Pool",
                                           "Heartwood P Pool", "Total Wood P Pool",
                                           "Fine Root P Pool",
                                           "Coarse Root P Pool", 
                                           "Forestfloor Leaf Litter P Pool",
                                           "Understorey P Pool",
                                           "Understorey Litter P Pool", "Standing Dead Wood P Pool"))
    
    plotDF32 <- subset(plotDF3, terms%in%c("Microbial P Pool 0-10cm", "Microbial P Pool 10-30cm",
                                           "Microbial P Pool 30-60cm", "Soil P Pool 0-10cm",
                                           "Soil P Pool 10-30cm", "Soil P Pool 30-60cm",
                                           "Soil Inorg P Pool 0-10cm", "Soil Inorg P Pool 10-30cm", 
                                           "Soil Inorg P Pool 30-60cm", "Soil Org P Pool 0-10cm", 
                                           "Soil Org P Pool 10-30cm", "Soil Org P Pool 30-60cm",
                                           "Soil Phosphate P Pool 0-10cm", "Soil Phosphate P Pool 10-30cm",
                                           "Soil Phosphate P Pool 30-60cm"))
    
    plotDF33 <- subset(plotDF3, terms%in%c("Exchangeable Pi Pool",
                                           "Exchangeable Po Pool",
                                           "Moderately labile Po Pool",
                                           "Secondary Fe bound Pi Pool",
                                           "Primary Ca bound Pi Pool",
                                           "Occluded P Pool"))
    
    
    plotDF31$collab <- ifelse(plotDF31$diff > 0.0, "pos", 
                              ifelse(plotDF31$diff < 0.0, "neg", "neut"))
    
    plotDF32$collab <- ifelse(plotDF32$diff > 0.0, "pos", 
                              ifelse(plotDF32$diff < 0.0, "neg", "neut"))
    
    plotDF33$collab <- ifelse(plotDF33$diff > 0.0, "pos", 
                              ifelse(plotDF33$diff < 0.0, "neg", "neut"))
    
    
    
    
    plotDF41 <- subset(plotDF4, terms%in%c("Canopy P flux", 
                                           "Wood P flux",
                                           "Fine Root P flux",
                                           "Coarse Root P flux", 
                                           "Understorey P flux"))
    
    plotDF42 <- subset(plotDF4, terms%in%c("Fineroot Litter P flux",
                                           "Leaflitter P flux",
                                           "Bark litter P flux",
                                           "Twig litter P flux",
                                           "Seed litter P flux",
                                           "Frass P flux",
                                           "Understorey Litter P flux"))
    
    plotDF43 <- subset(plotDF4, terms%in%c("Canopy retrans P flux", 
                                           "Sapwood retrans P flux",
                                           #"Fineroot retrans P flux",
                                           #"Coarseroot retrans P flux", 
                                           "Understorey retrans P flux"))
    
    plotDF44 <- subset(plotDF4, terms%in%c("Total vegetation production P flux", 
                                           "Total vegetation retranslocation P flux",
                                           "Total vegetation uptake P flux", 
                                           "Mineralization P flux 0-10cm",
                                           "Mineralization P flux 10-30cm", 
                                           "Mineralization P flux 30-60cm"))
    
    
    
    plotDF41$collab <- ifelse(plotDF41$diff > 0.0, "pos", 
                              ifelse(plotDF41$diff < 0.0, "neg", "neut"))
    
    plotDF42$collab <- ifelse(plotDF42$diff > 0.0, "pos", 
                              ifelse(plotDF42$diff < 0.0, "neg", "neut"))
    
    plotDF43$collab <- ifelse(plotDF43$diff > 0.0, "pos", 
                              ifelse(plotDF43$diff < 0.0, "neg", "neut"))
    
    plotDF44$collab <- ifelse(plotDF44$diff > 0.0, "pos", 
                              ifelse(plotDF44$diff < 0.0, "neg", "neut"))
    
    
    
    
    plotDF51 <- subset(plotDF5, terms%in%c("Canopy P Pool", "Sapwood P Pool",
                                           "Heartwood P Pool", "Total Wood P Pool",
                                           "Fine Root P Pool",
                                           "Coarse Root P Pool", 
                                           "Forestfloor Leaf Litter P Pool",
                                           "Understorey P Pool"))
    
    plotDF52 <- subset(plotDF5, terms%in%c("Microbial P Pool 0-10cm", 
                                           "Microbial P Pool 10-30cm", 
                                           "Microbial P Pool 30-60cm", 
                                           "Soil P Pool 0-10cm",
                                           "Soil P Pool 10-30cm",
                                           "Soil Phosphate P Pool 0-10cm",
                                           "Soil Phosphate P Pool 10-30cm"))
    
    plotDF51$collab <- ifelse(plotDF51$diff > 0.0, "pos", 
                              ifelse(plotDF51$diff < 0.0, "neg", "neut"))
    
    plotDF52$collab <- ifelse(plotDF52$diff > 0.0, "pos", 
                              ifelse(plotDF52$diff < 0.0, "neg", "neut"))
    
    plotDF6$collab <- ifelse(plotDF6$diff > 0.0, "pos", 
                              ifelse(plotDF6$diff < 0.0, "neg", "neut"))
    
    
    
    ### plotDF1 split into multiple sub DFs
    subDF1 <- plotDF1[plotDF1$terms%in%c("Total plant P stock",
                                         "Labile Pi stock",
                                         "Overstorey aboveground P stock",
                                         "Understorey aboveground P stock",
                                         "Belowground P stock",
                                         "Dead P stock"),]
    
    subDF2 <- plotDF1[plotDF1$terms%in%c("Total plant P requirement flux",
                                         "Total plant P retranslocation flux",
                                         "Plant P uptake flux",
                                         "Soil P mineralization flux"),]
    
    
    subDF3 <- plotDF1[plotDF1$terms%in%c("Plant P uptake over requirement",
                                         "Plant P uptake over P mineralization",
                                         "Leaflitter P over P mineralization",
                                         "Fineroot litter P over P mineralization",
                                         "Twig litter P over P mineralization",
                                         "Bark litter P over P mineralization",
                                         "Seed litter P over P mineralization",
                                         "Frass litter P over P mineralization",
                                         "Understorey litter P over P mineralization",
                                         "Leaching P over P mineralization"),]
    
    ### make plots
    #p1 <- ggplot(subDF1) +  
    #    geom_segment(aes(y=reorder(terms, diff), x=diff-diff_cf, 
    #                     yend=reorder(terms, diff), xend=diff+diff_cf), 
    #                 size=6, color="grey")+
    #    geom_point(aes(y=reorder(terms, diff), x=diff), 
    #               stat='identity', size=4, shape=19)+
    #    geom_vline(xintercept=0)+
    #    xlab(expression(paste(CO[2], " effect (g P ", m^-2, ")"))) + 
    #    ylab("") +
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.title.x = element_text(size=12, family="Helvetica"), 
    #          axis.text.x = element_text(size=12, family="Helvetica"),
    #          axis.text.y=element_text(size=12, family="Helvetica"),
    #          axis.title.y=element_text(size=12, family="Helvetica"),
    #          legend.text=element_text(size=12, family="Helvetica"),
    #          legend.title=element_text(size=12, family="Helvetica"),
    #          panel.grid.major=element_blank(),
    #          legend.position="bottom",
    #          legend.text.align=0)
    #
    #
    #p2 <- ggplot(subDF2) +  
    #    geom_segment(aes(y=reorder(terms, diff), x=diff-diff_cf, 
    #                     yend=reorder(terms, diff), xend=diff+diff_cf), 
    #                 size=6, color="grey")+
    #    geom_point(aes(y=reorder(terms, diff), x=diff), 
    #               stat='identity', size=4, shape=19)+
    #    geom_vline(xintercept=0)+
    #    xlab(expression(paste(CO[2], " effect (g P ", m^-2, " ", yr^-1,")"))) + 
    #    ylab("") +
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.title.x = element_text(size=12, family="Helvetica"), 
    #          axis.text.x = element_text(size=12, family="Helvetica"),
    #          axis.text.y=element_text(size=12, family="Helvetica"),
    #          axis.title.y=element_text(size=12, family="Helvetica"),
    #          legend.text=element_text(size=12, family="Helvetica"),
    #          legend.title=element_text(size=12, family="Helvetica"),
    #          panel.grid.major=element_blank(),
    #          legend.position="bottom",
    #          legend.text.align=0)
    #
    #
    #p3 <- ggplot(subDF3) +  
    #    geom_segment(aes(y=reorder(terms, diff), x=diff-diff_cf, 
    #                     yend=reorder(terms, diff), xend=diff+diff_cf), 
    #                 size=6, color="grey")+
    #    geom_point(aes(y=reorder(terms, diff), x=diff), 
    #               stat='identity', size=4, shape=19)+
    #    geom_vline(xintercept=0)+
    #    xlab(expression(paste(CO[2], " effect (unitless)"))) + 
    #    ylab("") +
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.title.x = element_text(size=12, family="Helvetica"), 
    #          axis.text.x = element_text(size=12, family="Helvetica"),
    #          axis.text.y=element_text(size=12, family="Helvetica"),
    #          axis.title.y=element_text(size=12, family="Helvetica"),
    #          legend.text=element_text(size=12, family="Helvetica"),
    #          legend.title=element_text(size=12, family="Helvetica"),
    #          panel.grid.major=element_blank(),
    #          legend.position="bottom",
    #          legend.text.align=0)
    
    
    
    
    
    p41 <- ggplot(plotDF21) +  
        geom_vline(xintercept=0)+
        geom_segment(aes(y=terms, x=diff-diff_cf, 
                         yend=terms, xend=diff+diff_cf, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_85, 
                         yend=terms, xend=diff+diff_cf_85, color=collab), alpha=0.2,
                     size=6)+
        #geom_segment(aes(y=terms, x=diff-diff_cf_80, 
        #                 yend=terms, xend=diff+diff_cf_80, color=collab), alpha=0.2,
        #             size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_75, 
                         yend=terms, xend=diff+diff_cf_75, color=collab), alpha=0.6,
                     size=6)+
        geom_errorbarh(aes(y=terms, xmin=diff-diff_se, 
                          xmax=diff+diff_se), col="black", height=0.1)+
        geom_point(aes(y=terms, fill=collab, x=diff), color="black",
                   stat='identity', size=4, shape=21)+
        xlab(expression(paste(CO[2], " effect (ele - amb, %)"))) + 
        ylab("") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.box.background = element_rect(alpha("grey",0.5)),
              legend.position=c(0.78, 0.6),
              legend.text.align=0)+
        scale_y_discrete(limits=c("Frass P Conc",
                                  "Understorey P Conc",
                                  "Leaflitter P Conc",
                                  "Coarse Root P Conc",
                                  "Fine Root P Conc",
                                  #"Heartwood P Conc",
                                  "Sapwood P Conc",
                                  "Canopy P Conc"),
                         labels=c("Canopy P Conc" = "Canopy P",
                                  "Sapwood P Conc" = "Sapwood P",
                                  #"Heartwood P Conc" = "Heartwood P",
                                  "Fine Root P Conc" = "Fine root P", 
                                  "Coarse Root P Conc" = "Coarse root P",
                                  "Leaflitter P Conc" = "Leaflitter P",
                                  "Understorey P Conc" = "Understorey P",
                                  "Frass P Conc" = "Frass P"))+
        scale_fill_manual(name="Legend",
                           labels=c("pos"="Positive",
                                    "neg"="Negative",
                                    "neut"="Zero"),
                           values=c("pos"="darkgreen", "neg"="brown", "neut"="black"))+
        scale_color_manual(name="Legend",
                           labels=c("pos"="Positive",
                                    "neg"="Negative",
                                    "neut"="Zero"),
                           values=c("pos"="darkgreen", 
                                    "neg"="brown", 
                                    "neut"="black"))
        #scale_color_manual(name="Legend",
        #                  labels=c("pos"="Positive",
        #                           "neg"="Negative",
        #                           "neut"="Zero"),
        #                  values=c("pos"=alpha("darkgreen",0.2), 
        #                           "neg"=alpha("brown", 0.2), 
        #                           "neut"=alpha("black", 0.2))); plot(p41)
    
    #plot(p41)
    
    
    p42 <- ggplot(plotDF22) +  
        geom_vline(xintercept=0)+
        geom_segment(aes(y=terms, x=diff-diff_cf, 
                         yend=terms, xend=diff+diff_cf, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_85, 
                         yend=terms, xend=diff+diff_cf_85, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_75, 
                         yend=terms, xend=diff+diff_cf_75, color=collab), alpha=0.6,
                     size=6)+
        geom_errorbarh(aes(y=terms, xmin=diff-diff_se, 
                           xmax=diff+diff_se), col="black", height=0.1)+
        geom_point(aes(y=terms, fill=collab, x=diff), color="black",
                   stat='identity', size=4, shape=21)+
        xlab(expression(paste(CO[2], " effect (ele - amb, %)"))) + 
        ylab("") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_discrete(limits=c("Soil P Conc 30-60cm",
                                  "Soil P Conc 10-30cm",
                                  "Soil P Conc 0-10cm",
                                  "Soil Inorg P Conc 30-60cm",
                                  "Soil Inorg P Conc 10-30cm",
                                  "Soil Inorg P Conc 0-10cm",
                                  "Soil Org P Conc 30-60cm",
                                  "Soil Org P Conc 10-30cm",
                                  "Soil Org P Conc 0-10cm",
                                  "Soil Phosphate P Conc 30-60cm",
                                  "Soil Phosphate P Conc 10-30cm",
                                  "Soil Phosphate P Conc 0-10cm",
                                  "Microbial P Conc 30-60cm",
                                  "Microbial P Conc 10-30cm",
                                  "Microbial P Conc 0-10cm"),
                         labels=c("Microbial P Conc 0-10cm" = "Microbial P (0-10cm)",
                                  "Microbial P Conc 10-30cm" = "Microbial P (10-30cm)",
                                  "Microbial P Conc 30-60cm" = "Microbial P (30-60cm)",
                                  "Soil P Conc 0-10cm" = "Soil P (0-10cm)",
                                  "Soil P Conc 10-30cm" = "Soil P (10-30cm)",
                                  "Soil P Conc 30-60cm" = "Soil P (30-60cm)",
                                  "Soil Inorg P Conc 0-10cm" = expression("Soil " * P[i] * " (0-10cm)"),
                                  "Soil Inorg P Conc 10-30cm" = expression("Soil " * P[i] * " (10-30cm)"),
                                  "Soil Inorg P Conc 30-60cm" = expression("Soil " * P[i] * " (30-60cm)"),
                                  "Soil Org P Conc 0-10cm" = expression("Soil " * P[o] * " (0-10cm)"),
                                  "Soil Org P Conc 10-30cm" = expression("Soil " * P[o] * " (10-30cm)"),
                                  "Soil Org P Conc 30-60cm" = expression("Soil " * P[o] * " (30-60cm)"),
                                  "Soil Phosphate P Conc 0-10cm" = expression("Soil " * PO[4]-P * " (0-10cm)"),
                                  "Soil Phosphate P Conc 10-30cm" = expression("Soil " * PO[4]-P * " (10-30cm)"),
                                  "Soil Phosphate P Conc 30-60cm" = expression("Soil " * PO[4]-P * " (30-60cm)")))+
        scale_fill_manual(name="",
                          labels=c("pos"="Positive",
                                   "neg"="Negative",
                                   "neut"="Zero"),
                          values=c("pos"="darkgreen", "neg"="brown", "neut"="black"))+
        scale_color_manual(name="",
                           labels=c("pos"="Positive",
                                    "neg"="Negative",
                                    "neut"="Zero"),
                           values=c("pos"="darkgreen", 
                                    "neg"="brown", 
                                    "neut"="black"))
    
    p43 <- ggplot(plotDF23) +  
        geom_vline(xintercept=0)+
        geom_segment(aes(y=terms, x=diff-diff_cf, 
                         yend=terms, xend=diff+diff_cf, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_85, 
                         yend=terms, xend=diff+diff_cf_85, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_75, 
                         yend=terms, xend=diff+diff_cf_75, color=collab), alpha=0.6,
                     size=6)+
        geom_errorbarh(aes(y=terms, xmin=diff-diff_se, 
                           xmax=diff+diff_se), col="black", height=0.1)+
        geom_point(aes(y=terms, fill=collab, x=diff), color="black",
                   stat='identity', size=4, shape=21)+
        xlab(expression(paste(CO[2], " effect (ele - amb, %)"))) + 
        ylab("") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_discrete(limits=c("Occluded P Conc 0-10cm",
                                  "Primary Ca bound Pi Conc 0-10cm",
                                  "Secondary Fe bound Pi Conc 0-10cm",
                                  "Moderately labile Po Conc 0-10cm",
                                  "Exchangeable Po Conc 0-10cm",
                                  "Exchangeable Pi Conc 0-10cm"),
                         labels=c("Exchangeable Pi Conc 0-10cm" = expression("Exchangeable " * P[i]),
                                  "Exchangeable Po Conc 0-10cm" = expression("Exchangeable " * P[o]),
                                  "Moderately labile Po Conc 0-10cm" = expression("Moderately labile " * P[o]),
                                  "Secondary Fe bound Pi Conc 0-10cm" = expression("Secondary " * F[e]-bound * " " * P[i]),
                                  "Primary Ca bound Pi Conc 0-10cm" = expression("Primary " * C[a]-bound * " " * P[i]),
                                  "Occluded P Conc 0-10cm" = expression("Occluded P")))+
        scale_fill_manual(name="",
                          labels=c("pos"="Positive",
                                   "neg"="Negative",
                                   "neut"="Zero"),
                          values=c("pos"="darkgreen", "neg"="brown", "neut"="black"))+
        scale_color_manual(name="",
                           labels=c("pos"="Positive",
                                    "neg"="Negative",
                                    "neut"="Zero"),
                           values=c("pos"="darkgreen", 
                                    "neg"="brown", 
                                    "neut"="black"))
    
    
    #p5 <- ggplot(plotDF3) +  
    #    geom_segment(aes(y=reorder(terms, diff), x=diff-diff_cf, 
    #                     yend=reorder(terms, diff), xend=diff+diff_cf), 
    #                 size=6, color="grey")+
    #    geom_point(aes(y=reorder(terms, diff), x=diff), 
    #               stat='identity', size=4, shape=19)+
    #    geom_vline(xintercept=0)+
    #    xlab(expression(paste(CO[2], " effect (g P ", m^-2, ")"))) + 
    #    ylab("") +
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.title.x = element_text(size=12, family="Helvetica"), 
    #          axis.text.x = element_text(size=12, family="Helvetica"),
    #          axis.text.y=element_text(size=12, family="Helvetica"),
    #          axis.title.y=element_text(size=12, family="Helvetica"),
    #          legend.text=element_text(size=12, family="Helvetica"),
    #          legend.title=element_text(size=12, family="Helvetica"),
    #          panel.grid.major=element_blank(),
    #          legend.position="bottom",
    #          legend.text.align=0)
    
    
    
    
    p51 <- ggplot(plotDF31) +  
        geom_vline(xintercept=0)+
        geom_segment(aes(y=terms, x=diff-diff_cf, 
                         yend=terms, xend=diff+diff_cf, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_85, 
                         yend=terms, xend=diff+diff_cf_85, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_75, 
                         yend=terms, xend=diff+diff_cf_75, color=collab), alpha=0.6,
                     size=6)+
        geom_errorbarh(aes(y=terms, xmin=diff-diff_se, 
                           xmax=diff+diff_se), col="black", height=0.1)+
        geom_point(aes(y=terms, fill=collab, x=diff), color="black",
                   stat='identity', size=4, shape=21)+
        xlab(expression(paste(CO[2], " effect (g P ", m^-2, ")"))) + 
        ylab("") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.box.background = element_rect(alpha("grey",0.5)),
              legend.position=c("none"),
              legend.text.align=0)+
        scale_y_discrete(limits=c("Standing Dead Wood P Pool",
                                  "Understorey Litter P Pool",
                                  "Understorey P Pool",
                                  "Forestfloor Leaf Litter P Pool",
                                  "Coarse Root P Pool",
                                  "Fine Root P Pool",
                                  "Total Wood P Pool",
                                  "Heartwood P Pool",
                                  "Sapwood P Pool",
                                  "Canopy P Pool"),
                         labels=c("Canopy P Pool" = "Canopy P",
                                  "Sapwood P Pool" = "Sapwood P",
                                  "Heartwood P Pool" = "Heartwood P",
                                  "Total Wood P Pool" = "Total Wood P",
                                  "Fine Root P Pool" = "Fine root P", 
                                  "Coarse Root P Pool" = "Coarse root P",
                                  "Forestfloor Leaf Litter P Pool" = "Forestfloor leaf P",
                                  "Understorey P Pool" = "Understorey P",
                                  "Understorey Litter P Pool" = "Understorey litter P",
                                  "Standing Dead Wood P Pool" = "Standing dead wood P"))+
        scale_fill_manual(name="Legend",
                          labels=c("pos"="Positive",
                                   "neg"="Negative",
                                   "neut"="Zero"),
                          values=c("pos"="darkgreen", "neg"="brown", "neut"="black"))+
        scale_color_manual(name="Legend",
                           labels=c("pos"="Positive",
                                    "neg"="Negative",
                                    "neut"="Zero"),
                           values=c("pos"="darkgreen", 
                                    "neg"="brown", 
                                    "neut"="black"))
    
    
    p52 <- ggplot(plotDF32) +  
        geom_vline(xintercept=0)+
        geom_segment(aes(y=terms, x=diff-diff_cf, 
                         yend=terms, xend=diff+diff_cf, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_85, 
                         yend=terms, xend=diff+diff_cf_85, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_75, 
                         yend=terms, xend=diff+diff_cf_75, color=collab), alpha=0.6,
                     size=6)+
        geom_errorbarh(aes(y=terms, xmin=diff-diff_se, 
                           xmax=diff+diff_se), col="black", height=0.1)+
        geom_point(aes(y=terms, fill=collab, x=diff), color="black",
                   stat='identity', size=4, shape=21)+
        xlab(expression(paste(CO[2], " effect (g P ", m^-2, ")"))) + 
        ylab("") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.box.background = element_rect(alpha("grey",0.5)),
              legend.position=c(0.78, 0.78),
              legend.text.align=0)+
        scale_y_discrete(limits=c("Soil P Pool 30-60cm",
                                  "Soil P Pool 10-30cm",
                                  "Soil P Pool 0-10cm",
                                  "Soil Inorg P Pool 30-60cm",
                                  "Soil Inorg P Pool 10-30cm",
                                  "Soil Inorg P Pool 0-10cm",
                                  "Soil Org P Pool 30-60cm",
                                  "Soil Org P Pool 10-30cm",
                                  "Soil Org P Pool 0-10cm",
                                  "Soil Phosphate P Pool 30-60cm",
                                  "Soil Phosphate P Pool 10-30cm",
                                  "Soil Phosphate P Pool 0-10cm",
                                  "Microbial P Pool 30-60cm",
                                  "Microbial P Pool 10-30cm",
                                  "Microbial P Pool 0-10cm"),
                         labels=c("Microbial P Pool 0-10cm" = "Microbial P (0-10cm)",
                                  "Microbial P Pool 10-30cm" = "Microbial P (10-30cm)",
                                  "Microbial P Pool 30-60cm" = "Microbial P (30-60cm)",
                                  "Soil P Pool 0-10cm" = "Soil P (0-10cm)",
                                  "Soil P Pool 10-30cm" = "Soil P (10-30cm)",
                                  "Soil P Pool 30-60cm" = "Soil P (30-60cm)",
                                  "Soil Inorg P Pool 0-10cm" = expression("Soil " * P[i] * " (0-10cm)"),
                                  "Soil Inorg P Pool 10-30cm" = expression("Soil " * P[i] * " (10-30cm)"),
                                  "Soil Inorg P Pool 30-60cm" = expression("Soil " * P[i] * " (30-60cm)"),
                                  "Soil Org P Pool 0-10cm" = expression("Soil " * P[o] * " (0-10cm)"),
                                  "Soil Org P Pool 10-30cm" = expression("Soil " * P[o] * " (10-30cm)"),
                                  "Soil Org P Pool 30-60cm" = expression("Soil " * P[o] * " (30-60cm)"),
                                  "Soil Phosphate P Pool 0-10cm" = expression("Soil " * PO[4]-P * " (0-10cm)"),
                                  "Soil Phosphate P Pool 10-30cm" = expression("Soil " * PO[4]-P * " (10-30cm)"),
                                  "Soil Phosphate P Pool 30-60cm" = expression("Soil " * PO[4]-P * " (30-60cm)")))+
        scale_fill_manual(name="",
                          labels=c("pos"="Positive",
                                   "neg"="Negative",
                                   "neut"="Zero"),
                          values=c("pos"="darkgreen", "neg"="brown", "neut"="black"))+
        scale_color_manual(name="",
                           labels=c("pos"="Positive",
                                    "neg"="Negative",
                                    "neut"="Zero"),
                           values=c("pos"="darkgreen", 
                                    "neg"="brown", 
                                    "neut"="black"))
    
    p53 <- ggplot(plotDF33) +  
        geom_vline(xintercept=0)+
        geom_segment(aes(y=terms, x=diff-diff_cf, 
                         yend=terms, xend=diff+diff_cf, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_85, 
                         yend=terms, xend=diff+diff_cf_85, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_75, 
                         yend=terms, xend=diff+diff_cf_75, color=collab), alpha=0.6,
                     size=6)+
        geom_errorbarh(aes(y=terms, xmin=diff-diff_se, 
                           xmax=diff+diff_se), col="black", height=0.1)+
        geom_point(aes(y=terms, fill=collab, x=diff), color="black",
                   stat='identity', size=4, shape=21)+
        xlab(expression(paste(CO[2], " effect (g P ", m^-2, ")"))) + 
        ylab("") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_discrete(limits=c("Occluded P Pool",
                                  "Primary Ca bound Pi Pool",
                                  "Secondary Fe bound Pi Pool",
                                  "Moderately labile Po Pool",
                                  "Exchangeable Po Pool",
                                  "Exchangeable Pi Pool"),
                         labels=c("Exchangeable Pi Pool" = expression("Exchangeable " * P[i]),
                                  "Exchangeable Po Pool" = expression("Exchangeable " * P[o]),
                                  "Moderately labile Po Pool" = expression("Moderately Labile " * P[o]),
                                  "Secondary Fe bound Pi Pool" = expression("Secondary " * F[e]-bound * " " * P[i]),
                                  "Primary Ca bound Pi Pool" = expression("Primary " * C[a]-bound * " " * P[i]),
                                  "Occluded P Pool" = expression("Occluded P")))+
        scale_fill_manual(name="",
                          labels=c("pos"="Positive",
                                   "neg"="Negative",
                                   "neut"="Zero"),
                          values=c("pos"="darkgreen", "neg"="brown", "neut"="black"))+
        scale_color_manual(name="",
                           labels=c("pos"="Positive",
                                    "neg"="Negative",
                                    "neut"="Zero"),
                           values=c("pos"="darkgreen", 
                                    "neg"="brown", 
                                    "neut"="black"))

    
    
    #p6 <- ggplot(plotDF4) +  
    #    geom_segment(aes(y=reorder(terms, diff), x=diff-diff_cf, 
    #                     yend=reorder(terms, diff), xend=diff+diff_cf), 
    #                 size=6, color="grey")+
    #    geom_point(aes(y=reorder(terms, diff), x=diff), 
    #               stat='identity', size=4, shape=19)+
    #    geom_vline(xintercept=0)+
    #    xlab(expression(paste(CO[2], " effect (g P ", m^-2, " ", yr^-1, ")"))) + 
    #    ylab("") +
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.title.x = element_text(size=12, family="Helvetica"), 
    #          axis.text.x = element_text(size=12, family="Helvetica"),
    #          axis.text.y=element_text(size=12, family="Helvetica"),
    #          axis.title.y=element_text(size=12, family="Helvetica"),
    #          legend.text=element_text(size=12, family="Helvetica"),
    #          legend.title=element_text(size=12, family="Helvetica"),
    #          panel.grid.major=element_blank(),
    #          legend.position="bottom",
    #          legend.text.align=0)
    
    
    
    
    p61 <- ggplot(plotDF41) +  
        geom_vline(xintercept=0)+
        geom_segment(aes(y=terms, x=diff-diff_cf, 
                         yend=terms, xend=diff+diff_cf, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_85, 
                         yend=terms, xend=diff+diff_cf_85, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_75, 
                         yend=terms, xend=diff+diff_cf_75, color=collab), alpha=0.6,
                     size=6)+
        geom_errorbarh(aes(y=terms, xmin=diff-diff_se, 
                           xmax=diff+diff_se), col="black", height=0.1)+
        geom_point(aes(y=terms, fill=collab, x=diff), color="black",
                   stat='identity', size=4, shape=21)+
        xlab(expression(paste(CO[2], " effect (g P ", m^-2, " ", yr^-1, ")"))) + 
        ylab("") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.box.background = element_rect(alpha("grey",0.5)),
              legend.position=c("none"),
              legend.text.align=0)+
        scale_y_discrete(limits=c("Understorey P flux",
                                  "Coarse Root P flux",
                                  "Fine Root P flux",
                                  "Wood P flux",
                                  "Canopy P flux"),
                         labels=c("Canopy P flux" = "Canopy P",
                                  "Wood P flux" = "Wood P",
                                  "Fine Root P flux" = "Fine root P", 
                                  "Coarse Root P flux" = "Coarse root P",
                                  "Understorey P flux" = "Understorey P"))+
        scale_fill_manual(name="Legend",
                          labels=c("pos"="Positive",
                                   "neg"="Negative",
                                   "neut"="Zero"),
                          values=c("pos"="darkgreen", "neg"="brown", "neut"="black"))+
        scale_color_manual(name="Legend",
                           labels=c("pos"="Positive",
                                    "neg"="Negative",
                                    "neut"="Zero"),
                           values=c("pos"="darkgreen", 
                                    "neg"="brown", 
                                    "neut"="black"))
    
    
    p62 <- ggplot(plotDF42) +  
        geom_vline(xintercept=0)+
        geom_segment(aes(y=terms, x=diff-diff_cf, 
                         yend=terms, xend=diff+diff_cf, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_85, 
                         yend=terms, xend=diff+diff_cf_85, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_75, 
                         yend=terms, xend=diff+diff_cf_75, color=collab), alpha=0.6,
                     size=6)+
        geom_errorbarh(aes(y=terms, xmin=diff-diff_se, 
                           xmax=diff+diff_se), col="black", height=0.1)+
        geom_point(aes(y=terms, fill=collab, x=diff), color="black",
                   stat='identity', size=4, shape=21)+
        xlab(expression(paste(CO[2], " effect (g P ", m^-2, " ", yr^-1, ")"))) + 
        ylab("") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.box.background = element_rect(alpha("grey",0.5)),
              legend.position=c(0.7, 0.78),
              legend.text.align=0)+
        scale_y_discrete(limits=c("Frass P flux",
                                  "Understorey Litter P flux",
                                  "Fineroot Litter P flux",
                                  "Seed litter P flux",
                                  "Bark litter P flux",
                                  "Twig litter P flux",
                                  "Leaflitter P flux"),
                         labels=c("Leaflitter P flux" = "Leaf litter P",
                                  "Twig litter P flux" = "Twig litter P",
                                  "Bark litter P flux" = "Bark litter P",
                                  "Seed litter P flux" = "Seed litter P",
                                  "Fineroot Litter P flux" = "Fine root litter P", 
                                  "Understorey Litter P flux" = "Understorey litter P",
                                  "Frass P flux" = "Frass P"))+
        scale_fill_manual(name="",
                          labels=c("pos"="Positive",
                                   "neg"="Negative",
                                   "neut"="Zero"),
                          values=c("pos"="darkgreen", "neg"="brown", "neut"="black"))+
        scale_color_manual(name="",
                           labels=c("pos"="Positive",
                                    "neg"="Negative",
                                    "neut"="Zero"),
                           values=c("pos"="darkgreen", 
                                    "neg"="brown", 
                                    "neut"="black"))
    
    
    p63 <- ggplot(plotDF43) +  
        geom_vline(xintercept=0)+
        geom_segment(aes(y=terms, x=diff-diff_cf, 
                         yend=terms, xend=diff+diff_cf, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_85, 
                         yend=terms, xend=diff+diff_cf_85, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_75, 
                         yend=terms, xend=diff+diff_cf_75, color=collab), alpha=0.6,
                     size=6)+
        geom_errorbarh(aes(y=terms, xmin=diff-diff_se, 
                           xmax=diff+diff_se), col="black", height=0.1)+
        geom_point(aes(y=terms, fill=collab, x=diff), color="black",
                   stat='identity', size=4, shape=21)+
        xlab(expression(paste(CO[2], " effect (g P ", m^-2, " ", yr^-1, ")"))) + 
        ylab("") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.box.background = element_rect(alpha("grey",0.5)),
              legend.position=c("none"),
              legend.text.align=0)+
        scale_y_discrete(limits=c("Understorey retrans P flux",
                                  "Coarseroot retrans P flux",
                                  "Fineroot retrans P flux",
                                  "Sapwood retrans P flux",
                                  "Canopy retrans P flux"),
                         labels=c("Canopy retrans P flux" = "Canopy retrans P",
                                  "Sapwood retrans P flux" = "Sapwood retrans P",
                                  "Fineroot retrans P flux" = "Fineroot retrans P", 
                                  "Coarseroot retrans P flux" = "Coarseroot retrans P",
                                  "Understorey retrans P flux" = "Understorey retrans P"))+
        scale_fill_manual(name="Legend",
                          labels=c("pos"="Positive",
                                   "neg"="Negative",
                                   "neut"="Zero"),
                          values=c("pos"="darkgreen", "neg"="brown", "neut"="black"))+
        scale_color_manual(name="Legend",
                           labels=c("pos"="Positive",
                                    "neg"="Negative",
                                    "neut"="Zero"),
                           values=c("pos"="darkgreen", 
                                    "neg"="brown", 
                                    "neut"="black"))
    
    p64 <- ggplot(plotDF44) +  
        geom_vline(xintercept=0)+
        geom_segment(aes(y=terms, x=diff-diff_cf, 
                         yend=terms, xend=diff+diff_cf, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_85, 
                         yend=terms, xend=diff+diff_cf_85, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_75, 
                         yend=terms, xend=diff+diff_cf_75, color=collab), alpha=0.6,
                     size=6)+
        geom_errorbarh(aes(y=terms, xmin=diff-diff_se, 
                           xmax=diff+diff_se), col="black", height=0.1)+
        geom_point(aes(y=terms, fill=collab, x=diff), color="black",
                   stat='identity', size=4, shape=21)+
        xlab(expression(paste(CO[2], " effect (g P ", m^-2, " ", yr^-1, ")"))) + 
        ylab("") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_discrete(limits=c("Mineralization P flux 30-60cm",
                                  "Mineralization P flux 10-30cm",
                                  "Mineralization P flux 0-10cm",
                                  "Total vegetation uptake P flux",
                                  "Total vegetation retranslocation P flux",
                                  "Total vegetation production P flux"),
                         labels=c("Mineralization P flux 30-60cm" = expression("Net " * P[min] * " 30-60cm"),
                                  "Mineralization P flux 10-30cm" = expression("Net " * P[min] * " 10-30cm"),
                                  "Mineralization P flux 0-10cm" = expression("Net " * P[min] * " 0-10cm"),
                                  "Total vegetation uptake P flux" = "Plant P uptake",
                                  "Total vegetation retranslocation P flux" = "Plant P resorption",
                                  "Total vegetation production P flux" = "Plant P demand"))+
        scale_fill_manual(name="",
                          labels=c("pos"="Positive",
                                   "neg"="Negative",
                                   "neut"="Zero"),
                          values=c("pos"="darkgreen", "neg"="brown", "neut"="black"))+
        scale_color_manual(name="",
                           labels=c("pos"="Positive",
                                    "neg"="Negative",
                                    "neut"="Zero"),
                           values=c("pos"="darkgreen", 
                                    "neg"="brown", 
                                    "neut"="black"))
    
    
    plotDF45 <- rbind(plotDF41, rbind(plotDF42, plotDF43))
    
    p65 <- ggplot(plotDF45) +  
        geom_vline(xintercept=0)+
        geom_segment(aes(y=terms, x=diff-diff_cf, 
                         yend=terms, xend=diff+diff_cf, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_85, 
                         yend=terms, xend=diff+diff_cf_85, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_75, 
                         yend=terms, xend=diff+diff_cf_75, color=collab), alpha=0.6,
                     size=6)+
        geom_errorbarh(aes(y=terms, xmin=diff-diff_se, 
                           xmax=diff+diff_se), col="black", height=0.1)+
        geom_point(aes(y=terms, fill=collab, x=diff), color="black",
                   stat='identity', size=4, shape=21)+
        xlab(expression(paste(CO[2], " effect (g P ", m^-2, " ", yr^-1, ")"))) + 
        ylab("") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.box.background = element_rect(alpha("grey",0.5)),
              legend.position=c("none"),
              legend.text.align=0)+
        scale_y_discrete(limits=c("Frass P flux",
                                  "Understorey Litter P flux",
                                  "Fineroot Litter P flux",
                                  "Seed litter P flux",
                                  "Bark litter P flux",
                                  "Twig litter P flux",
                                  "Leaflitter P flux",
                                  "Understorey retrans P flux",
                                  #"Coarseroot retrans P flux",
                                  #"Fineroot retrans P flux",
                                  "Sapwood retrans P flux",
                                  "Canopy retrans P flux",
                                  "Understorey P flux",
                                  "Coarse Root P flux",
                                  "Fine Root P flux",
                                  "Wood P flux",
                                  "Canopy P flux"),
                         labels=c("Canopy P flux" = "Canopy P",
                                  "Wood P flux" = "Wood P",
                                  "Fine Root P flux" = "Fineroot P", 
                                  "Coarse Root P flux" = "Coarseroot P",
                                  "Understorey P flux" = "Understorey P",
                                  "Leaflitter P flux" = "Leaf litter P",
                                  "Twig litter P flux" = "Twig litter P",
                                  "Bark litter P flux" = "Bark litter P",
                                  "Seed litter P flux" = "Seed litter P",
                                  "Fineroot Litter P flux" = "Fineroot litter P", 
                                  "Understorey Litter P flux" = "Understorey litter P",
                                  "Frass P flux" = "Frass P",
                                  "Canopy retrans P flux" = "Canopy retrans P",
                                  "Sapwood retrans P flux" = "Sapwood retrans P",
                                  #"Fineroot retrans P flux" = "Fineroot retrans P", 
                                  #"Coarseroot retrans P flux" = "Coarseroot retrans P",
                                  "Understorey retrans P flux" = "Understorey retrans P"))+
        scale_fill_manual(name="Legend",
                          labels=c("pos"="Positive",
                                   "neg"="Negative",
                                   "neut"="Zero"),
                          values=c("pos"="darkgreen", "neg"="brown", "neut"="black"))+
        scale_color_manual(name="Legend",
                           labels=c("pos"="Positive",
                                    "neg"="Negative",
                                    "neut"="Zero"),
                           values=c("pos"="darkgreen", 
                                    "neg"="brown", 
                                    "neut"="black"))
    
    
    p71 <- ggplot(plotDF51) +  
        geom_vline(xintercept=0)+
        geom_segment(aes(y=terms, x=diff-diff_cf, 
                         yend=terms, xend=diff+diff_cf, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_85, 
                         yend=terms, xend=diff+diff_cf_85, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_75, 
                         yend=terms, xend=diff+diff_cf_75, color=collab), alpha=0.6,
                     size=6)+
        geom_errorbarh(aes(y=terms, xmin=diff-diff_se, 
                           xmax=diff+diff_se), col="black", height=0.1)+
        geom_point(aes(y=terms, fill=collab, x=diff), color="black",
                   stat='identity', size=4, shape=21)+
        xlab(expression(paste(CO[2], " effect (g P ", m^-2, " ", yr^-1, ")"))) + 
        ylab("") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.box.background = element_rect(alpha("grey",0.5)),
              legend.position=c("none"),
              legend.text.align=0)+
        scale_y_discrete(limits=c("Understorey P Pool",
                                  "Forestfloor Leaf Litter P Pool",
                                  "Coarse Root P Pool",
                                  "Fine Root P Pool",
                                  "Total Wood P Pool",
                                  "Heartwood P Pool",
                                  "Sapwood P Pool",
                                  "Canopy P Pool"),
                         labels=c("Canopy P Pool" = expression(Delta * " Canopy P"),
                                  "Sapwood P Pool" = expression(Delta * " Sapwood P"),
                                  "Heartwood P Pool" = expression(Delta * " Heartwood P"),
                                  "Total Wood P Pool" = expression(Delta * " Total wood P"),
                                  "Fine Root P Pool" = expression(Delta * " Fine root P"), 
                                  "Coarse Root P Pool" = expression(Delta * " Coarse root P"),
                                  "Forestfloor Leaf Litter P Pool" = expression(Delta * " Forestfloor leaf P"),
                                  "Understorey P Pool" = expression(Delta * " Understorey P")))+
        scale_fill_manual(name="Legend",
                          labels=c("pos"="Positive",
                                   "neg"="Negative",
                                   "neut"="Zero"),
                          values=c("pos"="darkgreen", "neg"="brown", "neut"="black"))+
        scale_color_manual(name="Legend",
                           labels=c("pos"="Positive",
                                    "neg"="Negative",
                                    "neut"="Zero"),
                           values=c("pos"="darkgreen", 
                                    "neg"="brown", 
                                    "neut"="black"))
    
    
    p72 <- ggplot(plotDF52) +  
        geom_vline(xintercept=0)+
        geom_segment(aes(y=terms, x=diff-diff_cf, 
                         yend=terms, xend=diff+diff_cf, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_85, 
                         yend=terms, xend=diff+diff_cf_85, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_75, 
                         yend=terms, xend=diff+diff_cf_75, color=collab), alpha=0.6,
                     size=6)+
        geom_errorbarh(aes(y=terms, xmin=diff-diff_se, 
                           xmax=diff+diff_se), col="black", height=0.1)+
        geom_point(aes(y=terms, fill=collab, x=diff), color="black",
                   stat='identity', size=4, shape=21)+
        xlab(expression(paste(CO[2], " effect (g P ", m^-2, " ", yr^-1, ")"))) + 
        ylab("") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.box.background = element_rect(alpha("grey",0.5)),
              legend.position="none",
              legend.text.align=0)+
        scale_y_discrete(limits=c("Soil P Pool 10-30cm",
                                  "Soil P Pool 0-10cm",
                                  "Soil Phosphate P Pool 10-30cm",
                                  "Soil Phosphate P Pool 0-10cm",
                                  "Microbial P Pool 30-60cm",
                                  "Microbial P Pool 10-30cm",
                                  "Microbial P Pool 0-10cm"),
                         labels=c("Microbial P Pool 0-10cm" = expression(Delta * " Microbial P (0-10cm)"),
                                  "Microbial P Pool 10-30cm" = expression(Delta * " Microbial P (10-30cm)"),
                                  "Microbial P Pool 30-60cm" = expression(Delta * " Microbial P (30-60cm)"),
                                  "Soil P Pool 0-10cm" = expression(Delta * " Soil P (0-10cm)"),
                                  "Soil P Pool 10-30cm" = expression(Delta * " Soil P (10-30cm)"),
                                  "Soil Phosphate P Pool 0-10cm" = expression(Delta * " Phosphate P (0-10cm)"),
                                  "Soil Phosphate P Pool 10-30cm" = expression(Delta * " Phosphate P (10-30cm)")))+
        scale_fill_manual(name="",
                          labels=c("pos"="Positive",
                                   "neg"="Negative",
                                   "neut"="Zero"),
                          values=c("pos"="darkgreen", "neg"="brown", "neut"="black"))+
        scale_color_manual(name="",
                           labels=c("pos"="Positive",
                                    "neg"="Negative",
                                    "neut"="Zero"),
                           values=c("pos"="darkgreen", 
                                    "neg"="brown", 
                                    "neut"="black"))
    
    
    plotDF53 <- plotDF52[plotDF52$terms%in%c("Microbial P Pool 0-10cm",
                                             "Microbial P Pool 10-30cm",
                                             "Microbial P Pool 30-60cm",
                                             "Soil Phosphate P Pool 0-10cm",
                                             "Soil Phosphate P Pool 10-30cm"),]
    
    
    plotDF531 <- plotDF52[plotDF52$terms%in%c("Microbial P Pool 0-10cm",
                                             "Microbial P Pool 10-30cm",
                                             "Microbial P Pool 30-60cm"),]
    
    
    plotDF532 <- plotDF52[plotDF52$terms%in%c("Soil Phosphate P Pool 0-10cm",
                                             "Soil Phosphate P Pool 10-30cm"),]
    
    
    plotDF54 <- plotDF52[plotDF52$terms%in%c("Soil P Pool 0-10cm",
                                             "Soil P Pool 10-30cm"),]
    
    
    p73 <- ggplot(plotDF531) +  
        geom_vline(xintercept=0)+
        geom_segment(aes(y=terms, x=diff-diff_cf, 
                         yend=terms, xend=diff+diff_cf, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_85, 
                         yend=terms, xend=diff+diff_cf_85, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_75, 
                         yend=terms, xend=diff+diff_cf_75, color=collab), alpha=0.6,
                     size=6)+
        geom_errorbarh(aes(y=terms, xmin=diff-diff_se, 
                           xmax=diff+diff_se), col="black", height=0.1)+
        geom_point(aes(y=terms, fill=collab, x=diff), color="black",
                   stat='identity', size=4, shape=21)+
        xlab(expression(paste(CO[2], " effect (g P ", m^-2, " ", yr^-1, ")"))) + 
        ylab("") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.box.background = element_rect(alpha("grey",0.5)),
              legend.position="none",
              legend.text.align=0)+
        scale_y_discrete(limits=c("Microbial P Pool 30-60cm",
                                  "Microbial P Pool 10-30cm",
                                  "Microbial P Pool 0-10cm"),
                         labels=c("Microbial P Pool 0-10cm" = expression(Delta * " Microbial P 0-10cm"),
                                  "Microbial P Pool 10-30cm" = expression(Delta * " Microbial P 10-30cm"),
                                  "Microbial P Pool 30-60cm" = expression(Delta * " Microbial P 30-60cm")))+
        scale_fill_manual(name="",
                          labels=c("pos"="Positive",
                                   "neg"="Negative",
                                   "neut"="Zero"),
                          values=c("pos"="darkgreen", "neg"="brown", "neut"="black"))+
        scale_color_manual(name="",
                           labels=c("pos"="Positive",
                                    "neg"="Negative",
                                    "neut"="Zero"),
                           values=c("pos"="darkgreen", 
                                    "neg"="brown", 
                                    "neut"="black"))
    
    
    
    p74 <- ggplot(plotDF532) +  
        geom_vline(xintercept=0)+
        geom_segment(aes(y=terms, x=diff-diff_cf, 
                         yend=terms, xend=diff+diff_cf, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_85, 
                         yend=terms, xend=diff+diff_cf_85, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_75, 
                         yend=terms, xend=diff+diff_cf_75, color=collab), alpha=0.6,
                     size=6)+
        geom_errorbarh(aes(y=terms, xmin=diff-diff_se, 
                           xmax=diff+diff_se), col="black", height=0.1)+
        geom_point(aes(y=terms, fill=collab, x=diff), color="black",
                   stat='identity', size=4, shape=21)+
        xlab(expression(paste(CO[2], " effect (g P ", m^-2, " ", yr^-1, ")"))) + 
        ylab("") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.box.background = element_rect(alpha("grey",0.5)),
              legend.position="none",
              legend.text.align=0)+
        scale_y_discrete(limits=c("Soil Phosphate P Pool 10-30cm",
                                  "Soil Phosphate P Pool 0-10cm"),
                         labels=c("Soil Phosphate P Pool 0-10cm" = expression(Delta * " Phosphate-P 0-10cm"),
                                  "Soil Phosphate P Pool 10-30cm" = expression(Delta * " Phosphate-P 10-30cm")))+
        scale_fill_manual(name="",
                          labels=c("pos"="Positive",
                                   "neg"="Negative",
                                   "neut"="Zero"),
                          values=c("pos"="darkgreen", "neg"="brown", "neut"="black"))+
        scale_color_manual(name="",
                           labels=c("pos"="Positive",
                                    "neg"="Negative",
                                    "neut"="Zero"),
                           values=c("pos"="darkgreen", 
                                    "neg"="brown", 
                                    "neut"="black"))
    
    
    
    p75 <- ggplot(plotDF54) +  
        geom_vline(xintercept=0)+
        geom_segment(aes(y=terms, x=diff-diff_cf, 
                         yend=terms, xend=diff+diff_cf, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_85, 
                         yend=terms, xend=diff+diff_cf_85, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_75, 
                         yend=terms, xend=diff+diff_cf_75, color=collab), alpha=0.6,
                     size=6)+
        geom_errorbarh(aes(y=terms, xmin=diff-diff_se, 
                           xmax=diff+diff_se), col="black", height=0.1)+
        geom_point(aes(y=terms, fill=collab, x=diff), color="black",
                   stat='identity', size=4, shape=21)+
        xlab(expression(paste(CO[2], " effect (g P ", m^-2, " ", yr^-1, ")"))) + 
        ylab("") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.box.background = element_rect(alpha("grey",0.5)),
              legend.position="none",
              legend.text.align=0)+
        scale_y_discrete(limits=c("Soil P Pool 10-30cm",
                                  "Soil P Pool 0-10cm"),
                         labels=c("Soil P Pool 0-10cm" = expression(Delta * " Soil P (0-10cm)"),
                                  "Soil P Pool 10-30cm" = expression(Delta * " Soil P (10-30cm)")))+
        scale_fill_manual(name="",
                          labels=c("pos"="Positive",
                                   "neg"="Negative",
                                   "neut"="Zero"),
                          values=c("pos"="darkgreen", "neg"="brown", "neut"="black"))+
        scale_color_manual(name="",
                           labels=c("pos"="Positive",
                                    "neg"="Negative",
                                    "neut"="Zero"),
                           values=c("pos"="darkgreen", 
                                    "neg"="brown", 
                                    "neut"="black"))
    
    
    
    plotDF1$collab <- ifelse(plotDF1$diff > 0.0, "pos", 
                              ifelse(plotDF1$diff < 0.0, "neg", "neut"))
    
    
    plotDF81 <- plotDF1[plotDF1$terms%in%c("Plant P MRT", "Microbe P MRT"),]
    
    plotDF82 <- plotDF1[plotDF1$terms%in%c("Plant PUE", "Plant GPP efficiency"),]
    
    plotDF83 <- plotDF1[plotDF1$terms%in%c("Overstorey GPP efficiency",
                                           "Understorey GPP efficiency"),]
    
    
    ### add both GPP PUE together
    
    
    
    p81 <- ggplot(plotDF81) +  
        geom_vline(xintercept=0)+
        geom_segment(aes(y=terms, x=diff-diff_cf, 
                         yend=terms, xend=diff+diff_cf, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_85, 
                         yend=terms, xend=diff+diff_cf_85, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_75, 
                         yend=terms, xend=diff+diff_cf_75, color=collab), alpha=0.6,
                     size=6)+
        geom_errorbarh(aes(y=terms, xmin=diff-diff_se, 
                           xmax=diff+diff_se), col="black", height=0.1)+
        geom_point(aes(y=terms, fill=collab, x=diff), color="black",
                   stat='identity', size=4, shape=21)+
        xlab(expression(paste(CO[2], " effect (yr)"))) + 
        ylab("") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.box.background = element_rect(alpha("grey",0.5)),
              legend.position="none",
              legend.text.align=0)+
        scale_y_discrete(limits=c("Plant P MRT", "Microbe P MRT"),
                         labels=c("Plant P MRT" = expression("Plant P MRT"),
                                  "Microbe P MRT" = expression("Microbial P MRT")))+
        scale_fill_manual(name="",
                          labels=c("pos"="Positive",
                                   "neg"="Negative",
                                   "neut"="Zero"),
                          values=c("pos"="darkgreen", "neg"="brown", "neut"="black"))+
        scale_color_manual(name="",
                           labels=c("pos"="Positive",
                                    "neg"="Negative",
                                    "neut"="Zero"),
                           values=c("pos"="darkgreen", 
                                    "neg"="brown", 
                                    "neut"="black"))
    
    
    
    
    p82 <- ggplot(plotDF83) +  
        geom_vline(xintercept=0)+
        geom_segment(aes(y=terms, x=diff-diff_cf, 
                         yend=terms, xend=diff+diff_cf, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_85, 
                         yend=terms, xend=diff+diff_cf_85, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_75, 
                         yend=terms, xend=diff+diff_cf_75, color=collab), alpha=0.6,
                     size=6)+
        geom_errorbarh(aes(y=terms, xmin=diff-diff_se, 
                           xmax=diff+diff_se), col="black", height=0.1)+
        geom_point(aes(y=terms, fill=collab, x=diff), color="black",
                   stat='identity', size=4, shape=21)+
        xlab(expression(paste(CO[2], " effect (g C g ", P^-1, ")"))) + 
        ylab("") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.box.background = element_rect(alpha("grey",0.5)),
              legend.position="none",
              legend.text.align=0)+
        scale_y_discrete(limits=c("Understorey GPP efficiency",
                                  "Overstorey GPP efficiency"),
                         labels=c("Understorey GPP efficiency" = expression("   " * GPP[u] * " PUE"),
                                  "Overstorey GPP efficiency" = expression("   " * GPP[o] * " PUE")))+
        scale_fill_manual(name="",
                          labels=c("pos"="Positive",
                                   "neg"="Negative",
                                   "neut"="Zero"),
                          values=c("pos"="darkgreen", "neg"="brown", "neut"="black"))+
        scale_color_manual(name="",
                           labels=c("pos"="Positive",
                                    "neg"="Negative",
                                    "neut"="Zero"),
                           values=c("pos"="darkgreen", 
                                    "neg"="brown", 
                                    "neut"="black"))
    
    p83 <- ggplot(plotDF82) +  
        geom_vline(xintercept=0)+
        geom_segment(aes(y=terms, x=diff-diff_cf, 
                         yend=terms, xend=diff+diff_cf, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_85, 
                         yend=terms, xend=diff+diff_cf_85, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_75, 
                         yend=terms, xend=diff+diff_cf_75, color=collab), alpha=0.6,
                     size=6)+
        geom_errorbarh(aes(y=terms, xmin=diff-diff_se, 
                           xmax=diff+diff_se), col="black", height=0.1)+
        geom_point(aes(y=terms, fill=collab, x=diff), color="black",
                   stat='identity', size=4, shape=21)+
        xlab(expression(paste(CO[2], " effect (g C g ", P^-1, ")"))) + 
        ylab("") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.box.background = element_rect(alpha("grey",0.5)),
              legend.position="none",
              legend.text.align=0)+
        scale_y_discrete(limits=c("Plant PUE", "Plant GPP efficiency"),
                         labels=c("Plant PUE" = "Growth PUE",
                                  "Plant GPP efficiency" = "GPP PUE"))+
        scale_fill_manual(name="",
                          labels=c("pos"="Positive",
                                   "neg"="Negative",
                                   "neut"="Zero"),
                          values=c("pos"="darkgreen", "neg"="brown", "neut"="black"))+
        scale_color_manual(name="",
                           labels=c("pos"="Positive",
                                    "neg"="Negative",
                                    "neut"="Zero"),
                           values=c("pos"="darkgreen", 
                                    "neg"="brown", 
                                    "neut"="black"))
    
    
    
    
    require(grid)
    require(cowplot)
    
    ### Plotting
    #pdf("plots_tables/output/unnormalized/CO2_effect_on_total_p_budget.pdf", 
    #    width=6, height=12)
    #     #width=150, height=300, unit="mm", res = 300)
    #plot_grid(p1, p2, p3, labels="", ncol=1, align="h", axis="l",
    #          rel_heights=c(1., 1.0, 1.5))
    #dev.off()
    
    #pdf("plots_tables/output/unnormalized/CO2_effect_on_all_p_variables.pdf", 
    #     width=6, height=24)
    #plot_grid(p4, p5, p6, labels="", ncol=1, align="h", axis="l",
    #          rel_heights=c(1.0, 1.0, 1.0))
    #dev.off()
    
    
    
    
    p91 <- ggplot(plotDF6) +  
        geom_vline(xintercept=0)+
        geom_segment(aes(y=terms, x=diff-diff_cf, 
                         yend=terms, xend=diff+diff_cf, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_85, 
                         yend=terms, xend=diff+diff_cf_85, color=collab), alpha=0.2,
                     size=6)+
        geom_segment(aes(y=terms, x=diff-diff_cf_75, 
                         yend=terms, xend=diff+diff_cf_75, color=collab), alpha=0.6,
                     size=6)+
        geom_errorbarh(aes(y=terms, xmin=diff-diff_se, 
                           xmax=diff+diff_se), col="black", height=0.1)+
        geom_point(aes(y=terms, fill=collab, x=diff), color="black",
                   stat='identity', size=4, shape=21)+
        xlab(expression(paste(CO[2], " effect (yr)"))) + 
        ylab("") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.box.background = element_rect(alpha("grey",0.5)),
              legend.position="none",
              legend.text.align=0)+
        #scale_y_discrete(limits=c("Plant P MRT", "Microbe P MRT"),
        #                 labels=c("Plant P MRT" = expression("Plant P MRT"),
        #                          "Microbe P MRT" = expression("Microbial P MRT")))+
        scale_fill_manual(name="",
                          labels=c("pos"="Positive",
                                   "neg"="Negative",
                                   "neut"="Zero"),
                          values=c("pos"="darkgreen", "neg"="brown", "neut"="black"))+
        scale_color_manual(name="",
                           labels=c("pos"="Positive",
                                    "neg"="Negative",
                                    "neut"="Zero"),
                           values=c("pos"="darkgreen", 
                                    "neg"="brown", 
                                    "neut"="black"))
    
    #plot(p91)
    
    grid.labs <- c("(a)", "(b)", "(c)")
    
    pdf("plots_tables/output/unnormalized/CO2_effect_on_P_concentration.pdf", 
        width=10, height=6)
    left_col <- plot_grid(p41, p43, ncol=1, rel_heights=c(1,0.8))
    plot_grid(left_col, p42,  ncol = 2, rel_widths = c(1, 1),
              rel_heights=c(1, 1))
    grid.text(grid.labs,x = c(0.45, 0.45, 0.95), y = c(0.95, 0.4, 0.95),
              gp=gpar(fontsize=16, col="black", fontface="bold"))
    dev.off()
    
    
    grid.labs <- c("(a)", "(b)", "(c)", "(d)", "(e)")
    
    pdf("plots_tables/output/unnormalized/CO2_effect_on_P_pool.pdf", 
        width=10, height=10)
    left_col <- plot_grid(p51, p53, ncol=1, rel_heights=c(1,0.8))
    top_row <- plot_grid(left_col, p52,  ncol = 2, rel_widths = c(1, 1),
                         rel_heights=c(1, 1))
    bot_row <- plot_grid(p71, p72, ncol = 2, rel_widths = c(1, 1),
                         rel_heights=c(0.6, 1))
    
    plot_grid(top_row, bot_row, ncol = 1, rel_widths = c(1, 1),
              rel_heights=c(2, 1))
    
    grid.text(grid.labs,x = c(0.47, 0.96, 0.47, 0.47, 0.96), 
              y = c(0.96, 0.96, 0.6, 0.3, 0.3),
              gp=gpar(fontsize=16, col="black", fontface="bold"))
    
    
    #left_col <- plot_grid(p51, p53, p71, ncol=1, rel_heights=c(1.1,0.9,1))
    #right_col <- plot_grid(p52, p75, ncol = 1, rel_widths = c(1, 1),
    #                     rel_heights=c(1.1,0.25))
    #
    #plot_grid(left_col, right_col, ncol = 2, rel_widths = c(1, 1),
    #          rel_heights=c(1, 1))
    #
    #grid.text(grid.labs,x = c(0.47, 0.96, 0.47, 0.47, 0.96), 
    #          y = c(0.96, 0.96, 0.61, 0.3, 0.16),
    #          gp=gpar(fontsize=16, col="black", fontface="bold"))
    
    dev.off()
    
    
    ### fluxes 1
    #grid.labs <- c("(a)", "(c)", "(d)", "(b)")
    #
    #pdf("plots_tables/output/unnormalized/CO2_effect_on_P_flux.pdf", 
    #    width=12, height=6)
    #left_col <- plot_grid(p64, p73, p74, ncol=1, rel_heights=c(1.0, 0.6, 0.5))
    #
    #plot_grid(left_col, p65, ncol = 2, rel_widths = c(1, 1),
    #          rel_heights=c(1, 1))
    #grid.text(grid.labs,x = c(0.45, 0.45, 0.45, 0.95), y = c(0.95, 0.48, 0.2, 0.95),
    #          gp=gpar(fontsize=16, col="black", fontface="bold"))
    #dev.off()
    
    ### fluxes 2
    #grid.labs <- c("(a)", "(b)", "(c)", "(d)")
    #
    #pdf("plots_tables/output/unnormalized/CO2_effect_on_P_flux2.pdf", 
    #    width=12, height=6)
    #plot_grid(p64, p61, p63, p62, ncol = 2, rel_widths = c(1, 1, 1, 1),
    #          rel_heights=c(0.6, 0.6, 1, 1))
    #grid.text(grid.labs,x = c(0.47, 0.96, 0.47, 0.96), y = c(0.95, 0.95, 0.45, 0.45),
    #          gp=gpar(fontsize=16, col="black", fontface="bold"))
    #dev.off()
    
    
    
    #grid.labs <- c("(a)", "(b)", "(c)", "(d)")
    #
    #pdf("plots_tables/output/unnormalized/CO2_effect_on_P_flux2.pdf", 
    #    width=12, height=6)
    #
    #topright_row <- plot_grid(p81, p82, p83, ncol=1, rel_heights=c(1, 1.2, 1))
    #top_row <- plot_grid(p64, topright_row, ncol=2, rel_widths=c(1, 1))
    #bot_row <- plot_grid(p61, p63, ncol=2, rel_widths=c(1, 1))
    #
    #
    #plot_grid(top_row, bot_row, ncol = 1, rel_widths = c(1, 1),
    #          rel_heights=c(1, 1))
    #grid.text(grid.labs,x = c(0.47, 0.96, 0.47, 0.96), y = c(0.95, 0.95, 0.45, 0.45),
    #          gp=gpar(fontsize=16, col="black", fontface="bold"))
    #dev.off()
    
    
    
    
    ### revise 
    grid.labs <- c("(a)", "(b)", "(c)", "(d)")#, "(e)", "(f)")
    
    pdf("plots_tables/output/unnormalized/CO2_effect_on_P_flux3.pdf", 
        width=12, height=4)
    
    #topright_row <- plot_grid(p81, p82, p83, ncol=1, rel_heights=c(1.4, 1.4, 1))
    topright_row <- plot_grid(p81, p82, p83, ncol=1, rel_heights=c(1, 1, 1))
    
    #top_row <- plot_grid(p64, NULL, topright_row, ncol=3, rel_widths=c(1, 0.05, 1))
    plot_grid(p64, NULL, topright_row, ncol=3, rel_widths=c(1, 0.05, 1))
    
    #bot_row <- plot_grid(p74, NULL, p73, ncol=3, rel_widths=c(1, 0.05, 1))

    #plot_grid(top_row, bot_row, ncol = 1, rel_widths = c(1, 1), scale=0.95,
    #          rel_heights=c(1, 0.5))
    #grid.text(grid.labs,
    #          x = c(0.03, 0.52, 0.52, 0.52, 
    #                0.03, 0.52), 
    #          y = c(0.95, 0.95, 0.77, 0.55, 
    #                0.30, 0.30),
    #          gp=gpar(fontsize=16, col="black", fontface="bold"))
    grid.text(grid.labs,
              x = c(0.03, 0.52, 0.52, 0.522), 
              y = c(0.96, 0.96, 0.64, 0.35),
              gp=gpar(fontsize=16, col="black", fontface="bold"))
    
    dev.off()
    
    
     ### delta pools
    grid.labs <- c("(a)", "(b)")
    
    pdf("plots_tables/output/unnormalized/CO2_effect_on_delta_P_pool.pdf", 
        width=10, height=4)
    plot_grid(p71, p72, ncol = 2, rel_widths = c(1, 1),
              rel_heights=c(0.6, 1))
    grid.text(grid.labs,x = c(0.47, 0.96), y = c(0.95, 0.95),
              gp=gpar(fontsize=16, col="black", fontface="bold"))
    dev.off()
    
}
