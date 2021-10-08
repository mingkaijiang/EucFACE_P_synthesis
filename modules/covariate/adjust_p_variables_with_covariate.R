adjust_p_variables_with_covariate <- function(inDF, 
                                              corDF.adj, 
                                              var.col,
                                              with.depth.profile,
                                              plot.comparison,
                                              return.outcome) {
    
    ### Purpose:
    ### To adjust all the P fluxes and pool variables based on covariate input
    
    ### prepare the dataframes
    inDF$Ring <- as.numeric(inDF$Ring)
    var.name <- colnames(inDF)[var.col]
    
    #### Update variable name so that this function can be used across different variables
    colnames(inDF)[var.col] <- "Value"

    ### merge
    inDF <- merge(inDF, corDF.adj, by=c("Ring"))
    
    #### Assign factors
    inDF$Trt <- as.factor(inDF$Trt)
    inDF$Ring <- as.factor(inDF$Ring)
    inDF$Datef <- as.factor(inDF$Date)
    
    #### dataframe with annual totals in g m-2 yr-1
    ###------ Treatment interacting with date, or not
    ###------ Ring random factor
    ###------ Unit g m-2 yr-1
    
    ### add year information
    #inDF$Yr <- year(inDF$Date)
    
    ## Get year list and ring list
    #yr.list <- unique(inDF$Yr)
    #tDF <- summaryBy(Value+Cov2~Trt+Ring+Yr,data=inDF,FUN=sum, keep.names=T)
    #tDF$Yrf <- as.factor(tDF$Yr)
    #
    #### Loop through data, return annual flux in g m-2 yr-1
    #for (i in 1:6) {
    #    for (j in yr.list) {
    #        ### summed of all available data within a year
    #        tDF[tDF$Ring == i & tDF$Yr == j, "Value"] <- inDF$Value[inDF$Ring == i & inDF$Yr == j]
    #    }
    #}
    
    ### Analyse the variable model
    ## model 1: no interaction, year as factor, ring random factor, include pre-treatment effect
    if (with.depth.profile==T) {
        modelt1 <- lmer(Value~Trt + Depth + Datef + covariate + (1|Ring),
                        data=inDF)
    } else if (with.depth.profile == F) {
        modelt1 <- lmer(Value~Trt + covariate + (1|Ring),
                        data=inDF)
    }
    
    
    ## anova
    m1.anova <- Anova(modelt1, test="F")
    
    ## Check ele - amb diff
    summ1 <- summary(glht(modelt1, linfct = mcp(Trt = "Tukey")))
    
    ## average effect size
    eff.size1 <- coef(modelt1)[[1]][1,2]
    
    ## confidence interval 
    eff.conf1 <- confint(modelt1,"Trtele")
    
    ### conditional output
    out <- list(mod = modelt1, 
                anova = m1.anova,
                diff = summ1,
                eff = eff.size1,
                conf = eff.conf1)
    
    
    ### Predict the model with a standard LAI value
    newDF <- inDF
    newDF$covariate <- mean(corDF.adj$covariate)
    newDF$predicted <- predict(out$mod, newdata=newDF)
    
    ### re-adjust the explanatory variables
    newDF$Ring <- as.numeric(as.character(newDF$Ring))
    newDF$Datef <- NULL

    ### plot the covariate predicted against obs data
    if (plot.comparison==T) {
        
        ### with depth
        if (with.depth.profile==T) {
            
            ### calculate total across depth
            tmpDF <- summaryBy(Value+predicted~Trt+Ring+Date, FUN=sum,
                               data=newDF, keep.names=T, na.rm=T)
            
        } else if (with.depth.profile == F) {
            tmpDF <- newDF
        }
        
        ### calculate means, obs
        sumDF1 <- summaryBy(Value~Trt, data=tmpDF,
                            FUN=c(mean, sd), keep.names=T,
                            na.rm=T)
        
        sumDF1$Method <- "Obs"
        
        ### calculate means, predicted
        sumDF2 <- summaryBy(predicted~Trt, data=tmpDF,
                            FUN=c(mean, sd), keep.names=T,
                            na.rm=T)
        
        sumDF2$Method <- "Pred"
        
        names(sumDF2)[names(sumDF2)=="predicted.mean"] <- "Value.mean"
        names(sumDF2)[names(sumDF2)=="predicted.sd"] <- "Value.sd"
        
        ### merge
        plotDF <- rbind(sumDF1, sumDF2)
        
        
        ### plotting script
        p1 <- ggplot(plotDF, aes(x=Trt, Value.mean, group=Method))+
            geom_point(aes(fill=Method), 
                       pch=21, col="black", size=4,
                       position=position_dodge2(width=0.3)) +
            geom_errorbar(aes(x=Trt, ymin=Value.mean-Value.sd,
                              ymax=Value.mean+Value.sd),
                          position=position_dodge2(), width=0.3)+
            scale_fill_manual(name="",
                              values=c("Obs"="black", "Pred"="yellow"))+
            ylab(var.name)
        
        pdf(paste0("plots_tables/covariate/", var.name, ".pdf"), 
            width=8, height=5)
        plot(p1)
        dev.off()
    }
    
    
    
    ### add name for output purpose
    names(newDF)[names(newDF)=="predicted"] <- var.name
    
    
    ### return results
    if (return.outcome == "model") {
        return(out)
    } else if (return.outcome == "predicted") {
        return(newDF)
    }
}
