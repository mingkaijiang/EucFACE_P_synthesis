adjust_p_variables_with_covariate <- function(inDF, 
                                              corDF.adj, 
                                              var.col,
                                              poolORflux,
                                              ignore.date=F,
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

    
    #### Assign factors
    inDF$Trt[inDF$Ring%in%c(2,3,6)] <- "amb"
    inDF$Trt[inDF$Ring%in%c(1,4,5)] <- "ele"
    inDF$Trt <- as.factor(inDF$Trt)
    
    ### add year information
    inDF$Yr <- year(inDF$Date)
    inDF$Yrf <- as.factor(inDF$Yr)
    
    ### get list
    yr.list <- unique(inDF$Yr)

    
    if (ignore.date==F) {
        #### dataframe with annual totals in g m-2 yr-1 or g m-2,
        ###------ Ring random factor
        ###------ Year fixed factor
        
        ### check what variable this is: pool or flux
        if (poolORflux == "pool") {
            
            ### if depth profile is needed
            if (with.depth.profile==T) {
                
                ## Get year list and ring list
                tDF <- summaryBy(Value~Trt+Ring+Yrf+Depth,
                                 data=inDF,FUN=mean, keep.names=T, na.rm=T)
                
                ### merge with covariate
                tDF <- merge(tDF, corDF.adj, by=c("Ring", "Trt"))
                
                ### make ring a factor
                tDF$Ring <- as.factor(tDF$Ring)
                
                ### linear model 
                modelt1 <- lmer(Value~Trt + Depth + Yrf + covariate + (1|Ring),
                                data=tDF)
                
            } else if (with.depth.profile == F) {
                
                ## Get year list and ring list
                tDF <- summaryBy(Value~Trt+Ring+Yrf,
                                 data=inDF,FUN=mean, keep.names=T, na.rm=T)
                
                ### merge with covariate
                tDF <- merge(tDF, corDF.adj, by=c("Ring", "Trt"))
                
                ### make ring a factor
                tDF$Ring <- as.factor(tDF$Ring)
                
                ### linear model 
                modelt1 <- lmer(Value~Trt + Yrf + covariate + (1|Ring),
                                data=tDF)
                
            }  ## end depth profile check
            
            
        } else if (poolORflux == "flux") {
            
            
            ### if depth profile is needed
            if (with.depth.profile==T) {
                
                ### get depth
                depth.list <- unique(inDF$Depth)
                
                ### prepare an empty storage df
                tDF <- data.frame("Ring" = rep(1:6, length(yr.list)*length(depth.list)),
                                  "Trt" = rep(c("ele", "amb", "amb", 
                                                "ele", "ele", "amb"),
                                              length(yr.list)*length(depth.list)),
                                  "Yr" = rep(yr.list, each=6),
                                  "Depth" = rep(depth.list, each=6*length(yr.list)))
                
                # convert production flux from mg m-2 d-1 to g m-2 yr-1
                conv <- 365 / 1000  
                
                ### calculate annual total
                for (i in unique(inDF$Ring)) {
                    for (j in yr.list) {
                        for (k in depth.list) {
                            tDF$Value[tDF$Ring==i&tDF$Yr==j&tDF$Depth==k] <- with(inDF[inDF$Ring ==i&inDF$Yr==j&inDF$Depth==k,],
                                                                                  sum(Value*Days)/sum(Days)) * conv
                            
                        }
                    }
                    
                }
                
                ### make year a factor
                tDF$Yrf <- as.factor(tDF$Yr)
                
                
                ### merge with covariate
                tDF <- merge(tDF, corDF.adj, by=c("Ring", "Trt"))
                
                ### make ring a factor
                tDF$Ring <- as.factor(tDF$Ring)
                
                ### linear model 
                modelt1 <- lmer(Value~Trt + Depth + Yrf + covariate + (1|Ring),
                                data=tDF)
                
            } else if (with.depth.profile == F) {
                
                ### prepare an empty storage df
                tDF <- data.frame("Ring" = rep(1:6, length(yr.list)),
                                  "Trt" = rep(c("ele", "amb", "amb", "ele", "ele", "amb"),
                                              length(yr.list)),
                                  "Yr" = rep(yr.list, each=6))
                
                # convert production flux from mg m-2 d-1 to g m-2 yr-1
                conv <- 365 / 1000  
                
                ### calculate annual total
                for (i in unique(inDF$Ring)) {
                    for (j in yr.list) {
                        tDF$Value[tDF$Ring==i&tDF$Yr==j] <- with(inDF[inDF$Ring ==i&inDF$Yr==j,],
                                                                 sum(Value*Days)/sum(Days)) * conv
                        
                    }
                    
                }
                
                ### make year a factor
                tDF$Yrf <- as.factor(tDF$Yr)
                
                ### merge with covariate
                tDF <- merge(tDF, corDF.adj, by=c("Ring", "Trt"))
                
                ### make ring a factor
                tDF$Ring <- as.factor(tDF$Ring)
                
                ### linear model 
                modelt1 <- lmer(Value~Trt + Yrf + covariate + (1|Ring),
                                data=tDF)
                
            }  ## end depth profile check
        } ### end pool flux check
    } else {
        #### dataframe with annual totals in g m-2 yr-1 or g m-2,
        ###------ Ring random factor
        ###------ Year fixed factor
        
        ### check what variable this is: pool or flux
        if (poolORflux == "pool") {
            
            ### if depth profile is needed
            if (with.depth.profile==T) {
                
                ## Get year list and ring list
                tDF <- summaryBy(Value~Trt+Ring+Depth,
                                 data=inDF,FUN=mean, keep.names=T, na.rm=T)
                
                ### merge with covariate
                tDF <- merge(tDF, corDF.adj, by=c("Ring", "Trt"))
                
                ### make ring a factor
                tDF$Ring <- as.factor(tDF$Ring)
                
                ### linear model 
                modelt1 <- lmer(Value~Trt + Depth + covariate + (1|Ring),
                                data=tDF)
                
            } else if (with.depth.profile == F) {
                
                ## Get year list and ring list
                tDF <- summaryBy(Value~Trt+Ring,
                                 data=inDF,FUN=mean, keep.names=T, na.rm=T)
                
                ### merge with covariate
                tDF <- merge(tDF, corDF.adj, by=c("Ring", "Trt"))
                
                ### make ring a factor
                tDF$Ring <- as.factor(tDF$Ring)
                
                ### linear model 
                modelt1 <- lmer(Value~Trt + covariate + (1|Ring),
                                data=tDF)
                
            }  ## end depth profile check
            
            
        } else if (poolORflux == "flux") {
            
            
            ### if depth profile is needed
            if (with.depth.profile==T) {
                
                ### get depth
                depth.list <- unique(inDF$Depth)
                
                ### prepare an empty storage df
                tDF <- data.frame("Ring" = rep(1:6, length(depth.list)),
                                  "Trt" = rep(c("ele", "amb", "amb", 
                                                "ele", "ele", "amb"),
                                              length(depth.list)),
                                  "Depth" = rep(depth.list, each=6))
                
                # convert production flux from mg m-2 d-1 to g m-2 yr-1
                conv <- 365 / 1000  
                
                ### calculate annual total
                for (i in unique(inDF$Ring)) {
                    for (k in depth.list) {
                        tDF$Value[tDF$Ring==i&tDF$Depth==k] <- with(inDF[inDF$Ring ==i&inDF$Depth==k,],
                                                                              sum(Value*Days)/sum(Days)) * conv
                        
                    }
                }
                
                ### merge with covariate
                tDF <- merge(tDF, corDF.adj, by=c("Ring", "Trt"))
                
                ### make ring a factor
                tDF$Ring <- as.factor(tDF$Ring)
                
                ### linear model 
                modelt1 <- lmer(Value~Trt + Depth + covariate + (1|Ring),
                                data=tDF)
                
            } else if (with.depth.profile == F) {
                
                ### prepare an empty storage df
                tDF <- data.frame("Ring" = rep(1:6),
                                  "Trt" = rep(c("ele", "amb", "amb", "ele", "ele", "amb")))
                
                # convert production flux from mg m-2 d-1 to g m-2 yr-1
                conv <- 365 / 1000  
                
                ### calculate annual total
                for (i in unique(inDF$Ring)) {
                    
                        tDF$Value[tDF$Ring==i&tDF$Yr==j] <- with(inDF[inDF$Ring ==i,],
                                                                 sum(Value*Days)/sum(Days)) * conv
                        
                }
                
                ### merge with covariate
                tDF <- merge(tDF, corDF.adj, by=c("Ring", "Trt"))
                
                ### make ring a factor
                tDF$Ring <- as.factor(tDF$Ring)
                
                ### linear model 
                modelt1 <- lmer(Value~Trt + covariate + (1|Ring),
                                data=tDF)
                
            }  ## end depth profile check
        } ### end pool flux check
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
    newDF <- tDF
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
