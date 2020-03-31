
make_normalized_c_flux_summary_table <- function(inDF) {
  
  ### prepare outDF
  outDF <- inDF
  
  #### prepare a storage DF
  stDF <- data.frame(c(1:6), NA, NA)
  colnames(stDF) <- c("Ring", "Value", "Trt")
  stDF$Trt <- c("ele", "amb", "amb", "ele", "ele", "amb")
  
  
  #### read in covariate data
  ### Read initial basal area data
  f12 <- read.csv("temp_files/EucFACE_dendrometers2011-12_RAW.csv")
  f12$ba <- ((f12$X20.09.2012/2)^2) * pi
  baDF <- summaryBy(ba~Ring, data=f12, FUN=sum, na.rm=T, keep.names=T)
  
  ### return in unit of cm2/m2, which is m2 ha-1
  baDF$ba_ground_area <- baDF$ba / FACE_ring_area
  
  ### Read initial soil P concentration
  spDF <- subset(soil_p_concentration, Date=="2012-06-17")
  
  ### Read initial soil P pool
  sbDF <- subset(soil_p_pool, Date=="2012-06-17")
  
  #### assign covariate
  for (i in 1:6) {
    stDF$Cov1[stDF$Ring==i] <- baDF$ba_ground_area[baDF$Ring==i]
    stDF$Cov2[stDF$Ring==i] <- spDF$PercP[spDF$Ring==i]
    stDF$Cov3[stDF$Ring==i] <- sbDF$soil_p_g_m2[sbDF$Ring==i]
  }
  
  #### Assign factors
  stDF$Trt <- as.factor(stDF$Trt)
  
  #### prepare loop
  terms <- unique(inDF$terms)
  
  #### loop
  for (i in 1:length(terms)) {
    tDF <- subset(inDF, terms==terms[i])
    stDF$Value <- as.numeric(tDF[1,2:7])
    
    ## linear model with a covariate
    modelt1 <- lm(Value~Trt + Cov3,data=stDF)
    
    ## Check ele - amb diff
    summ1 <- summary(glht(modelt1, linfct = mcp(Trt = "Tukey")))
    
    ## average effect size
    eff.size1 <- coef(modelt1)[[2]]
    
    ## confidence interval 
    eff.conf1 <- confint(modelt1,"Trtele")
    
    ## summary list
    out <- list(mod = modelt1, 
                diff = summ1,
                eff = eff.size1,
                conf = eff.conf1)
    
    ## predict responses with a standard pre-existing value
    newDF <- stDF
    newDF$Cov3 <- mean(stDF$Cov3)
    newDF$predicted <- predict(out$mod, newdata=newDF)
    
    outDF[outDF$terms==terms[i], 2:7] <- newDF$predicted
    
  } 
  
  
  ### calculate treatment averages
  outDF$aCO2 <- round(rowMeans(subset(outDF, select=c(R2, R3, R6)), na.rm=T), 5)
  outDF$eCO2 <- round(rowMeans(subset(outDF, select=c(R1, R4, R5)), na.rm=T), 5)
  
  outDF$aCO2_sd <- rowSds(as.matrix(subset(outDF, select=c(R2, R3, R6))), na.rm=T)
  outDF$eCO2_sd <- rowSds(as.matrix(subset(outDF, select=c(R1, R4, R5))), na.rm=T)
  
  ###### Diff (eCO2 - aCO2)
  outDF$diff <- round(outDF$eCO2 - outDF$aCO2, 4)
  
  ###### percent differences (eCO2 - aCO2) / aCO2 * 100
  outDF$percent_diff <- round((outDF$eCO2 - outDF$aCO2) / (outDF$aCO2) * 100, 2)
  
  ### outDF
  outDF$notes <- "normalized with soil P pool"
  
  outDF$timepoint <- "annualized"
  
  ### save output
  write.csv(outDF, "plots_tables/summary_table_c_flux_normalized.csv", 
            row.names=F)
  
  ##### output tables
  return(outDF)
  
}

