adjust_covariate_for_statistical_input <- function (corDF,
                                                    covariate.name) {
    
    ### revise colnames
    names(corDF)[names(corDF)==covariate.name] <- "covariate"
    
    ### outDF
    outDF <- corDF[,c("Ring", "Trt", "covariate")]
    
    ### return
    return(outDF)
}