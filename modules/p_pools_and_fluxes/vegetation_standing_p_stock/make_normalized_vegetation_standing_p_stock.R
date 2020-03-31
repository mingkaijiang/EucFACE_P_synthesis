make_normalized_vegetation_standing_p_stock <- function(inDF) {
    
    ### ignores bark and twigs
    subDF <- inDF[inDF$terms%in%c("Wood P Pool", "Canopy P Pool", "Fine Root P Pool",
                                  "Coarse Root P Pool", "Understorey P Pool"),1:7]
    
    ### outDF
    outDF <- data.frame(c(1:6), NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(outDF) <- c("Ring", "leaf", "wood", "fineroot", "coarseroot", "understorey",
                         "total", "oa", "belowground", "Trt")
    
    outDF$leaf <- as.numeric(subDF[subDF$terms=="Canopy P Pool", 2:7])
    outDF$wood <- as.numeric(subDF[subDF$terms=="Wood P Pool", 2:7])
    outDF$fineroot <- as.numeric(subDF[subDF$terms=="Fine Root P Pool", 2:7])
    outDF$coarseroot <- as.numeric(subDF[subDF$terms=="Coarse Root P Pool", 2:7])
    outDF$understorey <- as.numeric(subDF[subDF$terms=="Understorey P Pool", 2:7])
    
 
    ### Calculate total
    outDF$total <- with(outDF, (leaf+wood+fineroot+coarseroot+understorey))
    
    ### calculate oa
    outDF$oa <- with(outDF, (leaf+wood))
    
    ### calculate belowground
    outDF$belowground <- with(outDF, (fineroot+coarseroot))
    
    
    ### assign aCO2 and eCO2
    outDF$Trt <- c("eCO2", "aCO2", "aCO2", "eCO2", "eCO2", "aCO2")
    
    return(outDF)
}