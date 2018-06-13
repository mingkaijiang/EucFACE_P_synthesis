make_other_litter_flux <- function(){
  
  litter_raw <- download_leaflitter()  
  
  # glitch fix
  litter_raw$Ring <- as.character(litter_raw$Ring)
  litter_raw$Trap <- as.character(litter_raw$Trap)
  litter_raw$Ring[is.na(litter_raw$Ring)] <- litter_raw$RING[is.na(litter_raw$Ring)]
  litter_raw$TRAP[is.na(litter_raw$Ring)] <- litter_raw$RING[is.na(litter_raw$Ring)]
  
  
  # remove two data points where big branches fall into litter bascket
  line.num <- which.max(litter_raw$Twig)
  litter_raw <- litter_raw[-line.num,]
  
  line.num <- which.max(litter_raw$Twig)
  litter_raw <- litter_raw[-line.num,]
  
  # Conversion factor from g basket-1 to mg m-2
  conv <- 1000 / frass_basket_area
  
  litter <- dplyr::mutate(litter_raw, 
                   Date = as.Date(litter_raw$Date, format = "%d/%m/%Y"),
                   Start_date = Date - days.past,
                   End_date = Date,
                   Twig = as.numeric(Twig) * conv / days.past,
                   Bark = as.numeric(Bark) * conv / days.past,
                   Seed = as.numeric(Seed) * conv / days.past)
  
  litter$other_tot <- litter$Twig + litter$Bark + litter$Seed
  
  # Averages by Ring
  litter_a <- summaryBy(other_tot ~ Date + Ring, FUN=mean, na.rm=TRUE,
                        data=litter, id = ~Start_date + End_date, keep.names=TRUE)
  
  # return other flux in unit of mg m-2 d-1 (not C or P, just litter mass!!!)
  litter_a <- as.data.frame(dplyr::rename(litter_a,
                                   other_flux = other_tot))
  
  return(litter_a)
}

