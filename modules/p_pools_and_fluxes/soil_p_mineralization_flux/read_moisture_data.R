# Read soil moisture content in topsoil
read_swc <- function() {
    swc <- read.csv("temp_files/facesoilwater_ring.csv")
    swc$Date <- as.Date(swc$Date)
    swc$Ring <-  as.numeric(substr(swc$Ring,2,3))
    #standardise treatment column
    swc$Treatment <- "A"
    swc$Treatment[swc$treatment == "elevated"] <- "E"
    swc <- subset(swc, select = -c(treatment))
    return(swc)
}

# Read soil moisture content at depth
# VWC - volumetric water content (m3 m-3)
# GWC - gravimetric water content (g g-1)
# swc - total water content (by layer) in mm - sum to get totals
read_npswc <- function() {
    np <- read.csv("temp_files/npsoilwater.csv")
    np$Date <- as.Date(np$Date)
    np$Ring <-  as.numeric(substr(np$Ring,2,3))
    
    # Multiply fraction SWC by volume to get mm of water stored
    # Already done
    # Depth <- sort(unique(np$Depth))
    # Volume <- c(25, 25, 25, 25, 25, 25, 50, 50, 50, 50, 50, 50)
    # depthvol <- data.frame(Depth,Volume)
    # np <- merge(np,depthvol,by="Depth")
    # np$swc <- with(np,VWC*Volume/10) # values in mm of soil water stored
    return(np)
}

# calculate total soil moisture in each ring to given depth
# return either as ring average (byring) or for each probe individually
summarise_np <- function(npsoilwater, depth, byring = TRUE) {
    
    # Obtain total soil moisture for each Probe
    probeswc <- summaryBy(swc~Date+Probe.ID+Ring+Location,
                          data=subset(npsoilwater,Depth < (depth+5)),FUN=sum,na.rm=T)
    
    # Average probes for each layer
    layerswc <- summaryBy(swc~Date+Depth+Ring+Location,
                          data=npsoilwater,FUN=mean,keep.names=T,na.rm=T)
    # Obtain total soil moisture for each Ring
    totswc <- summaryBy(swc~Date+Ring+Location,
                        data=subset(layerswc, Depth < (depth+5)),FUN=sum,na.rm=T)
    
    # choose which to return
    if (byring) { 
        return(totswc)
    } else {
        return(probeswc)
    }
}


# read air temperature data
read_airt <- function() {
    airt <- read.csv("temp_files/airt.csv")
    airt$Date <- as.Date(airt$Date)
    return(airt)
}

# read rainfall data
read_rain <- function() {
    rain <- read.csv("temp_files/faceraindaily.csv")
    rain$Date <- as.Date(rain$Date)
    return(rain)
}

