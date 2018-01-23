#- Make the understorey P pool
make_understorey_p_pool <- function(){
    ### download the data 
    # currently there is a data format issue, so read in data from local directory
    # download_understorey_p_data()
    
    ### read in the file
    df <- read.csv("download/FACE_P0019_RA_leafP-understory_20130509-20151030_L1.csv")
    
    ### Assign date format
    df$Date <- as.Date(df$Date, "%d-%b-%y")
    
    ### Plot species average comparisons
    pdf("plots_tables/Understorey_species_percent_p.pdf")
    require(ggplot2)
    p <- ggplot(df, aes(Species, PercP)) +   
        geom_boxplot() +
        xlab("Species") + ylab("Leaf P concentration (%)") + 
        ggtitle("Understory species effect")
    plot(p)
    dev.off()
    
    ### ring and date specific data
    outDF <- summaryBy(PercP~Ring+Date,
                             data=df,FUN=mean,keep.names=T,na.rm=T)
    # currently the output is percent P, not P mass pool!!!
    
    return(outDF)
    
}
