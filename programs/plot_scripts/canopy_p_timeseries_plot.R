
plotDF1 <- canopy_p_concentration 
plotDF2 <- leaflitter_p_concentration 


plotDF1$class <- "canopy"
plotDF2$class <- "litter"

plotDF <- rbind(plotDF1, plotDF2)
plotDF$Ring <- as.character(plotDF$Ring)
plotDF$class <- as.character(plotDF$class)

pdf("plots_tables/canopy_timeseries_P_conc_plot.pdf")
p1 <- ggplot(plotDF,
             aes(Date, PercP)) + 
    geom_point(aes(color=Ring, shape=class, size=2)) +
    xlab("Date") + ylab("Canopy P conc (%)")+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=10), 
          axis.text.x = element_text(size=10),
          axis.text.y=element_text(size=10),
          axis.title.y=element_text(size=10),
          legend.text=element_text(size=10),
          legend.title=element_text(size=12),
          panel.grid.major=element_blank(),
          legend.position="bottom") +
    scale_color_manual(name="Ring", values = c("1" = "red", "2" = "blue",
                                              "3" = "cyan", "4" = "pink",
                                              "5" = "orange", "6" = "darkblue"),
                      labels=c("R1", "R2", "R3", "R4", "R5", "R6"))+
    scale_shape_manual(name="Class", values = c(16,17),
                       labels=c("canopy", "litter"))
    

plot(p1)
dev.off()
