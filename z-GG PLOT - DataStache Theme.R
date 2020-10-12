theme_DataStache <- function(base_size = 13, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      
      # Base elements which are not used directly but inherited by others
      line =              element_line(colour = '#DADADA', size = 0.75, 
                                       linetype = 1, lineend = "butt"),
      rect =              element_rect(fill = "#F0F0F0", colour = "#F0F0F0", 
                                       size = 0.5, linetype = 1),
      text =              element_text(family = base_family, face = "plain",
                                       colour = "#656565", size = 10,
                                       angle = 0, 
                                       lineheight = 0.9),
      plot.margin =       unit(c(.25, .6, .25, .6), "cm"),
      
      # Modified inheritance structure of text element
      plot.title =        element_text(size = rel(1), family = '' , 
                                       face = 'bold', colour = '#3B3B3B',
                                       hjust = 0,
                                       margin = margin(b=5, unit="pt")),
      plot.subtitle =     element_text(size = rel(.7), family = '' , 
                                       hjust = 0, 
                                       vjust = 1.5, colour = '#656565'),
      plot.caption =      element_text(size = rel(.5), 
                                      hjust = 1,
                                      vjust = -1.5),
      axis.title.x =      element_blank(),
      axis.title.y =      element_blank(),
      axis.text =         element_text(family = "mono",
                                       size = rel(.5),
                                       face = "bold"),
      legend.text =       element_text(size = rel(.7)),
      
      
      # Modified inheritance structure of line element
      axis.ticks =        element_blank(), 
      axis.line.x =       element_line(size = 1.5),
      panel.grid.major =  element_line(color = "grey 80", size = (.2)),
      panel.grid.minor =  element_blank(),
      
      
      # Modified inheritance structure of rect element
      plot.background =   element_rect(),
      panel.background =  element_rect(),
      legend.key =        element_rect(colour = '#DADADA'),
      
      # Modifiying legend.position
      legend.position = 'none',
      
      complete = TRUE
    )
}

save(theme_DataStache, file = "rda/theme_DataStache.rda")
