




ggagg <- function(data, palette, labels, colorline, position, caps, limitvar, labelvar){
  
  
  #dodge_width <- position$width
  
  plot <- 
    ggplot(data, aes(
      y     = variable,
      x     = eff_value,
      color = eff_descriptor,
      group = eff_descriptor        
    )) +
    
    
    geom_vline(xintercept = 0,
               linetype   = "dashed",
               color      = colorline,
               linewidth  = 0.5) +
    
    
    #geom_errorbar(aes(xmin = lower_limit,
    #                  xmax = upper_limit),
    #              position  = position,
    #              width     = caps,
    #              linewidth = 0.5,
    #              alpha     = 0.5) +
    
    
    geom_linerange(
      aes(y = variable, xmin = lower_limit, xmax = upper_limit),
      position   = position,
      linewidth  = 1,
      alpha      = 1,
      orientation = "y"   # important for horizontal ranges
    ) +
    
    
    geom_point(position = position,
               size     = 2.5) +
    
    geom_text(aes(
      y = variable,
      x = ifelse(eff_value < 0, lower_limit - scale, upper_limit + scale),
      label = ifelse(null_effect == "NO", "*", NA_character_), 
      color = eff_descriptor
    ),
    position    = position,
    inherit.aes = FALSE,
    size        = 5, 
    show.legend = FALSE
    ) +
    
    scale_color_manual(values = palette,
                       labels = labels) +
    
    scale_y_discrete(
      limits = rev(limitvar),
      labels = rev(labelvar)
    ) +
    
    scale_x_continuous(
      breaks = scales::pretty_breaks(n = 3)
    ) +
    
    labs(x = NULL, y = NULL, color = NULL) +
    
    gg_RR_theme +
    theme(
      #text               = element_text(size = 10), 
      strip.text.y       = element_blank(),
      strip.background   = element_blank(),
      strip.text.x       = element_blank(),
      strip.text         = element_text(face = "bold", size = 10),
      #axis.text.y        = element_text(angle = 90, hjust = 0.5, face = "plain", size = 8),
      axis.text.x        = element_text(hjust = 0.5, face = "plain", size = 9),
      #axis.text.x        = element_blank(),
      legend.position    = "bottom",
      #axis.ticks.x        = element_blank(), 
      legend.text        = element_text(size = 10, face = "plain")
    )
  
}


