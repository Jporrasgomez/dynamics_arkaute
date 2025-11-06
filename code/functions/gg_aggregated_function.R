




ggagg <- function(data, palette, labels, colorline, position, asterisk, caps, limitvar, labelvar){
  
  
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
      linewidth  = 0.5,
      alpha      = 1,
      orientation = "y"   # important for horizontal ranges
    ) +
    
    
    geom_point(position = position,
               size     = 2) +
    
    geom_text(aes(
      y = variable,
      x = ifelse(eff_value < 0, lower_limit - scale * asterisk, upper_limit + scale * asterisk),
      label = ifelse(null_effect == "NO", "*", NA_character_), 
      color = eff_descriptor
    ),
    position    = position,
    inherit.aes = FALSE,
    size        = 5
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
    
    labs(x = NULL, y = NULL) +
    
    gg_RR_theme
  
}

