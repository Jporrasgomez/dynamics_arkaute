





ggagg_ppt <- function(data, palette, labels, colorline, position, caps, limitvar, labelvar){
  
  
  #dodge_width <- position$width
  
  plot <- 
    ggplot(data, aes(
      x     = variable,
      y     = eff_value,
      color = eff_descriptor,
      group = eff_descriptor        
    )) +
    
    
    geom_hline(yintercept = 0,
               linetype   = "dashed",
               color      = colorline,
               linewidth  = 1) +
    
    
    geom_errorbar(aes(ymin = lower_limit,
                      ymax = upper_limit),
                  position  = position,
                  width     = caps,
                  linewidth = 1) +
    
    
    geom_point(position = position,
               size     = 2) +
    
    geom_text(aes(
      x = variable,
      y = ifelse(eff_value < 0, lower_limit - 0.1, upper_limit + 0.1),
      label = ifelse(null_effect == "NO", "*", NA_character_), 
      color = eff_descriptor
    ),
    position    = position,
    inherit.aes = FALSE,
    size        = 7
    ) +
    
    scale_color_manual(values = palette,
                       labels = labels) +
    
    scale_x_discrete(
      limits = limitvar,
      labels = labelvar
    ) +
    
    scale_y_continuous(
      breaks = scales::pretty_breaks(n = 5)
    ) +
    
    labs(x = NULL, y = NULL) +
    
    gg_RR_theme
  
}
