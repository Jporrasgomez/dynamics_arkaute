

ggdyn <- function(data, palette, labels, colorline, position){
  
  # extraigo el width del objeto position_dodge2
  dodge_width <- position$width
  
  plot <- 
    ggplot(data, aes(
      x     = date,
      y     = eff_value,
      group = eff_descriptor,
      color = eff_descriptor
    )) + 
    
    facet_grid(variable ~ year, scales = "free",
               labeller = labeller(eff_descriptor = as_labeller(labels))
    ) + 
    
    geom_hline(yintercept = 0,
               linetype   = "dashed",
               color      = colorline,
               linewidth  = 0.5) +
    
    geom_vline(xintercept = as.Date("2023-05-11"),
               linetype   = "dashed",
               color      = "gray40",
               linewidth  = 0.5) +
    
    geom_errorbar(aes(ymin = lower_limit, ymax = upper_limit),
                  position  = position,
                  width     = dodge_width,
                  alpha     = 0.5,
                  linewidth = 0.5) +
    
    geom_point(position = position,
               size     = 1.1) +
    
    geom_line(position = position,
              linewidth = 0.5) +
    
    geom_text(aes(
      x = date,
      y = ifelse(eff_value < 0,
                     lower_limit - 8 * scale,
                     upper_limit + 8 * scale),
      label = ifelse(null_effect == "NO", "*", NA_character_),
      color = eff_descriptor
    ),
    position      = position,
    inherit.aes   = FALSE,
    size          = 5
    ) +
    
    scale_x_date(
      date_breaks = "2 months",
      date_labels = "%Y-%m"
    ) +
    
    scale_y_continuous(
      breaks      = scales::pretty_breaks(n = 2),
      minor_breaks = NULL
    ) +
    
    scale_color_manual(values = palette) +
    
    labs(y = NULL, x = NULL) +
    
    gg_RR_theme +
    theme(
      strip.text.y     = element_blank(),
      strip.background = element_blank(),
      strip.text.x     = element_blank()
    )
  
  
}
