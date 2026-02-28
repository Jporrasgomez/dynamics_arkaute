

ggdyn <- function(data, palette, labels, colorline, position, asterisk, caps){
  
  # extraigo el width del objeto position_dodge2
  #dodge_width <- position$width
  
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
    
    geom_vline(
      data = filter(data, year == 2023) %>%
        distinct(year) %>%
        mutate(x = as.Date("2023-05-11")),
      aes(xintercept = x),
      linetype  = "dashed",
      color     = "gray40",
      linewidth = 0.5) +
    
    #geom_vline(xintercept = -Inf, colour = "grey40", linewidth = 0.4) +
    
    
    #geom_errorbar(aes(ymin = lower_limit, ymax = upper_limit),
    #              position  = position,
    #              width     = caps,
    #              alpha     = 0.5,
    #              linewidth = 0.5) +
    
    geom_linerange(aes(ymin = lower_limit, ymax = upper_limit),
                   position = pos_dod_c_dyn, alpha = 0.6, linewidth = 0.5,
                   show.legend = FALSE) +
    
    geom_point(position = position,
               size     = 1.2) +
    
    geom_line(position = position,
              linewidth = 0.5) +
    
    geom_text(aes(
      x = date,
      y = ifelse(eff_value < 0,
                 lower_limit - asterisk * scale,
                 upper_limit + asterisk * scale),
      label = ifelse(null_effect == "NO", "*", NA_character_),
      color = eff_descriptor
    ),
    position      = position,
    inherit.aes   = FALSE,
    size          = 5, 
    show.legend = FALSE
    
    ) +
    
    scale_x_date(
      date_breaks = "2 months",
      date_labels = "%Y-%m"
    ) +
    
    scale_y_continuous(
      breaks      = scales::pretty_breaks(n = 2),
      minor_breaks = NULL,
      expand = expansion(mult = c(0.1, 0.1))
    ) +
    
    
    scale_color_manual(values = palette, labels = labels) +
    
    labs(y = NULL, x = NULL, color = NULL) +
    
    gg_RR_theme +
    theme(
      text               = element_text(size = 10), 
      strip.text.y       = element_blank(),
      strip.background   = element_blank(),
      strip.text.x       = element_blank(),
      strip.text         = element_text(face = "bold", size = 10),
      axis.text.y        = element_text(angle = 90, hjust = 0.5, face = "plain", size = 8),
      axis.text.x        = element_text(hjust = 0.5, face = "plain", size = 9),
      #axis.text.x        = element_blank(),
      legend.position    = "bottom",
      #axis.ticks.x        = element_blank(), 
      legend.text        = element_text(size = 10, face = "plain")
    )
  
  
}


