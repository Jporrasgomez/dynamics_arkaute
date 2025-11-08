


ggagg2 <- function(data, palette, labels, colorline,
                   asterisk, limitvar, labelvar){
  
  # -nudge, 0, +nudge para p_vs_c, w_vs_c, wp_vs_c (en ese orden)
  data <- data %>% 
   mutate(
      eff_descriptor = factor(eff_descriptor, levels = c("p_vs_c","w_vs_c","wp_vs_c")),
      x_jit = (as.integer(eff_descriptor) - 2) 
    )
  
  ggplot(data, aes(
    x = x_jit,                 # centrado en 0 + pequeño desplazamiento
    y = eff_value,
    color = eff_descriptor
  )) +
    facet_grid(rows = vars(variable), scales = "free_y", switch = "y") +
    
    geom_hline(yintercept = 0, linetype = "dashed",
               color = colorline, linewidth = 0.5) +
    
    
    geom_linerange(aes(ymin = lower_limit, ymax = upper_limit),
                   linewidth = 1, alpha = 1) +
    
    geom_point(size = 2.5) +
    
    geom_text(aes(
      y = ifelse(eff_value < 0, lower_limit - 0.06, upper_limit + 0.06),
      label = ifelse(null_effect == "NO", "*", NA_character_)
    ),
    show.legend = FALSE, size = 5) +
    
    scale_color_manual(values = palette, labels = labels) +
    
   # Chat gpt help
    scale_x_continuous(limits = c(-1.8 , 1.8 ), expand = expansion(mult = 0.1)) +
    
    scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
    
    labs(x = NULL, y = NULL, color = "Treatment comparison") +
    
    gg_RR_theme +
    theme(
      strip.background   = element_blank(),
      strip.placement    = "outside",
      strip.text         = element_text(face = "bold", size = 10),
      strip.text.y.left  = element_text(face = "bold"),
      axis.text.y        = element_text(angle = 90, hjust = 0.5, face = "plain", size = 8),
      axis.text.x        = element_blank(),
      axis.ticks.x       = element_blank(),
      legend.position    = "none"
    )
}




