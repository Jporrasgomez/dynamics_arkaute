



ggdyn2 <- function(data, palette, labels, colorline, position, asterisk, caps){
  

pos_dod_c_dyn <- position_dodge(width = 0.5)

levs  <- levels(data$date_label)
vpos  <- match("04-May-23", levs) + 0.5 
  
  plot <- 
  ggplot(data, aes(x = date_label, y = eff_value,
            group = eff_descriptor, color = eff_descriptor)) +
  
  facet_wrap(~ variable, scales = "free_y", ncol = 1, nrow = 9,
             labeller = labeller(eff_descriptor = as_labeller(labels))
  ) +
  
  scale_x_discrete(drop = FALSE) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +
  
  geom_vline(xintercept = vpos, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  
  geom_linerange(aes(ymin = lower_limit, ymax = upper_limit),
                 position = pos_dod_c_dyn, alpha = 0.6, linewidth = 0.5, 
                 show.legend = FALSE) +
  
  geom_point(position = pos_dod_c_dyn, size = 1.2) +
  
  geom_line(position = pos_dod_c_dyn, linewidth = 0.5) + 
    
    geom_text(aes(
      x = date_label,
      y = ifelse(eff_value < 0,
                 lower_limit - asterisk * scale,
                 upper_limit + asterisk * scale),
      label = ifelse(null_effect == "NO", "*", NA_character_),
      color = eff_descriptor
    ),
    position      = position,
    inherit.aes   = FALSE,
    size          = 5, 
    show.legend   = FALSE
    ) +
    
    scale_y_continuous(
      breaks      = scales::pretty_breaks(n = 3),
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
      axis.text.x        = element_text(angle = 45, hjust = 1, face = "plain", size = 9),
      #axis.text.x        = element_blank(),
      legend.position    = "bottom",
      #axis.ticks.x        = element_blank(), 
      legend.text        = element_text(size = 10, face = "plain")
    )
  
  
}








