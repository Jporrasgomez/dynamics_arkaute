



ggdyn2 <- function(data, palette, labels, colorline, position, asterisk, caps){
  
  # extraigo el width del objeto position_dodge2
  #dodge_width <- position$width
  
  plot <- 
    ggplot(data, aes(
      x     = sampling,
      y     = eff_value,
      group = eff_descriptor,
      color = eff_descriptor
    )) + 
    
    facet_wrap(~variable, scales = "free_y", ncol = 1, nrow = 7,
               labeller = labeller(eff_descriptor = as_labeller(labels))
    ) + 
    
    geom_hline(yintercept = 0,
               linetype   = "dashed",
               color      = colorline,
               linewidth  = 0.5) +
    
    geom_vline(xintercept = 0.5,
               linetype  = "dashed",
               color     = "gray40",
               linewidth = 0.5) +
    
    
    #geom_errorbar(aes(ymin = lower_limit, ymax = upper_limit),
    #              position  = position,
    #              width     = caps,
    #              alpha     = 0.5,
    #              linewidth = 0.5) +
    
    geom_linerange(aes(ymin = lower_limit, ymax = upper_limit),
                   position = pos_dod_c_dyn, alpha = 0.6, linewidth = 0.5) +
    
    geom_point(position = position,
               size     = 1.2) +
    
    geom_line(position = position,
              linewidth = 0.5) +
    
    geom_text(aes(
      x = sampling,
      y = ifelse(eff_value < 0,
                 lower_limit - asterisk * scale,
                 upper_limit + asterisk * scale),
      label = ifelse(null_effect == "NO", "*", NA_character_),
      color = eff_descriptor
    ),
    position      = position,
    inherit.aes   = FALSE,
    size          = 5
    ) +
    
    #scale_y_continuous(
    #  breaks      = scales::pretty_breaks(n = 2),
    #  minor_breaks = NULL,
    #  expand = expansion(mult = c(0.1, 0.1))
    #) +
    
    
    scale_color_manual(values = palette) +
    
    labs(y = NULL, x = NULL) +
    
    gg_RR_theme +
    theme(
      strip.text.y       = element_blank(),
      strip.background   = element_blank(),
      strip.text.x       = element_blank(),
      strip.text         = element_text(face = "bold", size = 10),
      axis.text.y        = element_text(angle = 90, hjust = 0.5, face = "plain", size = 8),
      axis.text.x        = element_blank(),
      legend.position    = "none",
      #axis.ticks.x        = element_blank()
    )
  
  
}



gg_eff_dynamics_c <- dyn %>% 
  filter(eff_descriptor %in% c("w_vs_c", "p_vs_c", "wp_vs_c")) %>% 
  filter(variable %in% limits_variables[i:j]) %>%  
  mutate(
    variable = factor(variable, 
                      levels = limits_variables[i:j], 
                      labels = labels_variables[i:j])) %>% 
  
  ggdyn2(palette_RR_CB,
        labels_RR2, 
        "grey50",
        position = pos_dod_c_dyn,
        asterisk = 8, 
        caps = pos_dod_c_dyn$width) 


print(gg_eff_dynamics_c)






dyn %>% 
  filter(eff_descriptor %in% c("w_vs_c")) %>% 
  filter(variable == "richness") %>%  
  
  ggplot(aes(
    x     = as.integer(sampling),
    y     = eff_value,
    group = eff_descriptor,
    color = eff_descriptor
  )) + 
  
  geom_point() +
  geom_line() + 
  geom_vline (xintercept = 0.5, lintype = 0.5)
  