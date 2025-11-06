



{ggagg <- function(data, palette, labels, colorline, position, asterisk, caps, limitvar, labelvar){
  
  
  #dodge_width <- position$width
  
  plot <- 
    ggplot(data, aes(
      x     = variable,
      y     = eff_value,
      color = eff_descriptor,
      group = eff_descriptor        
    )) +
    
    facet_wrap(~ variable, ncol = 1, scales = "free") +
    #facet_grid(rows = vars(variable), scales = "free", switch = "y") +
    
    
    geom_hline(yintercept = 0,
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
      aes(x = variable, ymin = lower_limit, ymax = upper_limit),
      position   = position,
      linewidth  = 1,
      alpha      = 1,
      orientation = "x"   # important for horizontal ranges
    ) +
    
    
    geom_point(position = position,
               size     = 2.5) +
    
    geom_text(aes(
      x = variable,
      y = ifelse(eff_value < 0, lower_limit - 0.06, upper_limit + 0.06),
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
    
    #scale_y_discrete(
    #  limits = rev(limitvar),
    #  labels = rev(labelvar)
    #) +
    
    scale_y_continuous(
      breaks = scales::pretty_breaks(n = 2)
      
    ) +
    
    labs(x = NULL, y = NULL, color = "Treatment comparison") +
    
  gg_RR_theme +
    #theme(
    #  axis.title.x = element_blank(),
    #  axis.text.x  = element_blank(),
    #  axis.ticks.x = element_blank()
    #)
    
    theme(
      strip.background   = element_blank(),
      strip.placement    = "outside",
      strip.text         = element_text(face = "bold", size = 10),  # por si acaso
      strip.text.y.left  = element_text(face = "plain"),  # << aquí
      axis.text.y        = element_text(angle = 90, hjust = 0.5, face = "plain"),
      axis.title.x       = element_blank(),
      axis.text.x        = element_blank(),
      axis.ticks.x       = element_blank(),
      legend.position    = "bottom"
    )
  
}


# Variables

limits_variables <- c("richness",
                      "abundance",
                      "Y_zipf",
                      "SLA", 
                      "LDMC", 
                      "leafN",
                      #"biomass",
                      #"biomass012",
                      "biomass_lm_plot"
)

labels_variables <- c("richness" = "Richness",            # 1
                      "abundance" = "Cover",              # 2
                      "Y_zipf" = "Evenness",              # 3
                      "SLA" = "SLA",                      # 4
                      "LDMC" = "LDMC",                    # 5
                      "leafN"= "Leaf-N",                  # 6
                      #"biomass" = "Biomass",
                      #"biomass012" = "Biomass",
                      "biomass_lm_plot" = "Biomass")      # 7


# Choosing the range of variables to be displayed (i to j)
i = 1
j = 7



pos_dod_c_agg <- position_dodge2(width = 0.3, preserve = "single")



lvls <- limits_variables[i:j]
labs <- unname(labels_variables[lvls])  # usa los nombres del vector con nombre

gg_eff_agg_c <- agg %>% 
  filter(eff_descriptor %in% c("p_vs_c", "w_vs_c", "wp_vs_c"),
         variable %in% lvls) %>% 
  mutate(
    eff_descriptor = factor(eff_descriptor, levels = c("p_vs_c","w_vs_c","wp_vs_c")),
    variable       = factor(variable, levels = lvls, labels = labs)  # orden + etiquetas
  ) %>% 
  ggagg(
    palette_RR_CB,
    labels_RR2,
    "grey50",
    position   = pos_dod_c_agg,
    asterisk   = 50,
    caps       = pos_dod_c_agg$width,
    limitvar   = lvls,
    labelvar   = labels_variables[lvls]
  )



print(gg_eff_agg_c)

}





