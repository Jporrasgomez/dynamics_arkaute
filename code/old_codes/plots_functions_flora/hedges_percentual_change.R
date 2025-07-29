




library(effectsize)

hedges_treatment <- effect %>% 
  filter(treatment != "c") %>% 
  left_join(effect_c, by = c("date", "sampling")) %>% 
  mutate(
    
    # Desviación estándar combinada (Pooled SD)
    pooled_sd = sqrt((((n - 1) * (.data[[sd_variable]]^2)) + 
                        ((n - 1) * (.data[[sd_variable_c]]^2))) / ((n+n) - 2)), 
    
    # Cálculo de Hedges' g sin corrección
    hedges_g_uncorrected = (.data[[mean_variable]] - .data[[mean_variable_c]]) / pooled_sd,
    
    # Factor de corrección J para muestras pequeñas
    J = 1 - (3 / (4 * ((n+n) - 2) - 1)),  
    
    # Hedges' g corregido
    hedges_g = J * hedges_g_uncorrected,
    
    # Error estándar de Hedges' g
    se_hedges_g = J * sqrt(((n+n) / (n * n)) + (hedges_g^2 / (2 * (n+n))))
  )

gg_hedges <- 
  ggplot(hedges_treatment, aes(x = date, y = hedges_g)) + 
  facet_wrap(~ treatment, labeller = labeller(treatment = labels3)) +
  geom_errorbar(aes(ymin = hedges_g - pooled_sd,
                    ymax = hedges_g + pooled_sd,
                    color = treatment), alpha = 0.5) +
  geom_point(aes(color = treatment)) + 
  geom_line(aes(color = treatment)) +
  scale_color_manual(values = palette) +
  geom_hline( yintercept= 0, linetype = "dashed", color = "gray40") +
  labs(x = NULL, y = paste0("Hedges'g effect for", ytitle)) +
  theme(legend.position = "none")

gg_hedges <<- gg_hedges

perchange_treatment <- effect %>% 
  filter(treatment != "c") %>% 
  left_join(effect_c, by = c("date", "sampling")) %>% 
  mutate(
    # Cálculo del cambio porcentual
    percent_change = ((.data[[mean_variable]] - .data[[mean_variable_c]]) / .data[[mean_variable_c]]) * 100,
    
    # Error estándar del cambio porcentual
    se_percent_change = (100 / .data[[mean_variable_c]]) * sqrt(
      (.data[[sd_variable]]^2 / 4) + 
        (.data[[mean_variable]]^2 / .data[[mean_variable_c]]^2) * (.data[[sd_variable_c]]^2 / 4)
    )
  )

gg_perc <- 
  ggplot(perchange_treatment, aes(x = date, y = percent_change)) + 
  facet_wrap(~ treatment, labeller = labeller(treatment = labels3)) +
  geom_errorbar(aes(ymin = percent_change - se_percent_change,
                    ymax = percent_change + se_percent_change,
                    color = treatment), alpha = 0.5) +
  geom_point(aes(color = treatment)) + 
  geom_line(aes(color = treatment)) +
  scale_color_manual(values = palette) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(x = NULL, y = paste0("Percenctual change (%) of", ytitle)) +
  theme(legend.position = "none")

gg_perc <<- gg_perc
