


effect_size_wp_vs_treatment <- function(data, variable){
  

  
  mean_variable <- paste0("mean_", variable)
  sd_variable <- paste0("sd_", variable)
  
  mean_variable_w <- paste0("mean_", variable, "_w")
  sd_variable_w <- paste0("sd_", variable, "_w")
  
  mean_variable_p <- paste0("mean_", variable, "_p")
  sd_variable_p <- paste0("sd_", variable, "_p")
  
  
  effect_wp <- data %>% 
    filter(treatment == "wp") %>% 
    select(date, sampling, treatment, all_of(mean_variable), all_of(sd_variable)) %>% 
    distinct()
  
  effect_w <- data %>% 
    filter(treatment == "w") %>% 
    select(date, sampling, all_of(mean_variable), all_of(sd_variable)) %>% 
    rename(!!mean_variable_w := !!sym(mean_variable),
           !!sd_variable_w := !!sym(sd_variable)) %>% 
    mutate(RR_descriptor = "wp_vs_w") %>% 
    distinct()
  
  effect_p <- data %>% 
    filter(treatment == "p") %>% 
    select(date, sampling, all_of(mean_variable), all_of(sd_variable)) %>% 
    rename(!!mean_variable_p := !!sym(mean_variable),
           !!sd_variable_p := !!sym(sd_variable)) %>% 
    mutate(RR_descriptor = "wp_vs_p") %>% 
    distinct()
  
  
  
  ytitle_dict <- list(
    "richness"   = "Richness",
    "abundance"  = "Community cover",
    "sigma_log"  = "Sigma (Coefficient 2 in Log model for RADs)",
    "mu_log"     = "Mu (Coefficient 1 in Log model for RADs)",
    "Y_zipf"     = "Gamma (Coefficient in Zipf model for RADs)",
    "biomass"    = "Community biomass",
    "NMDS1" = "NMDS1",
    "NMDS2" = "NMDS2",
    "NMDS3" = "NMDS3"
  )
  
  ytitle <- ytitle_dict[[variable]]
  
  if (is.null(ytitle)) {
    stop("Variable must be one of the following: ", paste(names(ytitle_dict), collapse = ", "))
  }
  
  
  n = 4
  
  
  ### Perturbation / Global Change
  
  RR_wp_vs_w <- effect_wp %>% 
    left_join(effect_w, by = c("date", "sampling")) %>% 
  
    mutate(
      # Cálculo del Log Response Ratio (RR)
      RR = log(.data[[mean_variable]] / .data[[mean_variable_w]]),
      
      # Cálculo de la varianza de RR_wp_vs_w
      var_RR = (.data[[sd_variable]]^2) / (n * .data[[mean_variable]]^2) + 
        (.data[[sd_variable_w]]^2) / (n * .data[[mean_variable_w]]^2),
      
      se_RR = sqrt(var_RR)  # Error estándar de RR_wp_vs_w
      
    ) %>% 
    mutate(
      # Cálculo de delta_RR_wp_vs_w (ajuste de sesgo)
      delta_RR= RR + 0.5 * (
        (.data[[sd_variable]]^2) / (n * .data[[mean_variable]]^2) - 
          (.data[[sd_variable_w]]^2) / (n * .data[[mean_variable_w]]^2)
      ),
      
      # Varianza de delta_RR_wp_vs_w
      var_delta_RR = var_RR + 0.5 * (
        (.data[[sd_variable]]^4) / (n^2 * .data[[mean_variable]]^4) + 
          (.data[[sd_variable_w]]^4) / (n^2 * .data[[mean_variable_w]]^4)
      ),
      se_delta_RR = sqrt(var_delta_RR),  # Error estándar de delta_RR_wp_vs_w
      
      # Cálculo de sigma_RR_wp_vs_w
      sigma_RR = 0.5 * log(
        (.data[[mean_variable]]^2 + (.data[[sd_variable]]^2) / n) / 
          (.data[[mean_variable_w]]^2 + (.data[[sd_variable_w]]^2) / n)
      ),
      
      # Varianza de sigma_RR_wp_vs_w
      var_sigma_RR = 2.0 * var_RR - 
        log(1.0 + var_RR + ((.data[[sd_variable]]^2) * (.data[[sd_variable_w]]^2)) / 
              (n^2 * .data[[mean_variable]]^2 * .data[[mean_variable_w]]^2)),
      se_sigma_RR = sqrt(var_sigma_RR)  # Error estándar de sigma_RR_wp_vs_w
    ) %>% 
    select(date, sampling, RR_descriptor, RR, se_RR, delta_RR, se_delta_RR, 
           sigma_RR, se_sigma_RR)%>% 
    filter(!RR == "Inf")%>% 
    filter(!RR == "-Inf")
  
  
  
  RR_wp_vs_p <- effect_wp %>% 
    left_join(effect_p, by = c("date", "sampling")) %>% 
    
    mutate(
      # Cálculo del Log Response Ratio (RR)
      RR = log(.data[[mean_variable]] / .data[[mean_variable_p]]),
      
      # Cálculo de la varianza de RR_wp_vs_w
      var_RR = (.data[[sd_variable]]^2) / (n * .data[[mean_variable]]^2) + 
        (.data[[sd_variable_p]]^2) / (n * .data[[mean_variable_p]]^2),
      
      se_RR = sqrt(var_RR)  # Error estándar de RR_wp_vs_w
      
    ) %>% 
    mutate(
      # Cálculo de delta_RR_wp_vs_w (ajuste de sesgo)
      delta_RR= RR + 0.5 * (
        (.data[[sd_variable]]^2) / (n * .data[[mean_variable]]^2) - 
          (.data[[sd_variable_p]]^2) / (n * .data[[mean_variable_p]]^2)
      ),
      
      # Varianza de delta_RR_wp_vs_w
      var_delta_RR = var_RR + 0.5 * (
        (.data[[sd_variable]]^4) / (n^2 * .data[[mean_variable]]^4) + 
          (.data[[sd_variable_p]]^4) / (n^2 * .data[[mean_variable_p]]^4)
      ),
      se_delta_RR = sqrt(var_delta_RR),  # Error estándar de delta_RR_wp_vs_w
      
      # Cálculo de sigma_RR_wp_vs_w
      sigma_RR = 0.5 * log(
        (.data[[mean_variable]]^2 + (.data[[sd_variable]]^2) / n) / 
          (.data[[mean_variable_p]]^2 + (.data[[sd_variable_p]]^2) / n)
      ),
      
      # Varianza de sigma_RR_wp_vs_w
      var_sigma_RR = 2.0 * var_RR - 
        log(1.0 + var_RR + ((.data[[sd_variable]]^2) * (.data[[sd_variable_p]]^2)) / 
              (n^2 * .data[[mean_variable]]^2 * .data[[mean_variable_p]]^2)),
      se_sigma_RR = sqrt(var_sigma_RR)  # Error estándar de sigma_RR_wp_vs_w
    ) %>% 
    select(date, sampling, RR_descriptor, RR, se_RR, delta_RR, se_delta_RR, 
           sigma_RR, se_sigma_RR)%>% 
    filter(!RR == "Inf")%>% 
    filter(!RR == "-Inf") %>% 
    filter(!RR == "NaN")
  
  
  RR_wp_vs_treatment <- rbind(RR_wp_vs_p, RR_wp_vs_w)
    
    
    
  
  RR_wp_vs_treatment <<- RR_wp_vs_treatment
  
  
  gg_RR_wp <- 
    ggplot(RR_wp_vs_treatment, aes(x = date, y = RR)) + 
    facet_wrap(~ RR_descriptor, labeller = labeller(RR_descriptor = labels_RR_wp)) +
    #geom_ribbon(aes(ymin = RR - se_RR, ymax = RR + se_RR, fill = treatment), alpha = 0.2) +
    geom_errorbar(aes(ymin = RR - se_RR,
                      ymax = RR + se_RR,
                      color = RR_descriptor), alpha = 0.5) +
    geom_point(aes(color = RR_descriptor)) + 
    geom_line(aes(color = RR_descriptor)) +
    scale_color_manual(values = palette_wp_vs_treatment) +
    scale_fill_manual(values = palette_wp_vs_treatment) +
    geom_hline( yintercept= 0, linetype = "dashed", color = "gray40") +
    geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
    labs(x = NULL, y = paste0("RR ", ytitle)) +
    theme(legend.position = "none")
  
  gg_RR_wp <<- gg_RR_wp
  
  gg_delta_RR_wp <- 
    ggplot(RR_wp_vs_treatment, aes(x = date, y = delta_RR)) + 
    facet_wrap(~ RR_descriptor, labeller = labeller(RR_descriptor = labels_RR_wp)) +
    #geom_ribbon(aes(ymin = RR - se_RR, ymax = RR + se_RR, fill = treatment), alpha = 0.2) +
    geom_errorbar(aes(ymin = delta_RR - se_delta_RR,
                      ymax = delta_RR + se_delta_RR,
                      color = RR_descriptor), alpha = 0.5) +
    geom_point(aes(color = RR_descriptor)) + 
    geom_line(aes(color = RR_descriptor)) +
    scale_color_manual(values = palette_wp_vs_treatment) +
    scale_fill_manual(values = palette_wp_vs_treatment) +
    geom_hline( yintercept= 0, linetype = "dashed", color = "gray40") +
    geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
    labs(x = NULL, y = paste0("delta-RR ", ytitle)) +
    theme(legend.position = "none")
  
  gg_delta_RR_wp <<- gg_delta_RR_wp
  
  gg_sigma_RR_wp <- 
    ggplot(RR_wp_vs_treatment, aes(x = date, y = sigma_RR)) + 
    facet_wrap(~ RR_descriptor, labeller = labeller(RR_descriptor = labels_RR_wp)) +
    #geom_ribbon(aes(ymin = RR - se_RR, ymax = RR + se_RR, fill = treatment), alpha = 0.2) +
    geom_errorbar(aes(ymin = delta_RR - se_sigma_RR,
                      ymax = delta_RR + se_sigma_RR,
                      color = RR_descriptor), alpha = 0.5) +
    geom_point(aes(color = RR_descriptor)) + 
    geom_line(aes(color = RR_descriptor)) +
    scale_color_manual(values = palette_wp_vs_treatment) +
    scale_fill_manual(values = palette_wp_vs_treatment) +
    geom_hline( yintercept= 0, linetype = "dashed", color = "gray40") +
    geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
    labs(x = NULL, y = paste0("sigma-RR ", ytitle)) +
    theme(legend.position = "none")
  
  gg_sigma_RR_wp <<- gg_sigma_RR_wp
  
  
  
}