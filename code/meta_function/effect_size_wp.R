







effect_size_wp <- function(data, variable){
  
  
  mean_variable <- paste0("mean_", variable)
  sd_variable <- paste0("sd_", variable)
  
  mean_variable_wp <- paste0("mean_", variable, "_wp")
  sd_variable_wp<- paste0("sd_", variable, "_wp")
  
  
  effect <- data %>% 
    select(date, sampling, treatment, all_of(mean_variable), all_of(sd_variable)) %>% 
    distinct()
  
  effect_wp <- effect %>% 
    filter(treatment == "wp") %>% 
    select(date, sampling, all_of(mean_variable), all_of(sd_variable)) %>% 
    rename(!!mean_variable_wp := !!sym(mean_variable),
           !!sd_variable_wp := !!sym(sd_variable))
  
  
  
  
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
  
  RR_treatment_wp <- effect %>% 
    filter(!treatment %in% c("c", "wp")) %>% 
    left_join(effect_wp, by = c("date", "sampling")) %>% 
    mutate(
      # Cálculo del Log Response Ratio (RR)
      RR = log(.data[[mean_variable]] / .data[[mean_variable_wp]]),
      
      # Cálculo de la varianza de RR
      var_RR = (.data[[sd_variable]]^2) / (n * .data[[mean_variable]]^2) + 
        (.data[[sd_variable_wp]]^2) / (n * .data[[mean_variable_wp]]^2),
      
      se_RR = sqrt(var_RR)  # Error estándar de RR
    ) %>% 
    mutate(
      # Cálculo de delta_RR (ajuste de sesgo)
      delta_RR = RR + 0.5 * (
        (.data[[sd_variable]]^2) / (n * .data[[mean_variable]]^2) - 
          (.data[[sd_variable_wp]]^2) / (n * .data[[mean_variable_wp]]^2)
      ),
      
      # Varianza de delta_RR
      var_delta_RR = var_RR + 0.5 * (
        (.data[[sd_variable]]^4) / (n^2 * .data[[mean_variable]]^4) + 
          (.data[[sd_variable_wp]]^4) / (n^2 * .data[[mean_variable_wp]]^4)
      ),
      se_delta_RR = sqrt(var_delta_RR),  # Error estándar de delta_RR
      
      # Cálculo de sigma_RR
      sigma_RR = 0.5 * log(
        (.data[[mean_variable]]^2 + (.data[[sd_variable]]^2) / n) / 
          (.data[[mean_variable_wp]]^2 + (.data[[sd_variable_wp]]^2) / n)
      ),
      
      # Varianza de sigma_RR
      var_sigma_RR = 2.0 * var_RR - 
        log(1.0 + var_RR + ((.data[[sd_variable]]^2) * (.data[[sd_variable_wp]]^2)) / 
              (n^2 * .data[[mean_variable]]^2 * .data[[mean_variable_wp]]^2)),
      se_sigma_RR = sqrt(var_sigma_RR)  # Error estándar de sigma_RR
    )
  
  
  gg_RR_wp <- 
    ggplot(RR_treatment_wp, aes(x = date, y = RR)) + 
    facet_wrap(~ treatment, labeller = labeller(treatment = labels_RR_wp)) +
    #geom_ribbon(aes(ymin = RR - se_RR, ymax = RR + se_RR, fill = treatment), alpha = 0.2) +
    geom_errorbar(aes(ymin = RR - se_RR,
                      ymax = RR + se_RR,
                      color = treatment), alpha = 0.5) +
    geom_point(aes(color = treatment)) + 
    geom_line(aes(color = treatment)) +
    scale_color_manual(values = palette_wp) +
    scale_fill_manual(values = palette_wp) +
    geom_hline( yintercept= 0, linetype = "dashed", color = "gray40") +
    geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
    labs(x = NULL, y = paste0("RR ", ytitle)) +
    theme(legend.position = "none")
  
  gg_RR_wp <<- gg_RR_wp
  
  gg_delta_RR_wp <- 
    ggplot(RR_treatment_wp, aes(x = date, y = delta_RR)) + 
    facet_wrap(~ treatment, labeller = labeller(treatment = labels_RR_wp)) +
    geom_errorbar(aes(ymin = delta_RR - se_delta_RR,
                      ymax = delta_RR + se_delta_RR,
                      color = treatment), alpha = 0.5) +
    geom_point(aes(color = treatment)) + 
    geom_line(aes(color = treatment)) +
    scale_color_manual(values = palette_wp) +
    geom_hline( yintercept= 0, linetype = "dashed", color = "gray40") +
    geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
    labs(x = NULL, y = paste0("delta-RR ", ytitle)) +
    theme(legend.position = "none")
  
  gg_delta_RR_wp <<- gg_delta_RR_wp
  
  gg_sigma_RR_wp <- 
    ggplot(RR_treatment_wp, aes(x = date, y = sigma_RR)) + 
    facet_wrap(~ treatment, labeller = labeller(treatment = labels_RR_wp)) +
    geom_errorbar(aes(ymin = sigma_RR - se_sigma_RR,
                      ymax = sigma_RR + se_sigma_RR,
                      color = treatment), alpha = 0.5) +
    geom_point(aes(color = treatment)) + 
    geom_line(aes(color = treatment)) +
    scale_color_manual(values = palette_wp) +
    geom_hline( yintercept= 0, linetype = "dashed", color = "gray40") +
    geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
    labs(x = NULL, y = paste0("sigma-RR ", ytitle)) +
    theme(legend.position = "none")
  
  gg_sigma_RR_wp <<- gg_sigma_RR_wp
  
 
  
}