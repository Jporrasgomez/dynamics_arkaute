







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
  
  
  
  
  # define y axis title
  if (variable == "richness") {
    ytitle <- "richness"
  } else {
    if (variable == "abundance") {
      ytitle = "community cover"
    } else {
      if (variable == "sigma_log") {
        ytitle <- "Sigma (Coefficient 2 in Log model for RADs)"
      } else {
        if (variable == "mu_log") {
          ytitle <- "Mu (Coefficient 1 in Log model for RADs)"
        } else {
          if (variable == "Y_zipf") {
            ytitle <- "Gamma (Coefficient in Zipf model for RADs)"
          } else {
            if (variable == "biomass") {
              ytitle <- "community biomass"
            } else{
              stop("variable must be one of the following: richness, abundance, sigma_log, mu_log, Y_zipf, biomass")
            }
          }
        }
      }
    }
  }
  
  
  n = 4
  
  labels <- labels_RR_wp
  
  ### Perturbation / Global Change
  
  RR_treatment_p <- effect %>% 
    filter(!treatment %in% c("c", "wp", "w")) %>% 
    left_join(effect_c, by = c("date", "sampling")) %>% 
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
  
  
  gg_RR_p <- 
    ggplot(RR_treatment_p, aes(x = date, y = RR)) + 
    facet_wrap(~ treatment, labeller = labeller(treatment = labels)) +
    #geom_ribbon(aes(ymin = RR - se_RR, ymax = RR + se_RR, fill = treatment), alpha = 0.2) +
    geom_errorbar(aes(ymin = RR - se_RR,
                      ymax = RR + se_RR,
                      color = treatment), alpha = 0.5) +
    geom_point(aes(color = treatment)) + 
    geom_line(aes(color = treatment)) +
    scale_color_manual(values = palette) +
    scale_fill_manual(values = palette) +
    geom_hline( yintercept= 0, linetype = "dashed", color = "gray40") +
    geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
    labs(x = NULL, y = paste0("RR ", ytitle)) +
    theme(legend.position = "none")
  
  gg_RR_p <<- gg_RR_p
  
  gg_delta_RR_p <- 
    ggplot(RR_treatment_p, aes(x = date, y = delta_RR)) + 
    facet_wrap(~ treatment, labeller = labeller(treatment = labels)) +
    geom_errorbar(aes(ymin = delta_RR - se_delta_RR,
                      ymax = delta_RR + se_delta_RR,
                      color = treatment), alpha = 0.5) +
    geom_point(aes(color = treatment)) + 
    geom_line(aes(color = treatment)) +
    scale_color_manual(values = palette) +
    geom_hline( yintercept= 0, linetype = "dashed", color = "gray40") +
    geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
    labs(x = NULL, y = paste0("delta-RR ", ytitle)) +
    theme(legend.position = "none")
  
  gg_delta_RR_p <<- gg_delta_RR_p
  
  gg_sigma_RR_p <- 
    ggplot(RR_treatment_p, aes(x = date, y = sigma_RR)) + 
    facet_wrap(~ treatment, labeller = labeller(treatment = labels)) +
    geom_errorbar(aes(ymin = sigma_RR - se_sigma_RR,
                      ymax = sigma_RR + se_sigma_RR,
                      color = treatment), alpha = 0.5) +
    geom_point(aes(color = treatment)) + 
    geom_line(aes(color = treatment)) +
    scale_color_manual(values = palette) +
    geom_hline( yintercept= 0, linetype = "dashed", color = "gray40") +
    geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
    labs(x = NULL, y = paste0("sigma-RR ", ytitle)) +
    theme(legend.position = "none")
  
  gg_sigma_RR_p <<- gg_sigma_RR_p
  
  
  
  ### Warming / Global Change
  
  
  RR_treatment_w <- effect %>% 
    filter(!treatment %in% c("c", "wp", "p")) %>% 
    left_join(effect_c, by = c("date", "sampling")) %>% 
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
  
  
  gg_RR_w <- 
    ggplot(RR_treatment_w, aes(x = date, y = RR)) + 
    facet_wrap(~ treatment, labeller = labeller(treatment = labels)) +
    #geom_ribbon(aes(ymin = RR - se_RR, ymax = RR + se_RR, fill = treatment), alpha = 0.2) +
    geom_errorbar(aes(ymin = RR - se_RR,
                      ymax = RR + se_RR,
                      color = treatment), alpha = 0.5) +
    geom_point(aes(color = treatment)) + 
    geom_line(aes(color = treatment)) +
    scale_color_manual(values = palette) +
    scale_fill_manual(values = palette) +
    geom_hline( yintercept= 0, linetype = "dashed", color = "gray40") +
    geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
    labs(x = NULL, y = paste0("RR ", ytitle)) +
    theme(legend.position = "none")
  
  gg_RR_w <<- gg_RR_w
  
  gg_delta_RR_w <- 
    ggplot(RR_treatment_w, aes(x = date, y = delta_RR)) + 
    facet_wrap(~ treatment, labeller = labeller(treatment = labels)) +
    geom_errorbar(aes(ymin = delta_RR - se_delta_RR,
                      ymax = delta_RR + se_delta_RR,
                      color = treatment), alpha = 0.5) +
    geom_point(aes(color = treatment)) + 
    geom_line(aes(color = treatment)) +
    scale_color_manual(values = palette) +
    geom_hline( yintercept= 0, linetype = "dashed", color = "gray40") +
    geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
    labs(x = NULL, y = paste0("delta-RR ", ytitle)) +
    theme(legend.position = "none")
  
  gg_delta_RR_w <<- gg_delta_RR_w
  
  gg_sigma_RR_w <- 
    ggplot(RR_treatment_w, aes(x = date, y = sigma_RR)) + 
    facet_wrap(~ treatment, labeller = labeller(treatment = labels)) +
    geom_errorbar(aes(ymin = sigma_RR - se_sigma_RR,
                      ymax = sigma_RR + se_sigma_RR,
                      color = treatment), alpha = 0.5) +
    geom_point(aes(color = treatment)) + 
    geom_line(aes(color = treatment)) +
    scale_color_manual(values = palette) +
    geom_hline( yintercept= 0, linetype = "dashed", color = "gray40") +
    geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
    labs(x = NULL, y = paste0("sigma-RR ", ytitle)) +
    theme(legend.position = "none")
  
  gg_sigma_RR_w <<- gg_sigma_RR_w
  
  
  

  
}