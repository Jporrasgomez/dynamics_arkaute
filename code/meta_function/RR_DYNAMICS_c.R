



RR_dynamics_c <- function(data, variable){
  

  effect <- data %>% 
    select(date, sampling, treatment, mean, sd) %>% 
    distinct()
  
  effect_c <- effect %>% 
    filter(treatment == "c") %>% 
    select(date, sampling, mean, sd) %>% 
    rename(mean_c = mean,
           sd_c = sd)
  
  
  ytitle_dict <- list(
    "richness" = "Richness",
    "abundance" = "Community cover",
    "sigma_log" = "Sigma (Coefficient 2 in Log model for RADs)",
    "mu_log" = "Mu (Coefficient 1 in Log model for RADs)",
    "Y_zipf" = "Gamma (Coefficient in Zipf model for RADs)",
    "biomass" = "Community biomass",
    "NMDS1" = "NMDS1",
    "NMDS2" = "NMDS2",
    "NMDS3" = "NMDS3",
    "total_turnover" = "Total turnover", 
    "appearance" = "Turnover: appearance", 
    "disappearance" = "Turnover: disappearance",
    "SLA" = "SLA (?)", 
    "LA" = "LA(?)", 
    "LDMC" = "LDMC(?)", 
    "leafN"= "Leaf nitrogen(?)",
    "seed.mass" = "Seed mass",
    "vegetation.height" = "Vegetation height",
    "PC1" = "Functional diversity(PC1)",
    "PC2" = "Functional diverisity (PC2)"
  )
  
  ytitle <- ytitle_dict[[variable]]
  
  if (is.null(ytitle)) {
    stop("Variable must be one of the following: ", paste(names(ytitle_dict), collapse = ", "))
  }
  
  
  
  n = 4
  
  RR_treatment <- effect %>% 
    filter(treatment != "c") %>% 
    left_join(effect_c, by = c("date", "sampling")) %>% 
    mutate(
      # Cálculo del Log Response Ratio (RR)
      RR = log(mean / mean_c),
      
      # Cálculo de la varianza de RR
      var_RR = (sd^2) / (n * mean^2) + 
        (sd_c^2) / (n * mean_c^2),
      
      se_RR = sqrt(var_RR)  # Error estándar de RR
    ) %>% 
    mutate(
      # Cálculo de delta_RR (ajuste de sesgo)
      delta_RR = RR + 0.5 * (
        (sd^2) / (n * mean^2) - 
          (sd_c^2) / (n * mean_c^2)
      ),
      
      # Varianza de delta_RR
      var_delta_RR = var_RR + 0.5 * (
        (sd^4) / (n^2 * mean^4) + 
          (sd_c^4) / (n^2 * mean_c^4)
      ),
      se_delta_RR = sqrt(var_delta_RR),  # Error estándar de delta_RR
      
      # Cálculo de sigma_RR
      sigma_RR = 0.5 * log(
        (mean^2 + (sd^2) / n) / 
          (mean_c^2 + (sd_c^2) / n)
      ),
      
      # Varianza de sigma_RR
      var_sigma_RR = 2.0 * var_RR - 
        log(1.0 + var_RR + ((sd^2) * (sd_c^2)) / 
              (n^2 * mean^2 * mean_c^2)),
      se_sigma_RR = sqrt(var_sigma_RR)  # Error estándar de sigma_RR
    ) %>% 
    mutate(
      variable = variable
    )
  
  RR_treatment <- RR_treatment %>% 
    filter(! RR == "-Inf") %>% 
    rename(RR_descriptor = treatment) %>% 
    mutate(
      RR_descriptor = fct_recode(RR_descriptor,
                            "w_vs_c" = "w",
                            "p_vs_c" = "p", 
                            "wp_vs_c" = "wp"))
    
  
  RR_treatment <<- RR_treatment
  
  z = 1.96
  
  gg_RR <- 
    ggplot(RR_treatment, aes(x = date, y = RR)) + 
    facet_wrap(~ RR_descriptor, labeller = labeller(RR_descriptor = labels_RR)) +
    #geom_ribbon(aes(ymin = RR - se_RR, ymax = RR + se_RR, fill = RR_descriptor), alpha = 0.2) +
    geom_errorbar(aes(ymin = RR - z * se_RR,
                      ymax = RR + z * se_RR,
                      color = RR_descriptor), alpha = 0.5) +
    geom_point(aes(color = RR_descriptor)) + 
    geom_line(aes(color = RR_descriptor)) +
    scale_color_manual(values = palette_RR) +
    geom_hline( yintercept= 0, linetype = "dashed", color = "gray40") +
    geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
    labs(x = NULL, y = paste0("RR ", ytitle)) +
    theme(legend.position = "none")
  
  gg_RR <<- gg_RR
  
  gg_delta_RR <- 
    ggplot(RR_treatment, aes(x = date, y = delta_RR)) + 
    facet_wrap(~ RR_descriptor, labeller = labeller(RR_descriptor = labels_RR)) +
    geom_errorbar(aes(ymin = delta_RR - z * se_delta_RR,
                      ymax = delta_RR + z * se_delta_RR,
                      color = RR_descriptor), alpha = 0.5) +
    geom_point(aes(color = RR_descriptor)) + 
    geom_line(aes(color = RR_descriptor)) +
    scale_color_manual(values = palette_RR) +
    geom_hline( yintercept= 0, linetype = "dashed", color = "gray40") +
    geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
    labs(x = NULL, y = paste0("delta-RR ", ytitle)) +
    theme(legend.position = "none")
  
  gg_delta_RR <<- gg_delta_RR
  
  gg_sigma_RR <- 
    ggplot(RR_treatment, aes(x = date, y = sigma_RR)) + 
    facet_wrap(~ RR_descriptor, labeller = labeller(RR_descriptor = labels_RR)) +
    geom_errorbar(aes(ymin = sigma_RR - z * se_sigma_RR,
                      ymax = sigma_RR + z * se_sigma_RR,
                      color = RR_descriptor), alpha = 0.5) +
    geom_point(aes(color = RR_descriptor)) + 
    geom_line(aes(color = RR_descriptor)) +
    scale_color_manual(values = palette_RR) +
    geom_hline( yintercept= 0, linetype = "dashed", color = "gray40") +
    geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
    labs(x = NULL, y = paste0("sigma-RR ", ytitle)) +
    theme(legend.position = "none")
  
  gg_sigma_RR <<- gg_sigma_RR
  
  
  
  
  
}