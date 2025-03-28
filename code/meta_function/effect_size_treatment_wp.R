


effect_size_treatment_wp <- function(data, variable){
  
  
  mean_variable <- paste0("mean_", variable)
  sd_variable <- paste0("sd_", variable)
  
  mean_variable_wp <- paste0("mean_", variable, "_wp")
  sd_variable_wp<- paste0("sd_", variable, "_wp")
  
  
  effect <- data %>% 
    select(treatment, n, all_of(mean_variable), all_of(sd_variable)) %>% 
    distinct()
  
  
  effect_wp <- effect %>% 
    filter(treatment == "wp") %>% 
    select(treatment, n, all_of(mean_variable), all_of(sd_variable)) %>%  # Mantener treatment
    rename(!!mean_variable_wp := !!sym(mean_variable),
           !!sd_variable_wp := !!sym(sd_variable),
           n_wp = n)
  
  
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
  

  ### Perturbation / Global Change
  RR_treatment_wp <- effect %>% 
    filter(!treatment %in% c("c", "wp")) %>% 
    mutate(
      !!mean_variable_wp := effect_wp[[mean_variable_wp]],
      !!sd_variable_wp := effect_wp[[sd_variable_wp]],
      !!sym("n_wp") := effect_wp[["n_wo"]]
      
    ) %>% 
    mutate(
      # Cálculo del Log Response Ratio (RR)
      RR = log(.data[[mean_variable]] / .data[[mean_variable_wp]]),
      
      # Cálculo de la varianza de RR
      se_RR = sqrt((.data[[sd_variable]]^2) / (n * .data[[mean_variable]]^2) + 
        (.data[[sd_variable_wp]]^2) / (n * .data[[mean_variable_wp]]^2))
    ) %>% 
    mutate(
      variable = variable
    )
  
  RR_treatment_wp <- RR_treatment_wp %>% 
    filter(!RR == "Inf")
  
  RR_treatment_wp <<- RR_treatment_wp
  
  
  gg_RR_wp <- 
    ggplot(RR_treatment_wp, aes(x = treatment, y = RR)) + 
    geom_errorbar(aes(ymin = RR - se_RR,
                      ymax = RR + se_RR,
                      color = treatment)) +
    geom_point(aes(color = treatment)) + 
    geom_line(aes(color = treatment)) +
    scale_color_manual(values = palette_wp) +
    scale_x_discrete(labels = labels_RR_wp) +
    geom_hline( yintercept= 0, linetype = "dashed", color = "gray40") +
    labs(x = NULL, y = paste0("RR ", ytitle)) +
    theme(legend.position = "none")
  
  
  gg_RR_wp <<- gg_RR_wp
  
}
  
  