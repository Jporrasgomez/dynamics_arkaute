


RR_treatment_wp <- function(data, variable){
  


  
  data <- data %>% 
    distinct(treatment, plot, sampling, date, .data[[variable]], .keep_all = TRUE) %>% 
    filter(!is.na(.data[[variable]])) %>% 
    group_by(treatment) %>% 
    mutate(
      n = n(),
      mean := mean(.data[[variable]], na.rm = TRUE),
      sd := sd(.data[[variable]], na.rm = TRUE)
    ) %>%
    ungroup() %>% 
    select(treatment, sampling, date, plot, n,
           !!variable, mean, sd) %>% 
    mutate( variable = variable)
  
  
  
  effect_wp <- data %>% 
    filter(treatment == "wp") %>% 
    ungroup() %>% 
    select(treatment, n, mean, sd) %>% 
    distinct()%>% 
    select(-treatment)
  
  effect_w <- data %>% 
    filter(treatment == "w") %>% 
    ungroup() %>% 
    select(n, treatment, mean, sd) %>% 
    rename(mean_w = mean,
           sd_w = sd, 
           n_w = n) %>% 
    mutate(RR_descriptor = "wp_vs_w") %>% 
    distinct() %>% 
    select(-treatment)
  
  effect_p <- data %>% 
    filter(treatment == "p") %>% 
    ungroup() %>% 
    select(n, treatment, mean, sd) %>% 
    rename(mean_p = mean,
           sd_p = sd,
           n_p = n) %>% 
    mutate(RR_descriptor = "wp_vs_p") %>% 
    distinct()%>% 
    select(-treatment)
  
  
  
  ytitle_dict <- list(
    "richness"   = "Richness",
    "abundance"  = "Community cover",
    "sigma_log"  = "Sigma (Coefficient 2 in Log model for RADs)",
    "mu_log"     = "Mu (Coefficient 1 in Log model for RADs)",
    "Y_zipf"     = "Gamma (Coefficient in Zipf model for RADs)",
    "biomass"    = "Community biomass",
    "biomass012"    = "Community biomass",
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
  

  ### Perturbation / Global Change
  RR_wp_vs_w <- effect_wp %>% 
    cbind(effect_w) %>% 
    mutate(
      # Cálculo del Log Response Ratio (RR)
      RR = log(mean / mean_w),
      
      # Cálculo de la varianza de RR
      se_RR = sqrt((sd^2) / (n * mean^2) + 
        (sd_w^2) / (n_w * mean_w^2))
    ) %>% 
    mutate(
      variable = variable
    ) %>% 
    filter(!RR == "Inf") %>% 
    select(RR_descriptor, RR, se_RR, variable) %>% 
    mutate(
      RR = (exp(RR) -1) *100,
      se_RR = (exp(se_RR) - 1) * 100
    )  %>% 
    mutate(
      RR_perc = 100*(exp(RR)-1),
      se_RR_perc = 100*(exp(se_RR)-1)
    )
  
  
  
  RR_wp_vs_p <- effect_wp %>% 
    cbind(effect_p) %>% 
    mutate(
      # Cálculo del Log Response Ratio (RR)
      RR = log(mean / mean_p),
      
      # Cálculo de la varianza de RR
      se_RR = sqrt((sd^2) / (n * mean^2) + 
                     (sd_p^2) / (n_p * mean_p^2))
    ) %>% 
    mutate(
      variable = variable
    ) %>% 
    filter(!RR == "Inf") %>% 
    select(RR_descriptor, RR, se_RR, variable) %>% 
    mutate(
      RR_perc = 100*(exp(RR)-1),
      se_RR_perc = 100*(exp(se_RR)-1)
    )
  
  RR_wp_vs_treatment <- rbind(RR_wp_vs_p, RR_wp_vs_w)
  
  RR_wp_vs_treatment <<- RR_wp_vs_treatment
  
  z = 1.96
  
  
  gg_RR_wp <- 
    ggplot(RR_wp_vs_treatment, aes(x = RR_descriptor, y = RR)) + 
    facet_wrap(~RR_descriptor, scales = "free_x") +  # 🔹 Permite que cada faceta tenga su propio eje X
    geom_errorbar(aes(ymin = RR - z * se_RR,
                      ymax = RR + z * se_RR,
                      color = RR_descriptor)) +
    geom_point(aes(color = RR_descriptor)) + 
    geom_line(aes(color = RR_descriptor, group = 1)) +  # 🔹 Agrupar para evitar la advertencia
    scale_color_manual(values = palette_RR_wp) +
    scale_x_discrete(labels = labels_RR_wp) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    labs(x = NULL, y = paste0("RR ", ytitle)) +
    theme(legend.position = "none")
  
  gg_RR_wp
  
  
  gg_RR_wp <<- gg_RR_wp
  
}
  
  