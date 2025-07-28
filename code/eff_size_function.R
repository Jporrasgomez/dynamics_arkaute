
effect_size <- function(data, variable){
  
  
  data <- data %>% 
    select(treatment, all_of(variable)) %>% 
    filter(!is.na(.data[[variable]])) %>% 
    rename(
      value = all_of(variable)
    ) %>% 
    group_by(treatment) %>% 
    mutate(
      n = n(),
      mean := mean(value),
      sd := sd(value)
    ) %>% 
    mutate(variable = variable) 
  
  RR_data <- NULL
  
  if (!variable %in% c("NMDS1", "NMDS2", "PC1", "PC2")) {
  
  ## Log Response Ratio Analysis ##
  
  RR_effect <- data %>% 
    select(treatment, n, mean, sd) %>% 
    distinct()
  
  RR_effect_c <- RR_effect %>% 
    filter(treatment == "c") %>% 
    rename(mean_c = mean,
           sd_c = sd,
           n_c = n) %>%
    ungroup() %>% 
    select(-treatment)
  
  RR_effect_wp <- RR_effect %>% 
    filter(treatment == "wp") %>% 
    rename(mean_wp = mean,
           sd_wp = sd, 
           n_wp = n) %>% 
    ungroup() %>% 
    select(-treatment)
  
  RR_effect_w <- RR_effect %>% 
    filter(treatment == "w") %>% 
    rename(mean_w = mean,
           sd_w = sd, 
           n_w = n) %>% 
    ungroup() %>% 
    select(-treatment)
  
  RR_effect_p <- RR_effect %>% 
    filter(treatment == "p") %>% 
    rename(mean_p = mean,
           sd_p = sd,
           n_p = n) %>% 
    ungroup() %>% 
    select(-treatment)
  
  

  
  RR_treatment_c <- RR_effect %>% 
    filter(treatment != "c") %>% 
    mutate(
      mean_c = RR_effect_c$mean_c,
      sd_c = RR_effect_c$sd_c,
      n_c = RR_effect_c$n_c
    ) %>% 
    mutate(
      eff_value =  log(mean / mean_c),
      se_eff =  sqrt((sd^2) / (n * mean^2) + 
                      (sd_c^2) / (n_c * mean_c^2))
    ) %>% 
    mutate(
      eff_value = if_else(variable == "Y_zipf", -1 * eff_value, eff_value)
    ) %>% 
    mutate(
      variable = variable
    ) %>% 
    rename(eff_descriptor = treatment) %>% 
    mutate(
      eff_descriptor = fct_recode(eff_descriptor,
                                 "w_vs_c" = "w",
                                 "p_vs_c" = "p", 
                                 "wp_vs_c" = "wp")) %>%
    mutate(
      analysis = paste0("LRR")
    ) %>% 
    select(eff_descriptor, eff_value, se_eff, variable, analysis) 
  
  
  
  RR_wp_vs_p <- RR_effect_wp %>% 
    cbind(RR_effect_p) %>% 
    mutate(
      eff_value = log(mean_wp / mean_p),
      
      se_eff = sqrt((sd_wp^2) / (n_wp * mean_wp^2) + 
                     (sd_p^2) / (n_p * mean_p^2))
    )  %>% 
    mutate(
      eff_value = if_else(variable == "Y_zipf", -1 * eff_value, eff_value)
    ) %>% 
    mutate(
      variable = variable,
      eff_descriptor = paste0("wp_vs_p")
    ) %>% 
    filter(!eff_value == "Inf") %>% 
    mutate(
      analysis = paste0("LRR")
    ) %>% 
    select(eff_descriptor, eff_value, se_eff, variable, analysis) 
  
  
  
  RR_data <- rbind(RR_treatment_c, RR_wp_vs_p) %>%
    mutate(
      upper_limit = eff_value +  se_eff * 1.96,
      lower_limit = eff_value - se_eff * 1.96) %>% 
    select(eff_descriptor, eff_value, upper_limit, lower_limit, variable, analysis) 
  
  #RR_data <<- RR_data
  
}
  
  ## Cohen's D analysis ##
  

  a <- 
  cohen.d(
    (data %>% filter(treatment == "w") %>% pull(value)),
    (data %>% filter(treatment == "c") %>% pull(value)),
    hedges.correction = FALSE
  )
  
  b <- 
    cohen.d(
      (data %>% filter(treatment == "p") %>% pull(value)),
      (data %>% filter(treatment == "c") %>% pull(value)),
      hedges.correction = FALSE
    )
  
  c <- 
    cohen.d(
      (data %>% filter(treatment == "wp") %>% pull(value)),
      (data %>% filter(treatment == "c") %>% pull(value)),
      hedges.coRR_effection = FALSE
    )
  
  d <- 
    cohen.d(
      (data %>% filter(treatment == "wp") %>% pull(value)),
      (data %>% filter(treatment == "p") %>% pull(value)),
      hedges.correction = FALSE
    )
  
  
  variable_name <- unique(data$variable)
  cohen_data <- data.frame(
    eff_descriptor = c("w_vs_c", "p_vs_c", "wp_vs_c", "wp_vs_p"),
    eff_value  = c(a$estimate, b$estimate, c$estimate, d$estimate),
    upper_limit = c(a$conf.int[2], b$conf.int[2], c$conf.int[2], d$conf.int[2]),
    lower_limit = c(a$conf.int[1], b$conf.int[1], c$conf.int[1], d$conf.int[1]),
    variable    = rep(variable_name, 4),
    analysis    = rep("cohen's_d", 4)
    )
  
  cohen_data <<- cohen_data
  
  
  
  if (!is.null(RR_data)) {
    effsize_data <- bind_rows(RR_data, cohen_data)
  } else {
    effsize_data <- cohen_data
  }
  
 effsize_data <- effsize_data %>% 
    mutate(
      null_effect = ifelse(lower_limit <= 0 & upper_limit >= 0, "YES","NO"),
      scale = (max(abs(upper_limit)) + max(abs(lower_limit)))/100
      ) %>% 
   select(
     eff_descriptor, eff_value, lower_limit, upper_limit, null_effect, scale,
     variable, analysis
   )
  
  

  effsize_data <<- effsize_data

}
