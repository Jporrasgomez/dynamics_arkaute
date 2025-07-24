

data <- arkaute_no0
variable <- "richness"


RR_treatment_c <- function(data, variable){
  
  
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
    mutate( variable = variable) %>% 
    rename(
      value = .data[[variable]]
    )
  
  
  effect <- data %>% 
    select(treatment, n, mean, sd) %>% 
    distinct()
  
  effect_c <- effect %>% 
    filter(treatment == "c") %>% 
    select(treatment, n, mean, sd) %>%  # Mantener treatment
    rename(mean_c = mean,
           sd_c = sd,
           n_c = n) %>% 
    distinct()%>% 
    select(-treatment)
  
  effect_wp <- data %>% 
    filter(treatment == "wp") %>% 
    ungroup() %>% 
    select(treatment, n, mean, sd) %>% 
    rename(mean_wp = mean,
           sd_wp = sd, 
           n_wp = n) %>% 
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
  
  

  
  RR_treatment_c <- effect %>% 
    filter(treatment != "c") %>% 
    mutate(
      mean_c = effect_c$mean_c,
      sd_c = effect_c$sd_c,
      n_c = effect_c$n_c
    ) %>% 
    mutate(
      RR =  log(mean / mean_c),
      se_RR =  sqrt((sd^2) / (n * mean^2) + 
                      (sd_c^2) / (n_c * mean_c^2))
    ) %>%
    mutate(
      upper_limit = RR +  se_RR * 1.96,
      lower_limit = RR - se_RR * 1.96
    ) %>% 
  
    mutate(
      variable = variable
    ) %>% 
    rename(RR_descriptor = treatment) %>% 
    mutate(
      RR_descriptor = fct_recode(RR_descriptor,
                                 "w_vs_c" = "w",
                                 "p_vs_c" = "p", 
                                 "wp_vs_c" = "wp")) %>% 
    select(RR_descriptor, RR, upper_limit, lower_limit, variable) 
  
  
  
  RR_wp_vs_p <- effect_wp %>% 
    cbind(effect_p) %>% 
    mutate(
      RR = log(mean_wp / mean_p),
      
      se_RR = sqrt((sd_wp^2) / (n_wp * mean_wp^2) + 
                     (sd_p^2) / (n_p * mean_p^2))
    ) %>% 
    mutate(
      upper_limit = RR + se_RR * 1.96,
      lower_limit = RR - se_RR * 1.96
    ) %>% 

    mutate(
      variable = variable
    ) %>% 
    filter(!RR == "Inf") %>% 
    select(RR_descriptor, RR, upper_limit, lower_limit, variable) 
  
  
  
  RR_treatment <- rbind(RR_treatment_c, RR_wp_vs_p)
  
  RR_treatment <<- RR_treatment
  
  
  
  cohen_w_vs_c <- 
  cohen.d(
    (data %>% filter(treatment == "w") %>% pull(value)),
    (data %>% filter(treatment == "c") %>% pull(value)),
    hedges.correction = FALSE
  ) %>% 
    print()
  
  cohen_p_vs_c <- 
    cohen.d(
      (effect %>% filter(treatment == "p") %>% pull(value)),
      (effect %>% filter(treatment == "c") %>% pull(value)),
      hedges.correction = FALSE
    )
  
  cohen_wp_vs_c <- 
    cohen.d(
      (effect %>% filter(treatment == "wp") %>% pull(value)),
      (effect %>% filter(treatment == "c") %>% pull(value)),
      hedges.correction = FALSE
    )
  

}
