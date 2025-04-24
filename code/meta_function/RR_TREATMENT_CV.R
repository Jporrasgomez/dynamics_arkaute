




RR_treatment_cv <- function(data, variable){

  
  data <- data %>% 
    distinct(treatment, plot, sampling, date, .data[[variable]], .keep_all = TRUE) %>% 
    group_by(treatment, sampling, date) %>% 
    mutate(
      mean = mean(.data[[variable]], na.rm = TRUE),
      sd = sd(.data[[variable]], na.rm = TRUE)
    ) %>%
    select(treatment, sampling, date, mean, sd) %>% 
    distinct() %>% 
    mutate(
      cv = sd/mean
    ) %>% 
    ungroup() %>% 
    select(treatment, sampling, date, mean, sd, cv) %>% 
    mutate(variable = variable) %>% 
    filter(!sd == 0) %>% 
    group_by(treatment) %>% 
    summarize(
      mean_cv = mean(cv), 
      sd_cv = sd(cv), 
      n = n()
    ) %>% 
    mutate(variable = variable)
  

  
  rr_cv_c <- data %>% 
    filter(treatment == "c") %>% 
    rename(mean_cv_c = mean_cv, 
           sd_cv_c = sd_cv,
           n_c = n) %>% 
    select(-variable, -treatment)
  
  rr_cv <- data %>% 
    filter(!treatment == "c") %>% 
    cbind(rr_cv_c) %>% 
    mutate(
      RR =  log(mean_cv / mean_cv_c),
      
      # Cálculo de la varianza de RR
      se_RR =  sqrt((sd_cv^2) / (n * mean_cv^2) + 
                      (sd_cv_c^2) / (n_c * mean_cv_c^2))
    ) %>% 
    rename(RR_descriptor = treatment) %>% 
    mutate(
      RR_descriptor = fct_recode(RR_descriptor,
                                 "w_vs_c" = "w",
                                 "p_vs_c" = "p", 
                                 "wp_vs_c" = "wp")) %>% 
    select(RR_descriptor, RR, se_RR, variable, n)
  
  
  
  rr_cv_p <- data %>% 
    filter(treatment == "p") %>% 
    rename(mean_cv_p = mean_cv, 
           sd_cv_p = sd_cv,
           n_p = n) %>% 
    select(-variable, -treatment)
  
  
  rr_cv_wp_vs_p <- data %>% 
    filter(treatment == "wp") %>% 
    rename(mean_cv_wp = mean_cv, 
           sd_cv_wp = sd_cv,
           n_wp = n) %>% 
    cbind(rr_cv_p) %>% 
    mutate(
      RR =  log(mean_cv_wp / mean_cv_p),
      
      # Cálculo de la varianza de RR
      se_RR =  sqrt((sd_cv_wp^2) / (n_wp * mean_cv_wp^2) + 
                      (sd_cv_p^2) / (n_p * mean_cv_p^2))
    ) %>% 
    rename(RR_descriptor = treatment) %>% 
    mutate(
      RR_descriptor = fct_recode(RR_descriptor,
                                 "wp_vs_p" = "wp")) %>% 
    select(RR_descriptor, RR, se_RR, variable)
    
  

  
  rr_cv_w <- data %>% 
    filter(treatment == "w") %>% 
    rename(mean_cv_w = mean_cv, 
           sd_cv_w = sd_cv,
           n_w = n) %>% 
    select(-variable, -treatment)
  
  rr_cv_wp_vs_w <- data %>% 
    filter(treatment == "wp") %>% 
    rename(mean_cv_wp = mean_cv, 
           sd_cv_wp = sd_cv,
           n_wp = n) %>% 
    cbind(rr_cv_w) %>% 
    mutate(
      RR =  log(mean_cv_wp / mean_cv_w),
      
      # Cálculo de la varianza de RR
      se_RR =  sqrt((sd_cv_wp^2) / (n_wp * mean_cv_wp^2) + 
                      (sd_cv_w^2) / (n_w * mean_cv_w^2))
    ) %>% 
    rename(RR_descriptor = treatment) %>% 
    mutate(
      RR_descriptor = fct_recode(RR_descriptor,
                                 "wp_vs_w" = "wp")) %>% 
    select(RR_descriptor, RR, se_RR, variable)
  
  
RR_cv <- rbind(rr_cv, rr_cv_wp_vs_p)
RR_cv <- rbind(RR_cv, rr_cv_wp_vs_w)  


RR_cv <<- RR_cv  
  
} 
    