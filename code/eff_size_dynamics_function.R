


effect_size_dynamics <- function(data, variable){
  
  
  data <- data %>% 
    select(treatment, date, sampling, plot, .data[[variable]]) %>% 
    filter(!is.na(.data[[variable]])) %>% 
    rename(
      value = all_of(variable)
    ) %>% 
    group_by(treatment, sampling) %>% 
    mutate(
      n = n(),
      mean := mean(value),
      sd := sd(value)
    ) %>% 
    as.data.frame() %>% 
    mutate(
      date = ymd(date)
    )
  
  
  RR_data <- NULL
  
  if (!variable %in% c("NMDS1", "NMDS2", "PC1", "PC2")) {
  
  ## Log Response Ratio ## 
  
  RR_effect <- data %>% 
    select(date, sampling, treatment, mean, sd) %>% 
    distinct()
  
  RR_effect_c <- RR_effect %>% 
    filter(treatment == "c") %>% 
    rename(mean_c = mean,
           sd_c = sd) %>% 
    ungroup() %>% 
    select(-treatment)
  
  RR_effect_wp <- RR_effect %>% 
    filter(treatment == "wp") %>% 
    rename(mean_wp = mean,
           sd_wp = sd) %>% 
    ungroup() %>% 
    select(-treatment)
  
  RR_effect_w <- RR_effect %>% 
    filter(treatment == "w") %>% 
    rename(mean_w = mean,
           sd_w = sd) %>% 
    ungroup() %>% 
    select(-treatment)
  
  RR_effect_p <- RR_effect %>% 
    filter(treatment == "p") %>% 
    rename(mean_p = mean,
           sd_p = sd) %>% 
    ungroup() %>% 
    select(-treatment)
  
  
  
  n = 4
  
  RR_treatment_c <- RR_effect %>% 
    filter(treatment != "c") %>% 
    left_join(RR_effect_c, by = c("date", "sampling")
              ) %>% 
    mutate(
      RR = log(mean / mean_c),
      var_RR = (sd^2) / (n * mean^2) + 
        (sd_c^2) / (n * mean_c^2),
      se_RR = sqrt(var_RR)  
    ) %>% 
    mutate(
      delta_RR = RR + 0.5 * (
        (sd^2) / (n * mean^2) - 
          (sd_c^2) / (n * mean_c^2)
      ),
      var_delta_RR = var_RR + 0.5 * (
        (sd^4) / (n^2 * mean^4) + 
          (sd_c^4) / (n^2 * mean_c^4)
      ),
      se_delta_RR = sqrt(var_delta_RR)
    )  %>% 
    filter(! RR == "-Inf") %>% 
    rename(eff_descriptor = treatment) %>% 
    mutate(
      eff_descriptor = fct_recode(eff_descriptor,
                                 "w_vs_c" = "w",
                                 "p_vs_c" = "p", 
                                 "wp_vs_c" = "wp")
    ) %>% 
    select(eff_descriptor, date, sampling, delta_RR, se_delta_RR)
  
  
  
  RR_wp_vs_p <- RR_effect_wp %>% 
    left_join(
      RR_effect_p, by = c("date", "sampling")
      ) %>% 
    mutate(
      RR = log(mean_wp / mean_p),
      
      var_RR = (sd_wp^2) / (n * mean_wp^2) + 
        (sd_p^2) / (n * mean_p^2),
      
      se_RR = sqrt(var_RR)
      
    ) %>% 
    
    mutate(
      delta_RR= RR + 0.5 * (
        (sd_wp^2) / (n * mean_wp^2) - 
          (sd_p^2) / (n * mean_p^2)),
      
      var_delta_RR = var_RR + 0.5 * (
        (sd_wp^4) / (n^2 * mean_wp^4) + 
          (sd_p^4) / (n^2 * mean_p^4)),
      
      se_delta_RR = sqrt(var_delta_RR)
      
    ) %>% 
    filter(!RR == "Inf")%>% 
    filter(!RR == "-Inf") %>% 
    filter(!RR == "NaN") %>% 
    mutate(
      eff_descriptor = paste0("wp_vs_p")
    )  %>% 
    select(eff_descriptor, date, sampling, delta_RR, se_delta_RR)
  
  
  
  RR_data <- rbind(RR_treatment_c, RR_wp_vs_p) %>% 
    mutate(
      upper_limit = delta_RR + se_delta_RR * 1.96, 
      lower_limit = delta_RR - se_delta_RR * 1.96,
      variable = variable,
      analysis = paste0("LRR")
    ) %>% 
    rename(
      eff_value = delta_RR
    ) %>% 
    
    select(eff_descriptor, sampling, date, eff_value, lower_limit, 
           upper_limit, variable, analysis)
    
  }
  
  
  
  ## Cohen with Hedges'g correction for small sample size ## 

  
  samps <- unique(data$sampling)
  
  {
  a <- data.frame(
    eff_descriptor = rep(NA_character_, length(samps)),
    sampling       = rep(NA_character_, length(samps)),
    date           = rep(as.Date(NA),       length(samps)),  # Date NA
    eff_value      = rep(NA_real_,      length(samps)),
    lower_limit    = rep(NA_real_,      length(samps)),
    upper_limit    = rep(NA_real_,      length(samps))
  )
  
  counter = 0
  for(i in seq_along(samps)){
    
    z <- 
      cohen.d(
        (data %>% filter(treatment == "w" & sampling == samps[i]) %>% pull(value)),
        (data %>% filter(treatment == "c" & sampling == samps[i]) %>% pull(value)),
        hedges.correction = TRUE
      )
   counter = counter + 1 
   
    a$eff_descriptor[counter] <- paste0("w_vs_c")
    a$sampling[counter] <- paste0(samps[i])
    a$date[counter] <- subset(data, sampling == samps[i])$date[1]
    a$eff_value[counter] <- z$estimate
    a$lower_limit[counter] <- z$conf.int[1]
    a$upper_limit[counter] <- z$conf.int[2]
    
  } 

  
  
  b <- data.frame(
    eff_descriptor = rep(NA_character_, length(samps)),
    sampling       = rep(NA_character_, length(samps)),
    date           = rep(as.Date(NA),       length(samps)),  # Date NA
    eff_value      = rep(NA_real_,      length(samps)),
    lower_limit    = rep(NA_real_,      length(samps)),
    upper_limit    = rep(NA_real_,      length(samps))
  )
  
  counter = 0
  for(i in seq_along(samps)){
    
    z <- 
      cohen.d(
        (data %>% filter(treatment == "p" & sampling == samps[i]) %>% pull(value)),
        (data %>% filter(treatment == "c" & sampling == samps[i]) %>% pull(value)),
        hedges.correction = TRUE
      )
    counter = counter + 1 
    
    b$eff_descriptor[counter] <- paste0("p_vs_c")
    b$sampling[counter] <- paste0(samps[i])
    b$date[counter] <- subset(data, sampling == samps[i])$date[1]
    b$eff_value[counter] <- z$estimate
    b$lower_limit[counter] <- z$conf.int[1]
    b$upper_limit[counter] <- z$conf.int[2]
    
  } 
  
  
  c <- data.frame(
    eff_descriptor = rep(NA_character_, length(samps)),
    sampling       = rep(NA_character_, length(samps)),
    date           = rep(as.Date(NA),       length(samps)),  # Date NA
    eff_value      = rep(NA_real_,      length(samps)),
    lower_limit    = rep(NA_real_,      length(samps)),
    upper_limit    = rep(NA_real_,      length(samps))
  )
  
  counter = 0
  for(i in seq_along(samps)){
    
    z <- 
      cohen.d(
        (data %>% filter(treatment == "wp" & sampling == samps[i]) %>% pull(value)),
        (data %>% filter(treatment == "c" & sampling == samps[i]) %>% pull(value)),
        hedges.correction = TRUE
      )
    counter = counter + 1 
    
    c$eff_descriptor[counter] <- paste0("wp_vs_c")
    c$sampling[counter] <- paste0(samps[i])
    c$date[counter] <- subset(data, sampling == samps[i])$date[1]
    c$eff_value[counter] <- z$estimate
    c$lower_limit[counter] <- z$conf.int[1]
    c$upper_limit[counter] <- z$conf.int[2]
    
  } 
  
  
  d <- data.frame(
    eff_descriptor = rep(NA_character_, length(samps)),
    sampling       = rep(NA_character_, length(samps)),
    date           = rep(as.Date(NA),       length(samps)),  # Date NA
    eff_value      = rep(NA_real_,      length(samps)),
    lower_limit    = rep(NA_real_,      length(samps)),
    upper_limit    = rep(NA_real_,      length(samps))
  )
  
  counter = 0
  for(i in seq_along(samps)){
    
    z <- 
      cohen.d(
        (data %>% filter(treatment == "wp" & sampling == samps[i]) %>% pull(value)),
        (data %>% filter(treatment == "p" & sampling == samps[i]) %>% pull(value)),
        hedges.correction = TRUE
      )
    counter = counter + 1 
    
    d$eff_descriptor[counter] <- paste0("wp_vs_p")
    d$sampling[counter] <- paste0(samps[i])
    d$date[counter] <- subset(data, sampling == samps[i])$date[1]
    d$eff_value[counter] <- z$estimate
    d$lower_limit[counter] <- z$conf.int[1]
    d$upper_limit[counter] <- z$conf.int[2]
    
  } 
  
  
  hedges_data <- do.call(rbind, list(a,b,c,d)) %>% 
    mutate(
      variable = variable, 
      analysis = paste0("hedge's_g")
    ) %>% 
    filter(!eff_value == "NaN")
  }
  
  
  
  
  if (!is.null(RR_data)) {
    effsize_dynamics_data <- bind_rows(RR_data, hedges_data)
  } else {
    effsize_dynamics_data <- hedges_data
  }
  
  effsize_dynamics_data <- effsize_dynamics_data %>% 
    mutate(
      null_effect = ifelse(lower_limit <= 0 & upper_limit >= 0, "YES","NO")) %>% 
    select(
      eff_descriptor, sampling, date, eff_value, lower_limit, upper_limit, null_effect, 
      variable, analysis
    )
  
  
  
  effsize_dynamics_data <<- effsize_dynamics_data
  
  
  
  
  
}
  