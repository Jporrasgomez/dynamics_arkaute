



effect_size_dynamics <- function(data, variable){
  
  
  data <- data %>% 
    select(treatment, date, sampling, plot, all_of(variable)) %>% 
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
    mutate(
      variable = variable
    ) %>% 
    as.data.frame() %>% 
    mutate(
      date = ymd(date)
    )
  
  
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
    
    
    
    effsize_dynamics_data <- do.call(rbind, list(a,b,c,d)) %>% 
      mutate(
        variable = variable, 
        analysis = paste0("hedge's_g")
      ) %>% 
      filter(eff_value != "NaN") %>% 
    mutate(
      null_effect = ifelse(lower_limit <= 0 & upper_limit >= 0, "YES","NO"),
      scale = (max(abs(upper_limit)) + max(abs(lower_limit)))/100, 
      year = year(date)
    ) %>% 
    select(
      eff_descriptor, sampling, date, year, eff_value, lower_limit, upper_limit, null_effect, 
      scale, variable, analysis
    )
  
  }
  
  effsize_dynamics_data <<- effsize_dynamics_data
  
  
  
  
  
}
