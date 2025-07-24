

data <- arkaute
variable <- "richness"

RR_dynamics_c <- function(data, variable){
  
  
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
    ) 
  
  
  
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
      )%>% 
    mutate(
      variable = variable
    )  %>% 
    filter(! RR == "-Inf") %>% 
    rename(eff_descriptor = treatment) %>% 
    mutate(
      eff_descriptor = fct_recode(eff_descriptor,
                                 "w_vs_c" = "w",
                                 "p_vs_c" = "p", 
                                 "wp_vs_c" = "wp")
    ) %>% 
    select(eff_descriptor, date, sampling, delta_RR, se_delta_RR, variable)
  
  
  
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
      variable = variable,
      eff_descriptor = paste0("wp_vs_p")
    )  %>% 
    select(eff_descriptor, date, sampling, delta_RR, se_delta_RR, variable)
  
  
  
  RR_data <- rbind(RR_treatment_c, RR_wp_vs_p) %>% 
    mutate(
      upper_limit = delta_RR + se_delta_RR * 1.96, 
      lower_limit = delta_RR - se_delta_RR * 1.96,
      analysis = paste0("LRR")
    ) %>% 
    
    select(eff_descriptor, date, sampling, delta_RR, lower_limit, 
           upper_limit, variable, analysis)
    
  
  
  ## Cohen with Hedges'g correction for small sample size ## 
  
  ## Cohen's D analysis ##
  
  samps <- unique(data$sampling)
  
  
  a <- data.frame(
    eff_descriptor = c(rep(NA, length(samps))),
    sampling = c(rep(NA, length(samps))),
    date = c(rep(NA, length(samps))), 
    eff_value  = c(rep(NA, length(samps))),
    lower_limit = c(rep(NA, length(samps))),
    upper_limit = c(rep(NA, length(samps))))
  
  counter = 0
  for(i in seq_along(samps)){
    
    date_value <- data %>% filter(sampling == samps[i]) %>% ungroup() %>% select(date) %>% distinct()
    
    z <- 
      cohen.d(
        (data %>% filter(treatment == "w" & sampling == samps[i]) %>% pull(value)),
        (data %>% filter(treatment == "c" & sampling == samps[i]) %>% pull(value)),
        hedges.coRR_effection = TRUE
      )
   counter = counter + 1 
   
    a$eff_descriptor[counter] <- paste0("w_vs_c")
    a$sampling[counter] <- paste0(samps[i])
    a$date[counter] <- lubridate::date(date_value$date) ################### no funciona date
    a$eff_value[counter] <- z$estimate
    a$lower_limit[counter] <- z$conf.int[1]
    a$upper_limit[counter] <- z$conf.int[2]
    
  } 
  ##########################seguir aqui
  
  b <- 
    cohen.d(
      (data %>% filter(treatment == "p") %>% pull(value)),
      (data %>% filter(treatment == "c") %>% pull(value)),
      hedges.coRR_effection = FALSE
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
      hedges.coRR_effection = FALSE
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
 
  
}
  