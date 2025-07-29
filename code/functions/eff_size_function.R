


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
  
  
  
  a <- 
    cohen.d(
      (data %>% filter(treatment == "w") %>% pull(value)),
      (data %>% filter(treatment == "c") %>% pull(value)),
      hedges.correction = TRUE
    )
  
  b <- 
    cohen.d(
      (data %>% filter(treatment == "p") %>% pull(value)),
      (data %>% filter(treatment == "c") %>% pull(value)),
      hedges.correction = TRUE
    )
  
  c <- 
    cohen.d(
      (data %>% filter(treatment == "wp") %>% pull(value)),
      (data %>% filter(treatment == "c") %>% pull(value)),
      hedges.correction = TRUE
    )
  
  d <- 
    cohen.d(
      (data %>% filter(treatment == "wp") %>% pull(value)),
      (data %>% filter(treatment == "p") %>% pull(value)),
      hedges.correction = TRUE
    )
  
  
  variable_name <- unique(data$variable)
  
  effsize_data <- data.frame(
    eff_descriptor = c("w_vs_c", "p_vs_c", "wp_vs_c", "wp_vs_p"),
    eff_value  = c(a$estimate, b$estimate, c$estimate, d$estimate),
    upper_limit = c(a$conf.int[2], b$conf.int[2], c$conf.int[2], d$conf.int[2]),
    lower_limit = c(a$conf.int[1], b$conf.int[1], c$conf.int[1], d$conf.int[1]),
    variable    = rep(variable_name, 4),
    analysis    = rep("hedge's_g", 4)
  ) %>% 
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
