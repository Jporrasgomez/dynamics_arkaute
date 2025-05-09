


results_cv <- function(data, variable){
  
  
  
  cv_db <- data %>% 
    select(treatment, plot, sampling, date, .data[[variable]]) %>% 
    distinct(treatment, plot, sampling, date,.data[[variable]]) %>%
    group_by(treatment, sampling, date) %>% 
    mutate(
      n = n(),
      mean = mean(.data[[variable]], na.rm = TRUE),
      sd = sd(.data[[variable]], na.rm = TRUE)
    ) %>%
    mutate(
      cv = sd/mean) %>% 
    rename(value =.data[[variable]]) %>% 
    mutate(variable = variable)
  
  
  RR_cv <- cv_db %>% 
    filter(!treatment == "c") %>% 
    select(date, sampling, treatment, cv)
  
  RR_cv_c  <- cv_db %>% 
    filter(treatment == "c")%>% 
    rename(cv_c = cv,
           treatment_c = treatment) %>% 
    select(treatment_c, sampling, date, cv_c, variable)
  
  RR_cv <- RR_cv %>% 
    left_join(RR_cv_c, by = c("date", "sampling"), relationship = "many-to-many") %>% 
    distinct() %>% 
    select(-treatment_c) %>% 
    filter(!(sampling == "1" & treatment %in% c("p", "wp"))) %>% 
    mutate(treatment  = fct_recode(treatment,
                                   "w_vs_c" = "w",
                                   "p_vs_c" = "p", 
                                   "wp_vs_c" = "wp")) %>% 
    rename(RR_descriptor = treatment) %>% 
    mutate(RR = log(cv / cv_c))
  
  cv_db <<- cv_db
  RR_cv <<- RR_cv
  
  
}