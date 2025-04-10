




meta_function <- function(data, variable1, variable2){
  

  mean_variable1 <- paste0("mean_", variable1)
  sd_variable1 <- paste0("sd_", variable1)
  cv_variable1 <- paste0("cv_", variable1)
  
  data <- data %>% 
    distinct(treatment, plot, sampling, date, .data[[variable1]], .keep_all = TRUE) %>% 
    group_by(treatment, sampling, date) %>% 
    mutate(
      n = n(),
      !!mean_variable1 := mean(.data[[variable1]], na.rm = TRUE),
      !!sd_variable1 := sd(.data[[variable1]], na.rm = TRUE)
    ) %>%
    mutate(
      !!cv_variable1 := .data[[sd_variable1]] / .data[[mean_variable1]]
    ) %>% 
    ungroup() %>% 
    select(treatment, sampling, date, plot, n,
           !!variable1, !!mean_variable1, !!sd_variable1, !!cv_variable1)
    
   
  
  
source("code/meta_function/stats_function.R")
  
stats(data, variable1, variable2)

gg_stats_variable <<- gg_stats
gg_dunn_variable <<- gg_dunn
gg_ttest_variable <<- gg_ttest


stats(data, paste0("cv_", variable1), variable2)
gg_stats_cv <<- gg_stats
gg_dunn_cv <<- gg_dunn
gg_ttest_cv <<- gg_ttest



source("code/meta_function/gg_dynamics.R")
gg_dynamics(data, variable1)
gg_all1n <<- gg_all1n
gg_facet <<- gg_facet



source("code/meta_function/gg_dynamics_cv.R")
gg_dynamics_cv(data, variable1)
gg_dynamics_cv <<- gg_dynamics_cv 


# Log response ratio
source("code/meta_function/RR_DYNAMICS_c.R")

RR_dynamics_c(data, variable1)
gg_RR_variable <<- gg_RR
gg_delta_RR_variable <<- gg_delta_RR
gg_sigma_RR_variable <<- gg_sigma_RR
RR_treatment <<- RR_treatment


source("code/meta_function/RR_DYNAMICS_wp.R")


RR_dynamics_wp(data, variable1)
gg_RR_wp <<- gg_RR_wp
gg_delta_RR_wp <<- gg_delta_RR_wp
gg_sigma_RR_wp <<- gg_sigma_RR_wp
RR_wp_vs_treatment <<-  RR_wp_vs_treatment




}
