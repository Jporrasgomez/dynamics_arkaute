




meta_function <- function(data, variable1, variable2){
  
  
source("code/meta_function/stats_function.R")
  
stats(data, variable1, variable2)

gg_stats_variable <<- gg_stats
gg_dunn_variable <<- gg_dunn
gg_ttest_variable <<- gg_ttest


stats(data, paste0("cv_", variable1), variable2)
gg_stats_cv <<- gg_stats
gg_dunn_cv <<- gg_dunn
gg_ttest_cv <<- gg_ttest


source("code/meta_function/sampling_0.R")
sampling_0(data, variable1, variable2)
gg_dunn_0 <<- gg_dunn_0


source("code/meta_function/gg_dynamics.R")
gg_dynamics(data, variable1)
gg_all1n <<- gg_all1n
gg_facet <<- gg_facet



source("code/meta_function/gg_dynamics_cv.R")
gg_dynamics_cv(data, variable1)
gg_dynamics_cv <<- gg_dynamics_cv 


# Log response ratio
source("code/meta_function/effect_size_c.R")

effect_size(data, variable1)
gg_RR_variable <<- gg_RR
gg_delta_RR_variable <<- gg_delta_RR
gg_sigma_RR_variable <<- gg_sigma_RR
RR_treatment <<- RR_treatment


source("code/meta_function/effect_size_wp.R")


effect_size_wp(data, variable1)
gg_RR_wp <<- gg_RR_wp
gg_delta_RR_wp <<- gg_delta_RR_wp
gg_sigma_RR_wp <<- gg_sigma_RR_wp
RR_treatment_wp <<- RR_treatment_wp




}
