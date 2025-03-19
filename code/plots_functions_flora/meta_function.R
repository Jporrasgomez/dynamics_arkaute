




meta_function <- function(data, variable1, variable2){
  
  
source("code/stats_function.R")
  
stats(data, variable1, variable2)
gg_dunn
gg_ttest
  
  

stats(data, paste0("cv_", variable1), variable2)
gg_dunn_cv <<- gg_dunn
gg_ttest_cv <<- gg_ttest


source("code/plots_functions_flora/gg_dynamics.R")
gg_dynamics(data, variable1)
gg_all1n <<- gg_all1n
gg_facet <<- gg_facet


source("code/plots_functions_flora/gg_dynamics_cv.R")
gg_dynamics_cv(data, variable1)
gg_dynamics_cv <<- gg_dynamics_cv 


# Log response ratio
source("code/plots_functions_flora/effect_treatment.R")

effect_size(data, variable1)
gg_RR <<- gg_RR
gg_delta_RR <<- gg_delta_RR
gg_sigma_RR <<- gg_sigma_RR
gg_hedges <<- gg_hedges
gg_perc <<- gg_perc

}