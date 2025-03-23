




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

stats(RR_treatment, "RR", variable2)
gg_stats_RR <<- gg_stats
gg_dunn_RR <- gg_dunn +
  labs(y = paste0("RR_", variable1)) +
  geom_hline(yintercept= 0, linetype = "dashed", color = "gray40")
gg_dunn_RR <<- gg_dunn_RR


gg_ttest_RR <- gg_ttest +
  labs(y = paste0("RR_", variable1)) + 
  geom_hline(yintercept= 0, linetype = "dashed", color = "gray40")
gg_ttest_RR <<- gg_ttest_RR

stats(RR_treatment, "delta_RR", variable2)
gg_stats_delta_RR <<- gg_stats


gg_dunn_delta_RR <- gg_dunn +
  labs(y = paste0("delta_RR_", variable1)) + 
  geom_hline(yintercept= 0, linetype = "dashed", color = "gray40")
gg_dunn_delta_RR <<- gg_dunn_delta_RR


gg_ttest_delta_RR <- gg_ttest +
  labs(y = paste0("delta_RR_", variable1)) + 
  geom_hline(yintercept= 0, linetype = "dashed", color = "gray40")
gg_ttest_delta_RR <<- gg_ttest_delta_RR

stats(RR_treatment, "sigma_RR", variable2)
gg_stats_sigma_RR <<- gg_stats


gg_dunn_sigma_RR <- gg_dunn +
  labs(y = paste0("sigma_RR_", variable1)) + 
  geom_hline(yintercept= 0, linetype = "dashed", color = "gray40")
gg_dunn_sigma_RR <<- gg_dunn_sigma_RR


gg_ttest_sigma_RR <- gg_ttest  +
  labs(y = paste0("sigma_RR_", variable1)) +
  geom_hline(yintercept= 0, linetype = "dashed", color = "gray40")
gg_ttest_sigma_RR <<- gg_ttest_sigma_RR



source("code/meta_function/effect_size_wp.R")


effect_size_wp(data, variable1)
gg_RR_wp <<- gg_RR_wp
gg_delta_RR_wp <<- gg_delta_RR_wp
gg_sigma_RR_wp <<- gg_sigma_RR_wp
RR_treatment_wp <<- RR_treatment_wp



source("code/meta_function/stats_wp.R")

stats_wp(RR_treatment_wp, "RR", variable2)

gg_dunn_RR_wp <- gg_dunn + 
  labs(y = paste0("RR_", variable1)) +
  geom_hline(yintercept= 0, linetype = "dashed", color = "gray40")
gg_dunn_RR_wp <<- gg_dunn_RR_wp

gg_ttest_RR_wp <- gg_ttest +
  labs(y = paste0("RR_", variable1)) +
  geom_hline(yintercept= 0, linetype = "dashed", color = "gray40")
gg_ttest_RR_wp <<- gg_ttest_RR_wp



stats_wp(RR_treatment_wp, "delta_RR", variable2)

gg_dunn_delta_RR_wp <- gg_dunn +
  labs(y = paste0("delta_RR_", variable1)) +
  geom_hline(yintercept= 0, linetype = "dashed", color = "gray40")
gg_dunn_delta_RR_wp <<- gg_dunn_delta_RR_wp

gg_ttest_delta_RR_wp <- gg_ttest  +
  labs(y = paste0("delta_RR_", variable1)) +
  geom_hline(yintercept= 0, linetype = "dashed", color = "gray40")
gg_ttest_delta_RR_wp <<- gg_ttest_delta_RR_wp



stats_wp(RR_treatment_wp, "sigma_RR", variable2)

gg_dunn_sigma_RR_wp <- gg_dunn + 
  labs(y = paste0("sigma_RR_", variable1)) +
  geom_hline(yintercept= 0, linetype = "dashed", color = "gray40")
gg_dunn_sigma_RR_wp <<- gg_dunn_sigma_RR_wp


gg_ttest_sigma_RR_wp <- gg_ttest  +
  labs(y = paste0("sigma_RR_", variable1)) +
  geom_hline(yintercept= 0, linetype = "dashed", color = "gray40")
gg_ttest_sigma_RR_wp <<- gg_ttest_sigma_RR_wp





}
