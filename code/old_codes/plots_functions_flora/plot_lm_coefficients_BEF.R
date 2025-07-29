

plot_lm_coefficients_BEF <- function(data){
  
  library(broom)
  library(ggpubr)
  
  data_c <- subset(data, treatment == "c")
  data_w <- subset(data, treatment == "w")
  data_p <- subset(data, treatment == "p")
  data_wp <-subset(data, treatment == "wp")
  
  
  #lm_c 
  
  lm_c <- lm(biomass ~ richness, data = data_c)
  
  lm_c_summary <- summary(lm_c)
  lm_c_tidy <- tidy(lm_c)
  lm_c_glance <- glance(lm_c)
  
  intercept_lm_c <- lm_c_tidy$estimate[1]
  slope_lm_c <- lm_c_tidy$estimate[2]
  r_squared_lm_c <- lm_c_glance$r.squared
  p_value_lm_c <- round(lm_c_tidy$p.value[2], 4)
  n_observations_lm_c <- nrow(data_c)
  
  
  gglm_c <- ggplot(data_c, aes(x = richness, y = biomass)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(title = "lm_c" ,subtitle = paste("y =", round(intercept_lm_c, 2), "+", round(slope_lm_c, 2), "* x\n",
                                           "R^2:", round(r_squared_lm_c, 2), ", p-value:", round(p_value_lm_c, 4), "\n",
                                           "N_obs:", n_observations_lm_c),
         x = "Richness",
         y = "Biomass") +
    theme_minimal()
  
  
  #lm_w 
  
  lm_w <- lm(biomass ~ richness, data = data_w)
  
  lm_w_summary <- summary(lm_w)
  lm_w_tidy <- tidy(lm_w)
  lm_w_glance <- glance(lm_w)
  
  intercept_lm_w <- lm_w_tidy$estimate[1]
  slope_lm_w <- lm_w_tidy$estimate[2]
  r_squared_lm_w <- lm_w_glance$r.squared
  p_value_lm_w <- round(lm_w_tidy$p.value[2], 4)
  n_observations_lm_w <- nrow(data_c)
  
  
  gglm_w <- ggplot(data_w, aes(x = richness, y = biomass)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(title = "lm_w" ,subtitle = paste("y =", round(intercept_lm_w, 2), "+", round(slope_lm_w, 2), "* x\n",
                                           "R^2:", round(r_squared_lm_w, 2), ", p-value:", round(p_value_lm_w, 4), "\n",
                                           "N_obs:", n_observations_lm_w),
         x = "Richness",
         y = "Biomass") +
    theme_minimal()
  
  
  
  #lm_p 
  
  lm_p <- lm(biomass ~ richness, data = data_p)
  
  lm_p_summary <- summary(lm_p)
  lm_p_tidy <- tidy(lm_p)
  lm_p_glance <- glance(lm_p)
  
  intercept_lm_p <- lm_p_tidy$estimate[1]
  slope_lm_p <- lm_p_tidy$estimate[2]
  r_squared_lm_p <- lm_p_glance$r.squared
  p_value_lm_p <- round(lm_p_tidy$p.value[2], 4)
  n_observations_lm_p <- nrow(data_c)
  
  
  gglm_p <- ggplot(data_p, aes(x = richness, y = biomass)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(title = "lm_p" ,subtitle = paste("y =", round(intercept_lm_p, 2), "+", round(slope_lm_p, 2), "* x\n",
                                           "R^2:", round(r_squared_lm_p, 2), ", p-value:", round(p_value_lm_p, 4), "\n",
                                           "N_obs:", n_observations_lm_p),
         x = "Richness",
         y = "Biomass") +
    theme_minimal()
  
  
  
  
  
  #lm_wp 
  
  lm_wp <- lm(biomass ~ richness, data = data_wp)
  
  lm_wp_summary <- summary(lm_wp)
  lm_wp_tidy <- tidy(lm_wp)
  lm_wp_glance <- glance(lm_wp)
  
  intercept_lm_wp <- lm_wp_tidy$estimate[1]
  slope_lm_wp <- lm_wp_tidy$estimate[2]
  r_squared_lm_wp <- lm_wp_glance$r.squared
  p_value_lm_wp <- round(lm_wp_tidy$p.value[2], 4)
  n_observations_lm_wp <- nrow(data_c)
  
  
  gglm_wp <- ggplot(data_wp, aes(x = richness, y = biomass)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(title = "lm_wp" ,subtitle = paste("y =", round(intercept_lm_wp, 2), "+", round(slope_lm_wp, 2), "* x\n",
                                          "R^2:", round(r_squared_lm_wp, 2), ", p-value:", round(p_value_lm_wp, 4), "\n",
                                          "N_obs:", n_observations_lm_wp),
         x = "Richness",
         y = "Biomass") +
    theme_minimal()
  
  
  graph <- ggarrange(
    gglm_c, gglm_w, gglm_p, gglm_wp, 
    ncol = 4, nrow = 1
  )
  
  return(graph)
  
}