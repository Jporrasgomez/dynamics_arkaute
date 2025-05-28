



rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)


pacman::p_load(dplyr,reshape2,tidyverse, lubridate, ggplot2, ggpubr, gridExtra,
               car, ggsignif, dunn.test, rstatix) #Cargamos los paquetes que necesitamos


arkaute <- read.csv("data/arkaute.csv") %>% 
  mutate(
    year = as.factor(year),
    date = ymd(date),
    omw_date = as.factor(omw_date),
    one_month_window = as.factor(one_month_window),
    sampling = as.factor(sampling),
    plot = as.factor(plot),
    treatment = as.factor(treatment))

arkaute_no0 <- arkaute %>% 
  filter(sampling != "0")
  
arkaute_norm <- read.csv("data/arkaute_norm.csv") %>% 
  mutate(
    year = as.factor(year),
    date = ymd(date),
    omw_date = as.factor(omw_date),
    one_month_window = as.factor(one_month_window),
    sampling = as.factor(sampling),
    plot = as.factor(plot),
    treatment = as.factor(treatment))

source("code/palettes_labels.R")

palette <- palette5
labels <- labels3


source("code/meta_function/meta_function.R")
source("code/meta_function/RR_TREATMENT_c.R")
source("code/meta_function/RR_TREATMENT_wp.R")

data_list <- list(arkaute_no0, arkaute_norm)

# Richness, abundance and evenness

variables <- c("richness", "abundance", "Y_zipf", "biomass", "biomass012", "NMDS1", "NMDS2", "PC1", "PC2")
                 # 1         # 2         # 3         # 4          # 5       # 6      # 7      # 8    # 9      
#{i = 9
# j = 1
#meta_function(data_list[[j]], variables[i], "treatment")
#RR_treatment_c(data_list[[j]], variables[i])
#RR_treatment_wp(data_list[[j]], variables[i])
#}
#
#gg_stats_variable
#
## Differences at treatment level
#gg_dunn_variable 
#gg_ttest_variable
#gg_RR_dynamics
#gg_RR_dynamics_wp
#
## Dynamics differences
#gg_all1n
#gg_facet
#gg_delta_RR
#gg_delta_RR_wp 
#
## Coefficient of variation
#gg_stats_cv
#gg_dunn_cv
#gg_ttest_cv  
#gg_dynamics_cv



# Storing results: 

results_list <- list()
results_list_wp <- list()


for(i in seq_along(variables)){
  
  meta_function(arkaute, variables[i], "treatment")
  
  results_list[[i]] <- RR_treatment %>% 
    select(date, RR_descriptor, sampling, variable, delta_RR, se_delta_RR)
  
  results_list_wp[[i]] <- RR_wp_vs_treatment %>% 
    select(date, RR_descriptor, sampling, variable, delta_RR, se_delta_RR)
}

RR_data <- do.call(rbind, results_list)
RR_data_wp <- do.call(rbind, results_list_wp)

RR_whole <- rbind(RR_data, RR_data_wp) %>% 
  mutate(variable = as.factor(variable)) %>% 
  mutate(
    delta_RR = if_else(variable == "Y_zipf", -1 * delta_RR, delta_RR),         ## To make evenness data easily interpretable
    delta_se_RR = if_else(variable == "Y_zipf", -1 * se_delta_RR, se_delta_RR),## To make evenness data easily interpretable
    
    variable = fct_recode(variable,
                          "Richness" = "richness",
                          "Cover" = "abundance",
                          "Evenness" = "Y_zipf",
                          "Biomass*" = "biomass",
                          "Biomass" = "biomass012",
                          "SC1" = "NMDS1",
                          "SC2" = "NMDS2",
                          "CWM1" = "PC1", 
                          "CWM2" = "PC2"),
    
    variable = fct_relevel(variable,
                           "Richness",
                           "Cover",
                           "Evenness",
                           "Biomass*",
                           "Biomass",
                           "SC1",
                           "SC2",
                           "CWM1", 
                           "CWM2"))

 
z = 1.96

{gg_dynamics <- RR_whole %>% 
    filter(RR_descriptor %in% c("w_vs_c", "p_vs_c", "wp_vs_c")) %>% 
    filter(variable %in% c("Richness", "Cover", "Biomass")) %>% 
    filter(variable != "Biomass*") %>% 
ggplot(aes(x = date, y = delta_RR)) + 
  facet_grid(variable ~ RR_descriptor, scales = "free_y",
             labeller = labeller(
               RR_descriptor = as_labeller(labels_RR2), 
             )) +  
  geom_hline(yintercept= 0, linetype = "dashed", color = "#12D08C", linewidth = 0.8) +
  geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40", linewidth = 0.8) +
  geom_errorbar(aes(ymin = delta_RR - z * se_delta_RR,
                    ymax = delta_RR + z * se_delta_RR,
                    color = RR_descriptor), alpha = 0.5, linewidth = 0.8) +
  geom_point(aes(color = RR_descriptor), size = 1.5) + 
    scale_y_continuous(
      breaks = scales::breaks_pretty(n = 3)
   #   labels = function(y) round((exp(y) - 1) * 100, 0)
    )+
  geom_line(aes(color = RR_descriptor), linewidth = 0.8) +
  scale_color_manual(values = palette_RR_CB) +
  labs(y = "Log Response Ratio") +
    theme(
      axis.title.x = element_blank(),  # Ajusta la distancia aquí
      panel.grid = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      text = element_text(size = 16),
      legend.position = "none"
    )
print(gg_dynamics)
ggsave("results/Plots/protofinal/2.dynamics_SIBECOL1.png", plot = gg_dynamics, dpi = 300)
}



{gg_dynamics_wp <- RR_whole %>% 
    filter(RR_descriptor %in% c("wp_vs_p")) %>% 
    filter(!variable == "Biomass") %>% 
ggplot(aes(x = date, y = delta_RR)) + 
  facet_grid(variable ~ RR_descriptor, scales = "free_y",
             labeller = labeller(
               RR_descriptor = as_labeller(labels_RR_wp2), 
             )) +  
  geom_errorbar(aes(ymin = delta_RR - z * se_delta_RR,
                    ymax = delta_RR + z * se_delta_RR,
                    color = RR_descriptor), alpha = 0.5) +
  geom_point(aes(color = RR_descriptor), size = 1.2) + 
  geom_line(aes(color = RR_descriptor)) +
  scale_color_manual(values = palette_RR_wp) +
  geom_hline(yintercept= 0, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
  theme(legend.position = "none")
print(gg_dynamics_wp)
#ggsave("results/Plots/protofinal/3.globalchange_dynamics.png", plot = gg_dynamics_wp, dpi = 300)
}







