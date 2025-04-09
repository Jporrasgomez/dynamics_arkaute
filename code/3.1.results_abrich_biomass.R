



rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)

source("code/1.first_script.R")
source("code/species_composition_TURNOVER.R")
rm(list = setdiff(ls(), c("ab_rich_dynamics", "biomass_imp", "biomass_imp012",
                          "turnover_db")))

pacman::p_load(dplyr,reshape2,tidyverse, lubridate, ggplot2, ggpubr, gridExtra,
               car, ggsignif, dunn.test, rstatix) #Cargamos los paquetes que necesitamos


source("code/palettes_labels.R")

palette <- palette5
labels <- labels3


source("code/meta_function/meta_function.R")
source("code/meta_function/effect_size_treatment_c.R")
source("code/meta_function/effect_size_treatment_wp.R")

# Richness, abundance and evenness

variables <- c("richness", "abundance", "Y_zipf")
                 # 1         # 2         # 3         
{i = 1
meta_function(ab_rich_dynamics, variables[i], "treatment")
effect_size_treatment_c(ab_rich_dynamics, variables[i])
effect_size_treatment_wp(ab_rich_dynamics, variables[i])
}

gg_stats_variable

# Differences at treatment level
gg_dunn_variable 
gg_ttest_variable
gg_RR
gg_RR_wp

# Dynamics differences
gg_all1n
gg_facet
gg_delta_RR
gg_delta_RR_wp 

# Coefficient of variation
gg_stats_cv
gg_dunn_cv
gg_ttest_cv  
gg_dynamics_cv



# Storing results: 

list <- list()
list_wp <- list()


for(i in 1:3){
  
  meta_function(ab_rich_dynamics, variables[i], "treatment")
  
  list[[i]] <- RR_treatment %>% 
    select(date, treatment, sampling, variable, delta_RR, se_delta_RR)
  
  list_wp[[i]] <- RR_wp_vs_treatment %>% 
    select(date, RR_descriptor, sampling, variable, delta_RR, se_delta_RR)
  
}

RR_abricheven <- do.call(rbind, list)
RR_abricheven_wp <- do.call(rbind, list_wp)



#Biomass

meta_function(biomass_imp012, "biomass", "treatment")
#gg_stats_variable
#gg_dunn_variable 
#gg_ttest_variable

#gg_all1n
#gg_facet
#gg_stats_cv
#gg_dunn_cv
#gg_ttest_cv  
#gg_dynamics_cv
#gg_RR
#gg_delta_RR
#gg_sigma_RR
#gg_RR_wp 
#gg_delta_RR_wp 
#gg_sigma_RR_wp

RR_biomass <- RR_treatment %>% 
  select(date, treatment, sampling, variable, delta_RR, se_delta_RR)

RR_biomass_wp <- RR_wp_vs_treatment %>% 
  select(date, RR_descriptor, sampling, variable, delta_RR, se_delta_RR)



list_turnover <- list()
list_turnover_wp <- list()

turnover <- c("appearance", "disappearance", "total_turnover")

for(i in 1:3){
  
  meta_function(turnover_db, turnover[i], "treatment")
  
  list_turnover[[i]] <- RR_treatment %>% 
    select(date, treatment, sampling, variable, delta_RR, se_delta_RR)
  
  list_turnover_wp[[i]] <- RR_wp_vs_treatment %>% 
    select(date, RR_descriptor, sampling, variable, delta_RR, se_delta_RR)
  
}

RR_turnover <- do.call(rbind, list_turnover)
RR_turnover_wp <- do.call(rbind, list_turnover_wp)




library(forcats)

RR_whole <- rbind(RR_abricheven, RR_biomass) %>% 
  rbind(RR_turnover) %>% 
  mutate(variable = as.factor(variable)) %>% 
  mutate(
    delta_RR = if_else(variable == "Y_zipf", -1 * delta_RR, delta_RR),
    delta_se_RR = if_else(variable == "Y_zipf", -1 * se_delta_RR, se_delta_RR),
    
    variable = fct_recode(variable,
                          "Richness" = "richness",
                          "Cover" = "abundance",
                          "Evenness" = "Y_zipf",
                          "Biomass" = "biomass",
                          "Total Turnover" = "total_turnover",
                          "Turnover (Appearance)" = "appearance",
                          "Turnover (Disappearance)" = "disappearance"),
    variable = fct_relevel(variable,
                           "Richness",
                           "Cover",
                           "Evenness",
                           "Biomass",
                           "Total Turnover",
                           "Turnover (Appearance)",
                           "Turnover (Disappearance)"))
    


RR_whole_wp <- rbind(RR_abricheven_wp, RR_biomass_wp) %>% 
  rbind(RR_turnover_wp) %>%
    mutate(
      delta_RR = if_else(variable == "Y_zipf", -1 * delta_RR, delta_RR),
      delta_se_RR = if_else(variable == "Y_zipf", -1 * se_delta_RR, se_delta_RR),
      variable = fct_recode(variable,
                        "Richness" = "richness",
                        "Cover" = "abundance",
                        "Evenness" = "Y_zipf",
                        "Biomass" = "biomass",
                        "Total Turnover" = "total_turnover",
                        "Turnover (Appearance)" = "appearance",
                        "Turnover (Disappearance)" = "disappearance"
                        ),
      variable = fct_relevel(variable,
                             "Richness",
                             "Cover",
                             "Evenness",
                             "Biomass",
                             "Total Turnover",
                             "Turnover (Appearance)",
                             "Turnover (Disappearance)"))



z = 1.96

ggplot(RR_whole, aes(x = date, y = delta_RR)) + 
  facet_grid(variable ~ treatment, scales = "free_y",
             labeller = labeller(
               treatment = as_labeller(labels_RR), 
               variable = function(x) ifelse(x == "Y_zipf", "RAD coefficient (γ-Zipf)", str_to_title(x))
             )) +  
  geom_errorbar(aes(ymin = delta_RR - z * se_delta_RR,
                    ymax = delta_RR + z * se_delta_RR,
                    color = treatment), alpha = 0.5) +
  geom_point(aes(color = treatment), size = 1.2) + 
  geom_line(aes(color = treatment)) +
  scale_color_manual(values = palette) +
  geom_hline(yintercept= 0, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
  theme(legend.position = "none")



ggplot(RR_whole_wp, aes(x = date, y = delta_RR)) + 
  facet_grid(variable ~ RR_descriptor, scales = "free_y",
             labeller = labeller(
               RR_descriptor = as_labeller(labels_RR_wp), 
               variable = function(x) ifelse(x == "Y_zipf", "RAD coefficient (γ-Zipf)", str_to_title(x))
             )) +  
  geom_errorbar(aes(ymin = delta_RR - z * se_delta_RR,
                    ymax = delta_RR + z * se_delta_RR,
                    color = RR_descriptor), alpha = 0.5) +
  geom_point(aes(color = RR_descriptor), size = 1.2) + 
  geom_line(aes(color = RR_descriptor)) +
  scale_color_manual(values = palette_wp_vs_treatment) +
  geom_hline(yintercept= 0, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
  theme(legend.position = "none")







