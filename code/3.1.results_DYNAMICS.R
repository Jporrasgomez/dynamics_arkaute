



rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)

source("code/1.first_script.R")

#turnover_db <- read.csv("data/turnover_db.csv")
nmds_df_plot <- read.csv("data/nmds_df_plot.csv")

rm(list = setdiff(ls(), c("ab_rich_dynamics", "biomass_imp", "biomass_imp012",
                          "turnover_db", "nmds_df_plot")))

pacman::p_load(dplyr,reshape2,tidyverse, lubridate, ggplot2, ggpubr, gridExtra,
               car, ggsignif, dunn.test, rstatix) #Cargamos los paquetes que necesitamos


source("code/palettes_labels.R")

palette <- palette5
labels <- labels3


source("code/meta_function/meta_function.R")
source("code/meta_function/RR_TREATMENT_c.R")
source("code/meta_function/RR_TREATMENT_wp.R")

# Richness, abundance and evenness

variables <- c("richness", "abundance", "Y_zipf")
                 # 1         # 2         # 3         
{i = 1
meta_function(ab_rich_dynamics, variables[i], "treatment")
RR_treatment_c(ab_rich_dynamics, variables[i])
RR_treatment_wp(ab_rich_dynamics, variables[i])
}

gg_stats_variable

# Differences at treatment level
gg_dunn_variable 
gg_ttest_variable
gg_RR_dynamics
gg_RR_dynamics_wp

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
    select(date, RR_descriptor, sampling, variable, delta_RR, se_delta_RR)
  
  list_wp[[i]] <- RR_wp_vs_treatment %>% 
    select(date, RR_descriptor, sampling, variable, delta_RR, se_delta_RR)
  
}

RR_abricheven <- do.call(rbind, list)
RR_abricheven_wp <- do.call(rbind, list_wp)



#Biomass

{meta_function(biomass_imp012, "biomass", "treatment")

RR_biomass012 <- RR_treatment %>% 
  select(date, RR_descriptor, sampling, variable, delta_RR, se_delta_RR) %>% 
  mutate(
    variable = fct_recode(variable,
                          "biomass012" = "biomass"))

RR_biomass012_wp <- RR_wp_vs_treatment %>% 
  select(date, RR_descriptor, sampling, variable, delta_RR, se_delta_RR) %>% 
  mutate(
    variable = fct_recode(variable,
                          "biomass012" = "biomass"))}


{meta_function(biomass_imp, "biomass", "treatment")
  
  RR_biomass <- RR_treatment %>% 
    select(date, RR_descriptor, sampling, variable, delta_RR, se_delta_RR)
  
  RR_biomass_wp <- RR_wp_vs_treatment %>% 
    select(date, RR_descriptor, sampling, variable, delta_RR, se_delta_RR)}




#list_turnover <- list()
#list_turnover_wp <- list()
#
#turnover <- c("appearance", "disappearance", "total_turnover")
#
#for(i in 1:3){
#  
#  meta_function(turnover_db, turnover[i], "treatment")
#  
#  list_turnover[[i]] <- RR_treatment %>% 
#    select(date, RR_descriptor, sampling, variable, delta_RR, se_delta_RR)
#  
#  list_turnover_wp[[i]] <- RR_wp_vs_treatment %>% 
#    select(date, RR_descriptor, sampling, variable, delta_RR, se_delta_RR)
#  
#}
#
#
#RR_turnover <- do.call(rbind, list_turnover)
#RR_turnover_wp <- do.call(rbind, list_turnover_wp)



{meta_function(nmds_df_plot, "NMDS1", "treatment")
RR_nmds1 <- RR_treatment %>% 
  select(date, RR_descriptor, sampling, variable, delta_RR, se_delta_RR)

RR_nmds1_wp <- RR_wp_vs_treatment %>% 
  select(date, RR_descriptor, sampling, variable, delta_RR, se_delta_RR)}

{meta_function(nmds_df_plot, "NMDS2", "treatment")
  RR_nmds2 <- RR_treatment %>% 
    select(date, RR_descriptor, sampling, variable, delta_RR, se_delta_RR)
  
  RR_nmds2_wp <- RR_wp_vs_treatment %>% 
    select(date, RR_descriptor, sampling, variable, delta_RR, se_delta_RR)}

RR_nmds <- rbind(RR_nmds1, RR_nmds2)
RR_nmds_wp <- rbind(RR_nmds1_wp, RR_nmds2_wp)


library(forcats)

RR_whole <- rbind(RR_abricheven, RR_biomass) %>% 
  rbind(RR_biomass012) %>% 
  #rbind(RR_turnover) %>% 
  rbind(RR_nmds) %>% 
  mutate(variable = as.factor(variable)) %>% 
  mutate(
    delta_RR = if_else(variable == "Y_zipf", -1 * delta_RR, delta_RR),
    delta_se_RR = if_else(variable == "Y_zipf", -1 * se_delta_RR, se_delta_RR),
    
    variable = fct_recode(variable,
                          "Richness" = "richness",
                          "Cover" = "abundance",
                          "Evenness" = "Y_zipf",
                          "Biomass" = "biomass",
                          "Biomass012" = "biomass012",
                          "Sp.comp. (NMDS1)" = "NMDS1",
                          "Sp.comp.(NMDS2)" = "NMDS2"),
    variable = fct_relevel(variable,
                           "Richness",
                           "Cover",
                           "Evenness",
                           "Biomass",
                           "Biomass012",
                           "Sp.comp. (NMDS1)",
                           "Sp.comp.(NMDS2)"))
    


RR_whole_wp <- rbind(RR_abricheven_wp, RR_biomass_wp) %>% 
  rbind(RR_biomass012_wp) %>% 
  #rbind(RR_turnover_wp) %>%
  rbind(RR_nmds_wp) %>% 
  mutate(variable = as.factor(variable)) %>% 
    mutate(
      delta_RR = if_else(variable == "Y_zipf", -1 * delta_RR, delta_RR),
      delta_se_RR = if_else(variable == "Y_zipf", -1 * se_delta_RR, se_delta_RR),
      
      variable = fct_recode(variable,
                            "Richness" = "richness",
                            "Cover" = "abundance",
                            "Evenness" = "Y_zipf",
                            "Biomass" = "biomass",
                            "Biomass012" = "biomass012",
                            "Sp.comp. (NMDS1)" = "NMDS1",
                            "Sp.comp.(NMDS2)" = "NMDS2"),
      variable = fct_relevel(variable,
                             "Richness",
                             "Cover",
                             "Evenness",
                             "Biomass",
                             "Biomass012",
                             "Sp.comp. (NMDS1)",
                             "Sp.comp.(NMDS2)"))

z = 1.96

{gg_dynamics <- 
ggplot(RR_whole, aes(x = date, y = delta_RR)) + 
  facet_grid(variable ~ RR_descriptor, scales = "free_y",
             labeller = labeller(
               RR_descriptor = as_labeller(labels_RR), 
               variable = function(x) ifelse(x == "Y_zipf", "RAD coefficient (γ-Zipf)", str_to_title(x))
             )) +  
  geom_errorbar(aes(ymin = delta_RR - z * se_delta_RR,
                    ymax = delta_RR + z * se_delta_RR,
                    color = RR_descriptor), alpha = 0.5) +
  geom_point(aes(color = RR_descriptor), size = 1.2) + 
  geom_line(aes(color = RR_descriptor)) +
  scale_color_manual(values = palette_RR) +
  geom_hline(yintercept= 0, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
  theme(legend.position = "none")
print(gg_dynamics)
ggsave("results/Plots/protofinal/dynamics.png", plot = gg_dynamics, dpi = 300)}


{gg_dynamics_wp <- 
ggplot(RR_whole_wp, aes(x = date, y = delta_RR)) + 
  facet_grid(variable ~ RR_descriptor, scales = "free_y",
             labeller = labeller(
               RR_descriptor = as_labeller(labels_RR_wp2), 
               variable = function(x) ifelse(x == "Y_zipf", "RAD coefficient (γ-Zipf)", str_to_title(x))
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
ggsave("results/Plots/protofinal/globalchange_dynamics.png", plot = gg_dynamics_wp, dpi = 300)}







