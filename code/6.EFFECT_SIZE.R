




rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)


pacman::p_load(dplyr,reshape2,tidyverse, lubridate, ggplot2, ggpubr, gridExtra,
               car, ggsignif, dunn.test, rstatix, ggbreak, effsize) #Cargamos los paquetes que necesitamos

source("code/palettes_labels.R")

palette <- palette_CB
labels <- labels3


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



variables <- c("richness",                         # 1     
               "abundance",                        # 2     
               "Y_zipf",                           # 3     
               "biomass",                          # 4     
               "biomass012",                       # 5     
               "biomass_lm_plot",                  # 6     
               #"NMDS1", "NMDS2", "PC1", "PC2",
               "SLA",                              # 7     
               "LDMC",                             # 8     
               "leafN"                             # 9     
               )

LES_variables <- c("SLA", "LDMC", "leafN")



source("code/functions/eff_size_LRR_function.R")

list_eff <- list()
for (i in seq_along(variables)){
  
  LRR_agg(arkaute_no0, variables[i])
  
  list_eff[[i]] <- effsize_data
  
  rm(effsize_data)
}

effect_size_aggregated <- do.call(rbind, list_eff) 


#i = 8
#effect_size_aggregated %>% 
#  filter(eff_descriptor == "p_vs_c") %>% 
#  filter(variable == variables[i]) %>% 
#  print()

source("code/functions/eff_size_dynamics_LRR_function.R")

list_eff_dyn <- list()
for (i in seq_along(variables)){
  
  LRR_dynamics(arkaute, variables[i])
  
  list_eff_dyn[[i]] <- effsize_dynamics_data
  
  rm(effsize_dynamics_data)
}

effect_size_dynamics <- do.call(rbind, list_eff_dyn)



# Plots #





library(patchwork)


######### ! OJO con la variable de biomasa que usamos.
# 1) biomass: datos de biomasa sin usar regesión lineal para rellenar los vacíos de los muestreos 0, 1 , 2 y 12
# 2) biomass012: datos de biomasa en los que se ha usado una regresión lineal a nivel de especie (mirar script 1.2.lm_biomass012.R)
# 3) biomass_lm_plot : datos de biomassa en los que se ha usado una regresión 
# lineal a nivel de PLOT. 280 puntos para la regresión. Mirar script 5.1.biomass_lm_plot.R


# Variables

limits_variables <- c("richness",
                      "abundance",
                      "Y_zipf",
                      "SLA", 
                      "LDMC", 
                      "leafN",
                      #"biomass"                 # Biomass data without imputation of samplings 0, 1, 2 and 12
                      #"biomass012"              # Biomass data with imputation of samplings 0, 1, 2 and 12 using linear model at species level
                      "biomass_lm_plot"          # Biomass data with imputation of samplings 0, 1, 2 and 12 using linear model at plot level
                      )

labels_variables <- c("richness" = "Richness",            # 1
                      "abundance" = "Cover",              # 2
                      "Y_zipf" = "Evenness",              # 3
                      "SLA" = "SLA",                      # 4
                      "LDMC" = "LDMC",                    # 5
                      "leafN"= "LN",                  # 6
                      #"biomass" = "Biomass"
                      #"biomass012" = "Biomass"
                      "biomass_lm_plot" = "Biomass"      # 7
                      )      


# Choosing the range of variables to be displayed (i to j)
i = 1
j = 7

########### ALL TREATMENTS / CONTROL ###

agg <- effect_size_aggregated
dyn <- effect_size_dynamics

lvls <- limits_variables[i:j]
labs <- unname(labels_variables[lvls])


source("code/functions/gg_aggregated_function.R")
source("code/functions/gg_aggregated_function_2.R")
source("code/functions/gg_aggregated_function_2_wp.R")

pos_dod_c_agg <- position_dodge2(width = 0.3, preserve = "single")
pos_dod_c_dyn <- position_dodge2(width = 12, preserve = "single")

gg_eff_agg_c <- agg %>% 
  filter(eff_descriptor %in% c("p_vs_c", "w_vs_c", "wp_vs_c")) %>% 
  filter(variable %in% limits_variables[i:j]) %>% 
  mutate(eff_descriptor = factor(eff_descriptor,
                                 levels = c("p_vs_c", "w_vs_c", "wp_vs_c"))
  ) %>% 
  ggagg(palette_RR_CB, # using my function
        labels_RR2,
        "grey50",
        position   = pos_dod_c_agg,
        asterisk = 50,
        #caps = pos_dod_c_agg$width,
        limitvar = limits_variables[i:j],
        labelvar = labels_variables[i:j])  




gg_eff_agg_c2 <- agg %>% 
  filter(eff_descriptor %in% c("p_vs_c", "w_vs_c", "wp_vs_c"),
                variable %in% lvls) %>% 
  mutate(
    eff_descriptor = factor(eff_descriptor, levels = c("p_vs_c","w_vs_c","wp_vs_c")),
    variable       = factor(variable, levels = lvls, labels = labs)
  ) %>% 
  ggagg2(
    palette   = palette_RR_CB,
    labels    = labels_RR2,
    colorline = "grey50",
    limitvar  = lvls,
    labelvar  = labels_variables[lvls], 
    breaks_axix_y = 2
  )


source("code/functions/gg_dynamics_function.R")

gg_eff_dynamics_c <- dyn %>% 
  filter(eff_descriptor %in% c("w_vs_c", "p_vs_c", "wp_vs_c")) %>% 
  filter(variable %in% limits_variables[i:j]) %>%  
  mutate(
    variable = factor(variable, 
                      levels = limits_variables[i:j], 
                      labels = labels_variables[i:j])) %>% 
  
  ggdyn(palette_RR_CB,
        labels_RR2, 
        "grey50",
        position = pos_dod_c_dyn,
        asterisk = 8, 
        caps = pos_dod_c_dyn$width) 

library(patchwork)
gg_Experiment_Results <- 
  ggarrange(
    gg_eff_agg_c2   + theme(plot.margin = margin(5,5,5,5)),   # margen uniforme
    gg_eff_dynamics_c + theme(plot.margin = margin(5,5,5,5)),
    #labels   = c("A","B"),
    ncol   = 2, 
    widths = c(1, 4)) +
  plot_annotation(
    #title = "LRR",
    theme = theme( plot.title = element_text(face = "bold", size = 10, hjust = 0.5)))

print(gg_Experiment_Results)
ggsave("results/Plots/protofinal/1.Results_LRR.png", plot = gg_Experiment_Results, dpi = 300)




########### COMBINED / PERTURBATION    ###

pos_dod_wp_agg <- position_dodge2(width = 0.1, preserve = "single")
pos_dod_wp_dyn <- position_dodge2(width = 4, preserve = "single")



gg_eff_agg_wp <- agg %>% 
  filter(eff_descriptor == "wp_vs_p") %>% 
  filter(variable %in% limits_variables[i:j]) %>% 
  ggagg(palette_RR_wp,
        labels_RR_wp,
        p_CB, 
        position   = pos_dod_wp_agg,
        asterisk = 4,
        caps = pos_dod_wp_agg$width,
        limitvar = limits_variables[i:j],
        labelvar = labels_variables[i:j]
        ) 

gg_eff_agg_wp2 <- agg %>% 
  filter(eff_descriptor == "wp_vs_p",
         variable %in% lvls) %>% 
  mutate(
    variable = factor(variable, levels = lvls, labels = labs)
  ) %>% 
  ggagg2_wp(
    palette   = palette_RR_wp,
    labels    = labels_RR_wp,
    p_CB,
    limitvar  = lvls,
    labelvar  = labels_variables[lvls], 
    breaks_axix_y = 2
  )




gg_eff_dynamics_wp <- dyn %>% 
  filter(eff_descriptor %in% c("wp_vs_p")) %>% 
  filter(variable %in% limits_variables[i:j]) %>%  
  mutate(variable = factor(variable, 
                           levels = limits_variables[i:j], 
                           labels = labels_variables[i:j])) %>% 
  ggdyn(palette_RR_wp,
        labels_RR_wp2,
        p_CB,
        position = pos_dod_wp_dyn,
        asterisk = 8, 
        caps = pos_dod_wp_dyn$width)


gg_Warming_Effect <- 
  (gg_eff_agg_wp2   + theme(plot.margin = margin(5,5,5,5))) +
  (gg_eff_dynamics_wp + theme(plot.margin = margin(5,5,5,5))) +
  plot_layout(
    ncol   = 2,
    widths = c(1, 4)) +
  plot_annotation(
    #title = "LRR",
    theme = theme(
      plot.title = element_text(face = "bold", size = 10, hjust = 0.5)))

print(gg_Warming_Effect)
ggsave("results/Plots/protofinal/1.Warming_Effect_LRR.png", plot = gg_Warming_Effect, dpi = 300)




########### COMBINED / WARMING    ###

pos_dod_wpw_agg <- position_dodge2(width = 0.1, preserve = "single")
pos_dod_wpw_dyn <- position_dodge2(width = 4, preserve = "single")



gg_eff_agg_wpw <- agg %>% 
  filter(eff_descriptor == "wp_vs_w") %>% 
  filter(variable %in% limits_variables[i:j]) %>% 
  ggagg(palette_RR_wp,
        labels_RR_wp,
        w_CB, 
        position   = pos_dod_wp_agg,
        asterisk = 4,
        caps = pos_dod_wp_agg$width,
        limitvar = limits_variables[i:j],
        labelvar = labels_variables[i:j])


gg_eff_dynamics_wpw <- dyn %>% 
  filter(eff_descriptor %in% c("wp_vs_w")) %>% 
  filter(variable %in% limits_variables[i:j]) %>%  
  mutate(variable = factor(variable, 
                           levels = limits_variables[i:j], 
                           labels = labels_variables[i:j])) %>% 
  ggdyn(palette_RR_wp,
        labels_RR_wp2,
        w_CB,
        position = pos_dod_wpw_dyn,
        asterisk = 8, 
        caps = pos_dod_wpw_dyn$width)


gg_Perturbation_Effect <- 
  (gg_eff_agg_wpw   + theme(plot.margin = margin(5,5,5,5))) +
  (gg_eff_dynamics_wpw + theme(plot.margin = margin(5,5,5,5))) +
  plot_layout(
    ncol   = 2,
    widths = c(1, 4)) +
  plot_annotation(
    #title = "LRR",
    theme = theme(
      plot.title = element_text(face = "bold", size = 10, hjust = 0.5)))

print(gg_Perturbation_Effect)
#ggsave("results/Plots/protofinal/1.Perturbation_Effect_LRR_smallsize.png", plot = gg_Perturbation_Effect, dpi = 300)



i = 1
j = 7

source("code/functions/gg_agg_PPT.R")

gg_PPT_wp_agg <- 
agg %>% 
  filter(eff_descriptor == "wp_vs_p") %>% 
  filter(variable %in% limits_variables[i:j]) %>% 
  ggagg_ppt(palette_RR_wp,
        labels_RR_wp,
        p_CB, 
        position   = pos_dod_wp_agg,
        caps = pos_dod_wp_agg$width,
        limitvar = limits_variables[i:j],
        labelvar = labels_variables[i:j])
print(gg_PPT_wp_agg)


ggsave("results/Plots/protofinal/Warming_effect_agg_PPT.png", plot = gg_PPT_wp_agg, dpi = 300)





gg_PPT_control_agg <- 
  agg %>% 
  filter(eff_descriptor %in% c("wp_vs_c", "w_vs_c", "p_vs_c")) %>% 
  filter(variable %in% limits_variables[i:j]) %>% 
  ggagg_ppt(palette_RR_CB,
            labels_RR2,
            "grey20", 
            position   = position_dodge2(width = 0.2, preserve = "single"),
            caps = position_dodge2(width = 0.2, preserve = "single")$width,
            limitvar = limits_variables[i:j],
            labelvar = labels_variables[i:j])
print(gg_PPT_control_agg)


ggsave("results/Plots/protofinal/treatment_effect_agg_PPT.png", plot = gg_PPT_control_agg, dpi = 300)


































