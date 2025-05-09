



rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)

source("code/1.first_script.R")

nmds_df_plot <- read.csv("data/nmds_df_plot.csv") %>% 
  mutate(treatment = as.factor(treatment),
         plot = as.factor(plot),
         sampling = as.factor(sampling),
         date = ymd(date))

cwm_plot_db <- read.csv("data/cwm_plot_db.csv") %>% 
  mutate(treatment = as.factor(treatment),
         plot = as.factor(plot),
         sampling = as.factor(sampling),
         date = ymd(date))


rm(list = setdiff(ls(), c("ab_rich_dynamics", "biomass_imp", "biomass_imp012", "nmds_df_plot",
                          "cwm_plot_db")))

pacman::p_load(dplyr,reshape2,tidyverse, lubridate, ggplot2, ggpubr, gridExtra,
               car, ggsignif, dunn.test, rstatix) #Cargamos los paquetes que necesitamos


source("code/palettes_labels.R")


# Richness, abundance and evenness

source("code/meta_function/cv_results_DYNAMICS.R")




{variables <- c("richness", "abundance", "Y_zipf")
list <- list()
list_RR <- list()
for(i in 1:3){
  
results_cv(ab_rich_dynamics, variables[i])
  list[[i]] <- cv_db
  list_RR[[i]] <- RR_cv
}

abricheven_cv <- do.call(rbind, list)
abricheven_RR_cv <- do.call(rbind, list_RR)
}


#Biomass

{results_cv(biomass_imp, "biomass")
biomass_cv <- cv_db
biomass_RR_cv <- RR_cv

results_cv(biomass_imp012, "biomass")
biomass012_cv <- cv_db %>% 
  mutate(
  variable = fct_recode(variable,
                        "biomass012" = "biomass"))
biomass012_RR_cv <- RR_cv %>% 
  mutate(
    variable = fct_recode(variable,
                          "biomass012" = "biomass"))}
  
  
  # Sp composition
  
{variables <- c("NMDS1", "NMDS2")
  list <- list()
  list_RR <- list()
  
  for(i in 1:2){
    results_cv(nmds_df_plot, variables[i])
    list[[i]] <- cv_db
    list_RR[[i]] <- RR_cv
  }
    
  
sp_comp_cv <- do.call(rbind, list)
sp_comp_RR <- do.call(rbind, list_RR)
    }
  
  
  
{variables <- c("LA", "LDMC", "leafN", "seed.mass", "SLA", "vegetation.height")
    list <- list()
    list_RR <- list()
    for(i in 1:6){
      results_cv(cwm_plot_db, variables[i])
      list[[i]] <- cv_db
      list_RR[[i]] <- RR_cv

    }
    
    traits_cv <- do.call(rbind, list)
    traits_RR <- do.call(rbind, list_RR)
  }
  


library(forcats)

data_whole <- rbind(abricheven_cv, biomass_cv) %>% 
  rbind(biomass012_cv) %>% 
  rbind(sp_comp_cv) %>% 
  rbind(traits_cv) %>% 
  select(-value, -plot) %>% 
  distinct() 
# Dynamics

{gg_cv_dynamics <- 
data_whole %>%
  filter(variable %in% c("richness", "abundance", "Y_zipf", "biomass", "biomass012")) %>% 
  mutate(
    variable = fct_recode(variable,
                          "Richness" = "richness",
                          "Cover" = "abundance",
                          "Evenness" = "Y_zipf",
                          "Biomass" = "biomass",
                          "Biomass012" = "biomass012"), 
    
    variable = fct_relevel(variable,
                           "Richness",
                           "Cover",
                           "Evenness",
                           "Biomass",
                           "Biomass012")) %>% 
ggplot(aes(x = date, y = cv)) + 
  facet_grid(variable ~ treatment, scales = "free_y", 
             labeller = labeller(
               treatment = as_labeller(labels3))) + 
  geom_smooth(
    se = TRUE, aes(color = treatment, fill = treatment),
    method = "lm", span = 0.6, alpha = 0.2 ) +
  geom_point(aes(color = treatment), size = 1.2) + 
  geom_line(aes(color = treatment), alpha = 0.2) +
  scale_color_manual(values = palette5) +
  scale_fill_manual(values = palette5) +
  geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
  theme(legend.position = "none") + 
  labs( y = "Coefficient of variation", x = "Date")
print(gg_cv_dynamics)
#ggsave("results/Plots/protofinal/cv_dynamics.png", plot = gg_cv_dynamics, dpi = 300)
}
  

{gg_cv_dynamics2 <- 
data_whole %>%
  filter(variable %in% c("NMDS1", "NMDS2", "LA", "SLA", "LDMC", "vegetation.height", "seed.mass", 
                         "leafN")) %>% 
  mutate(
    variable = fct_recode(variable,
                          "Sp.Comp.(NMDS1)" = "NMDS1",
                          "Sp.Comp.(NMDS2)" = "NMDS2",
                          "LA" = "LA",
                          "SLA" = "SLA",
                          "LDMC" = "LDMC",
                          "Height" = "vegetation.height",
                          "Seed mass" = "seed.mass", 
                          "Leaf N" = "leafN"), 
    
    variable = fct_relevel(variable,
                           "Sp.Comp.(NMDS1)",
                           "Sp.Comp.(NMDS2)",
                           "LA",
                           "SLA",
                           "LDMC",
                           "Height",
                           "Seed mass", 
                           "Leaf N")) %>% 
  ggplot(aes(x = date, y = cv)) + 
  facet_grid(variable ~ treatment, scales = "free_y", 
             labeller = labeller(
               treatment = as_labeller(labels3))) + 
  geom_smooth(
    se = TRUE, aes(color = treatment, fill = treatment),
    method = "lm", span = 0.6, alpha = 0.2 ) +
  geom_point(aes(color = treatment), size = 1.2) + 
  geom_line(aes(color = treatment), alpha = 0.2, group  = 1) +
  scale_color_manual(values = palette5) +
  scale_fill_manual(values = palette5) +
  geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
  theme(legend.position = "none") + 
  labs( y = "Coefficient of variation", x = "Date")
print(gg_cv_dynamics2)
#ggsave("results/Plots/protofinal/cv_dynamics2.png", plot = gg_cv_dynamics2, dpi = 300)
}



# Response ratio


data_whole_RR <-  rbind(abricheven_RR_cv, biomass_RR_cv) %>% 
  rbind(biomass012_RR_cv) %>% 
  rbind(sp_comp_RR) %>% 
  rbind(traits_RR) %>% 
  select(-cv, -cv_c, -cv_wp)

{gg_RR_cv_dynamics <- 
data_whole_RR %>% 
  filter(RR_descriptor %in% c("w_vs_c", "p_vs_c", "wp_vs_c")) %>% 
  filter(variable %in% c("richness", "abundance", "Y_zipf", "biomass", "biomass012")) %>% 
  mutate(
    variable = fct_recode(variable,
                          "Richness" = "richness",
                          "Cover" = "abundance",
                          "Evenness" = "Y_zipf",
                          "Biomass" = "biomass",
                          "Biomass012" = "biomass012"), 
    
    variable = fct_relevel(variable,
                           "Richness",
                           "Cover",
                           "Evenness",
                           "Biomass",
                           "Biomass012")) %>% 

ggplot(aes(x = date, y = RR)) + 
  facet_grid(variable ~ RR_descriptor, scales = "free_y", 
             labeller = labeller(
               RR_descriptor = as_labeller(labels_RR))) + 
  geom_smooth(
    se = TRUE, aes(color = RR_descriptor, fill = RR_descriptor),
    method = "lm", span = 0.6, alpha = 0.2 ) +
  geom_point(aes(color = RR_descriptor), size = 1.2) + 
  geom_line(aes(color = RR_descriptor), alpha = 0.2) +
  scale_color_manual(values = palette_RR) +
  scale_fill_manual(values = palette_RR) +
  geom_hline(yintercept= 0, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
  theme(legend.position = "none") + 
  labs( y = "RR_cv", x = "Date")
print(gg_RR_cv_dynamics)
#ggsave("results/Plots/protofinal/RR_cv_dynamics.png", plot = gg_RR_cv_dynamics, dpi = 300)
}



{gg_RR_cv_dynamics2 <-
data_whole_RR %>% 
  filter(RR_descriptor %in% c("w_vs_c", "p_vs_c", "wp_vs_c")) %>% 
  filter(variable %in% c("NMDS1", "NMDS2", "LA", "SLA", "LDMC", "vegetation.height", "seed.mass", 
                         "leafN")) %>% 
  mutate(
    variable = fct_recode(variable,
                          "Sp.Comp.(NMDS1)" = "NMDS1",
                          "Sp.Comp.(NMDS2)" = "NMDS2",
                          "LA" = "LA",
                          "SLA" = "SLA",
                          "LDMC" = "LDMC",
                          "Height" = "vegetation.height",
                          "Seed mass" = "seed.mass", 
                          "Leaf N" = "leafN"), 
    
    variable = fct_relevel(variable,
                           "Sp.Comp.(NMDS1)",
                           "Sp.Comp.(NMDS2)",
                           "LA",
                           "SLA",
                           "LDMC",
                           "Height",
                           "Seed mass", 
                           "Leaf N")) %>% 
  
  ggplot(aes(x = date, y = RR)) + 
  facet_grid(variable ~ RR_descriptor, scales = "free_y", 
             labeller = labeller(
               RR_descriptor = as_labeller(labels_RR))) + 
  geom_smooth(
    se = TRUE, aes(color = RR_descriptor, fill = RR_descriptor),
    method = "lm", span = 0.6, alpha = 0.2 ) +
  geom_point(aes(color = RR_descriptor), size = 1.2) + 
  geom_line(aes(color = RR_descriptor), alpha = 0.2) +
  scale_color_manual(values = palette_RR) +
  scale_fill_manual(values = palette_RR) +
  geom_hline(yintercept= 0, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
  theme(legend.position = "none") + 
  labs( y = "RR_cv", x = "Date")
print(gg_RR_cv_dynamics2)
#ggsave("results/Plots/protofinal/RR_cv_dynamics2.png", plot = gg_RR_cv_dynamics2, dpi = 300)
}







{gg_RR_cv_dynamics_GC <- 
    data_whole_RR %>% 
    filter(RR_descriptor %in% c("wp_vs_p", "wp_vs_w")) %>% 
    filter(variable %in% c("richness", "abundance", "Y_zipf", "biomass", "biomass012")) %>% 
    mutate(
      variable = fct_recode(variable,
                            "Richness" = "richness",
                            "Cover" = "abundance",
                            "Evenness" = "Y_zipf",
                            "Biomass" = "biomass",
                            "Biomass012" = "biomass012"), 
      
      variable = fct_relevel(variable,
                             "Richness",
                             "Cover",
                             "Evenness",
                             "Biomass",
                             "Biomass012")) %>% 
    
    ggplot(aes(x = date, y = RR)) + 
    facet_grid(variable ~ RR_descriptor, scales = "free_y", 
               labeller = labeller(
                 RR_descriptor = as_labeller(labels_RR_wp2))) + 
    geom_smooth(
      se = TRUE, aes(color = RR_descriptor, fill = RR_descriptor),
      method = "lm", span = 0.6, alpha = 0.2 ) +
    geom_point(aes(color = RR_descriptor), size = 1.2) + 
    geom_line(aes(color = RR_descriptor), alpha = 0.2) +
    scale_color_manual(values = palette_RR_wp) +
    scale_fill_manual(values = palette_RR_wp) +
    geom_hline(yintercept= 0, linetype = "dashed", color = "gray40") +
    geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
    theme(legend.position = "none") + 
    labs( y = "RR_cv", x = "Date")
  print(gg_RR_cv_dynamics_GC)
  ggsave("results/Plots/protofinal/gg_RR_cv_dynamics_GC.png", plot = gg_RR_cv_dynamics_GC, dpi = 300)
  }



{gg_RR_cv_dynamics_GC2 <-
    data_whole_RR %>% 
    filter(RR_descriptor %in% c("wp_vs_p", "wp_vs_w")) %>%
    filter(variable %in% c("NMDS1", "NMDS2", "LA", "SLA", "LDMC", "vegetation.height", "seed.mass", 
                           "leafN")) %>% 
    mutate(
      variable = fct_recode(variable,
                            "Sp.Comp.(NMDS1)" = "NMDS1",
                            "Sp.Comp.(NMDS2)" = "NMDS2",
                            "LA" = "LA",
                            "SLA" = "SLA",
                            "LDMC" = "LDMC",
                            "Height" = "vegetation.height",
                            "Seed mass" = "seed.mass", 
                            "Leaf N" = "leafN"), 
      
      variable = fct_relevel(variable,
                             "Sp.Comp.(NMDS1)",
                             "Sp.Comp.(NMDS2)",
                             "LA",
                             "SLA",
                             "LDMC",
                             "Height",
                             "Seed mass", 
                             "Leaf N")) %>% 
    
    ggplot(aes(x = date, y = RR)) + 
    facet_grid(variable ~ RR_descriptor, scales = "free_y", 
               labeller = labeller(
                 RR_descriptor = as_labeller(labels_RR_wp2))) + 
    geom_smooth(
      se = TRUE, aes(color = RR_descriptor, fill = RR_descriptor),
      method = "lm", span = 0.6, alpha = 0.2 ) +
    geom_point(aes(color = RR_descriptor), size = 1.2) + 
    geom_line(aes(color = RR_descriptor), alpha = 0.2) +
    scale_color_manual(values = palette_RR_wp) +
    scale_fill_manual(values = palette_RR_wp) +
    geom_hline(yintercept= 0, linetype = "dashed", color = "gray40") +
    geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
    theme(legend.position = "none") + 
    labs( y = "RR_cv", x = "Date")
  print(gg_RR_cv_dynamics_GC2)
  ggsave("results/Plots/protofinal/gg_RR_cv_dynamics_GC2.png", plot = gg_RR_cv_dynamics_GC2, dpi = 300)
}





