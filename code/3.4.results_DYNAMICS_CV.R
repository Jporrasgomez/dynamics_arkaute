



rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)

source("code/1.first_script.R")


rm(list = setdiff(ls(), c("ab_rich_dynamics", "biomass_imp", "biomass_imp012")))

pacman::p_load(dplyr,reshape2,tidyverse, lubridate, ggplot2, ggpubr, gridExtra,
               car, ggsignif, dunn.test, rstatix) #Cargamos los paquetes que necesitamos


source("code/palettes_labels.R")
source("code/meta_function/meta_function.R")


# Richness, abundance and evenness

variables <- c("richness", "abundance", "Y_zipf")

list <- list()

for(i in 1:3){
  
  meta_function(ab_rich_dynamics, variables[i], "treatment")
  
  list[[i]] <- data_meta %>% 
    select(date, sampling, treatment, mean, sd, cv, variable) %>% 
    distinct()
  
}

abricheven <- do.call(rbind, list)


#Biomass

{meta_function(biomass_imp012, "biomass", "treatment")
  
  biomass012 <- data_meta %>% 
    select(date, sampling, treatment, mean, sd, cv, variable) %>% 
    distinct() %>% 
    mutate(variable  = fct_recode(variable,
                        "biomass012" = "biomass"))}

{meta_function(biomass_imp, "biomass", "treatment")
  
  biomass <- data_meta %>% 
    select(date, sampling, treatment, mean, sd, cv, variable) %>% 
    distinct()  }


library(forcats)

data_whole <- rbind(abricheven, biomass) %>% 
  rbind(biomass012)

variables <- c("richness", "abundance", "Y_zipf", "biomass", "biomass012")

list <- list()

for(i in seq_along(variables)){
  
  RR_cv <- data_whole %>% 
    filter(!treatment == "c") %>% 
    filter(variable == variables[i]) %>% 
    select(date, sampling, treatment, cv)
  
  RR_cv_c  <- data_whole %>% 
    filter(treatment == "c")%>% 
    filter(variable == variables[i]) %>% 
    select(date, sampling, cv) %>% 
    rename(cv_c = cv) 
  
  RR_cv <- RR_cv %>% 
    left_join(RR_cv_c, by = c("date", "sampling")) %>% 
    mutate(variable = variables[i])
  
  list[[i]] <- RR_cv
  
}

RR_cv <- do.call(rbind, list) %>% 
  mutate(treatment  = fct_recode(treatment,
                                "w_vs_c" = "w",
                                "p_vs_c" = "p", 
                                "wp_vs_c" = "wp")) %>% 
  rename(RR_descriptor = treatment) %>% 
  mutate(RR = log(cv / cv_c)) %>% 
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
                           "Biomass012")
  )


ggplot(RR_cv, aes(x = date, y = RR)) + 
  facet_grid(variable ~ RR_descriptor, scales = "free_y", 
             labeller = labeller(
               RR_descriptor = as_labeller(labels_RR))) + 
  geom_smooth(
    se = TRUE, aes(color = RR_descriptor, fill = RR_descriptor),
    method = "lm", span = 0.6, alpha = 0.2 ) +
  geom_point(aes(color = RR_descriptor), size = 1.2) + 
  geom_line(aes(color = RR_descriptor)) +
  scale_color_manual(values = palette_RR) +
  scale_fill_manual(values = palette_RR) +
  geom_hline(yintercept= 0, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
  theme(legend.position = "none", labs( y = "RR_cv"))
  
  






