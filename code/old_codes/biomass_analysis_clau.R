


rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)
pacman::p_load(dplyr,reshape2,tidyverse, lubridate, ggplot2, ggpubr, MetBrewer) #Cargamos los paquetes que necesitamos

source("code/first_script.R")



# Configuración del estílo de los gráficos
theme_set(theme_bw()+ theme(legend.position = "NULL"))


# Bases de datos con las que vamos a trabajar
flora_biomass <- flora %>%
  filter(!sampling %in% c("0", "1", "2", "12")) 

# Define the new names
#new_names <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)

# Update the sampling column with the new names
#flora_biomass$sampling <- factor(flora_biomass$sampling, 
 #                                levels = sort(unique(flora_biomass$sampling)), 
  #                               labels = new_names)

flora_morph <- flora_complete



### Dinámicas

biomass_dynamics <- flora_biomass
mean_sd_biomass_dynamics<- biomass_dynamics %>%
  group_by(treatment, sampling) %>%
  summarize(mean_biomass = mean(biomass_total),
            sd_biomass = sd(biomass_total))


treatment_labs_dynamics <- c("Control", "Warming", "Perturbation", "Warming and perturbation")
names(treatment_labs_dynamics) <- c("c","w", "p", "wp")


ggplot(mean_sd_biomass_dynamics, aes(x = as.factor(sampling), y = mean_biomass, group = sampling)) +
  facet_grid(~ treatment, labeller = labeller(treatment = treatment_labs_dynamics)) +
  geom_errorbar(aes(ymin = mean_biomass - sd_biomass, ymax = mean_biomass + sd_biomass,
                    color = treatment, alpha = 0.5),
                position = position_dodge(width = 0.5), width = 0.25, size = 0.75, alpha = 0.4) +
  geom_point(data = biomass_dynamics, aes(x = as.factor(sampling), y = biomass_total, color = treatment),
             position = position_dodge(width = 0.5), size = 1.5, alpha = 0.08) +
  geom_path(group = 1, aes(color = treatment), linewidth = 0.7)+
  geom_point(aes(color = treatment), fill = "white", position = position_dodge(width = 0.5), size = 2.25, shape = 21) +
  #scale_colour_manual(values = met.brewer("VanGogh3",4)) +
  scale_colour_manual(values = c("c" = "green4", "p" = "blue4", "w" = "red3", "wp" = "purple2")) +
  scale_x_discrete(breaks = levels(as.factor(mean_sd_biomass_dynamics$sampling))[seq(1, length(levels(as.factor(mean_sd_biomass_dynamics$sampling))), by = 2)]) +
  labs(x = " ", y = "Community biomass") +
  theme_bw() + theme(legend.position = "none",
                     panel.grid = element_blank(),
                     strip.background = element_blank(),
                     strip.text = element_text(face = "bold"),
                     text = element_text(size = 11))

# Variabilidad de dinámicas (Coefficient of variation (CV) = sd / mean)

mean_sd_biomass_dynamics$cv <- mean_sd_biomass_dynamics$sd_biomass / mean_sd_biomass_dynamics$mean_biomass

ggplot(mean_sd_biomass_dynamics, aes(x = as.factor(sampling), y = cv, group = treatment)) +
  facet_grid(~ treatment, labeller = labeller(treatment = treatment_labs_dynamics)) +
  geom_path(group = 1, aes(color = treatment), linewidth = 0.7, alpha = 0.3)+
  geom_point(aes(color = treatment), fill = "white", position = position_dodge(width = 0.5), size = 1.5,
             shape = 21, alpha = 0.5) +
  geom_smooth( se = F, aes(color = treatment))+
  #scale_colour_manual(values = met.brewer("VanGogh3",4)) +
  scale_colour_manual(values = c("c" = "green4", "p" = "blue4", "w" = "red3", "wp" = "purple2")) +
  labs(x = "Sampling time", y = "Community biomass") +
  theme_bw() + theme(legend.position = "none",
                     panel.grid = element_blank(),
                     strip.background = element_blank(),
                     strip.text = element_text(face = "bold"),
                     text = element_text(size = 12))


#Diferencia por tratamientos Y AÑADIR TEST ESTADÍSTICOS

#Diferencias entre tratamientos agregando por réplicas

flora_biomass_1_plots <- flora_biomass %>%
  filter(sampling %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")) 

flora_biomass_2_plots <- flora_biomass %>%
  filter(!sampling %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")) 

ggarrange(
  ggplot(flora_biomass_1_plots, aes(x = treatment, y = biomass_total , fill = treatment)) +
    geom_boxplot() +
    labs(x = " ", y = "Biomass: different treatments. Year 1)") +
    scale_fill_manual(values = c("c" = "green4", "w" = "#EE6363", "p" = "skyblue2", "wp" = "purple")),
  
  ggplot(flora_biomass_2_plots, aes(x = treatment, y = biomass_total , fill = treatment)) +
    geom_boxplot() +
    labs(x = " ", y = "Biomass: different treatments. Year 2)") +
    scale_fill_manual(values = c("c" = "green4", "w" = "#EE6363", "p" = "skyblue2", "wp" = "purple")),
  
  nrow = 1, ncol = 2)




######### LOG RESPONSE RATIO #####



RR_biomass <-  flora_biomass %>%
  group_by(sampling, date, month, treatment, plot) %>%
  reframe(biomass =  biomass_total, #total biomass per plot (m2) including the number of individual estimation
          richness = richness,  #total number of species per plot
          abundance = abundance_total) %>% # total coverage of plot
  distinct(sampling, date, month, plot, treatment, biomass, richness, abundance)


## Hacemos un loop para calcular el response ratio pero vamos a calcular la referencia de c, w y p. 

samps <- unique(flora_biomass$sampling)


for (i in 1:length(samps)) {
  subset_c <- subset(RR_biomass, sampling == samps[i] & treatment == "c")
  RR_biomass$ref_c_meanbiomass[RR_biomass$sampling == samps[i]] <- mean(subset_c$biomass)
  
  subset_w <- subset(RR_biomass, sampling == samps[i] & treatment == "w")
  RR_biomass$ref_w_meanbiomass[RR_biomass$sampling == samps[i]] <- mean(subset_w$biomass)
  
  subset_p <- subset(RR_biomass, sampling == samps[i] & treatment == "p")
  RR_biomass$ref_p_meanbiomass[RR_biomass$sampling == samps[i]] <- mean(subset_p$biomass)

}




RR_biomass_ref_c <- RR_biomass %>%
  filter(!treatment %in% "c")
RR_biomass_ref_c$logRR_biomass <- round(log(RR_biomass_ref_c$biomass / RR_biomass_ref_c$ref_c_meanbiomass), 2)
mean_sd_data_ref_c <- RR_biomass_ref_c %>%
  group_by(treatment, sampling) %>%
  summarize(mean_biomass = mean(logRR_biomass),
            sd_biomass = sd(logRR_biomass))


RR_biomass_ref_w <- RR_biomass %>%
  filter(!treatment %in% c( "w", "c", "p"))
RR_biomass_ref_w$logRR_biomass <- round(log(RR_biomass_ref_w$biomass / RR_biomass_ref_w$ref_w_meanbiomass), 2)
mean_sd_data_ref_w <- RR_biomass_ref_w %>%
  group_by(treatment, sampling) %>%
  summarize(mean_biomass = mean(logRR_biomass),
            sd_biomass = sd(logRR_biomass))

RR_biomass_ref_p <- RR_biomass %>%
  filter(!treatment %in% c("p", "c", "w"))
RR_biomass_ref_p$logRR_biomass <- round(log(RR_biomass_ref_p$biomass / RR_biomass_ref_p$ref_p_meanbiomass), 2)
mean_sd_data_ref_p <- RR_biomass_ref_p %>%
  group_by(treatment, sampling) %>%
  summarize(mean_biomass = mean(logRR_biomass),
            sd_biomass = sd(logRR_biomass))


#Graficos con script de Rodrigo

#LogRR_biomass donde el control es la referencia de los 3 tratamientos
treatment.labs <- c("Warming", "Perturbation", "Warming and perturbation")
names(treatment.labs) <- c("w", "p", "wp")



##Ggplot logRR con REFRENCIA = CONTROL
#No se ve muchas diferencias. Pero Creo que es porque la perturbación



ggplot(mean_sd_data_ref_c, aes(x = as.factor(sampling), y = mean_biomass, group = sampling)) +
  facet_grid(~ treatment, labeller = labeller(treatment = treatment.labs)) +
  geom_errorbar(aes(ymin = mean_biomass - sd_biomass, ymax = mean_biomass + sd_biomass,
                    color = treatment, alpha = 0.5),
                position = position_dodge(width = 0.5), width = 0.25, size = 0.75) +
  geom_point(data = RR_biomass_ref_c, aes(x = as.factor(sampling), y = logRR_biomass, color = treatment),
             position = position_dodge(width = 0.5), size = 2, alpha = 0.4) +
  geom_path(group = 1, aes(color = treatment), linewidth = 0.8)+
  geom_point(aes(color = treatment), fill = "white", position = position_dodge(width = 0.5), size = 2.25, shape = 21) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.7) +
  #scale_colour_manual(values = met.brewer("VanGogh2", 3)) +
  scale_colour_manual(values = c("p" = "blue4", "w" = "red3", "wp" = "purple2")) +
  scale_x_discrete(breaks = levels(as.factor(mean_sd_data_ref_c$sampling))[seq(1, length(levels(as.factor(mean_sd_data_ref_c$sampling))), by = 2)]) +
  labs(x = "Sampling time", y = "Total biomass (log response ratio)") +
  theme_bw() + theme(legend.position = "none",
                     panel.grid = element_blank(),
                     strip.background = element_blank(),
                     strip.text = element_text(face = "bold"),
                     text = element_text(size = 12))




##Ggplot logRR(wp/w)
# Cabiar título y color

gglogrr_wp_w_biomass <- 
ggplot(mean_sd_data_ref_w, aes(x = as.factor(sampling), y = mean_biomass, group = sampling)) +
  facet_grid(~ treatment, labeller = labeller(treatment = treatment.labs)) +
  geom_errorbar(aes(ymin = mean_biomass - sd_biomass, ymax = mean_biomass + sd_biomass,
                    color = treatment, alpha = 0.5),
                position = position_dodge(width = 0.5), width = 0.25, size = 0.75) +
  geom_point(data = RR_biomass_ref_w, aes(x = as.factor(sampling), y = logRR_biomass, color = treatment),
             position = position_dodge(width = 0.5), size = 2, alpha = 0.4) +
  geom_path(group = 1, aes(color = treatment), linewidth = 0.6) +
  geom_point(aes(color = treatment), fill = "white", position = position_dodge(width = 0.5), size = 2.25, shape = 21) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.7) +
  scale_colour_manual(values = met.brewer("VanGogh2", 3)) +
  labs(x = "Muestreos", y = "Biomasa total (log response ratio (wp/w))") +
  theme_bw() + theme(legend.position = "none",
                     panel.grid = element_blank(),
                     strip.background = element_blank(),
                     strip.text = element_text(face = "bold"),
                     text = element_text(size = 12))

##Ggplot logRR(wp/p)
# Cambiar título y color

gglogrr_wp_p_biomass <- 
ggplot(mean_sd_data_ref_p, aes(x = as.factor(sampling), y = mean_biomass, group = sampling)) +
  facet_grid(~ treatment, labeller = labeller(treatment = treatment.labs)) +
  geom_errorbar(aes(ymin = mean_biomass - sd_biomass, ymax = mean_biomass + sd_biomass,
                    color = treatment, alpha = 0.5),
                position = position_dodge(width = 0.5), width = 0.25, size = 0.75) +
  geom_point(data = RR_biomass_ref_p, aes(x = as.factor(sampling), y = logRR_biomass, color = treatment),
             position = position_dodge(width = 0.5), size = 2, alpha = 0.4) +
  geom_path(group = 1, aes(color = treatment), linewidth = 0.6) +
  geom_point(aes(color = treatment), fill = "white", position = position_dodge(width = 0.5), size = 2.25, shape = 21) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.7) +
  scale_colour_manual(values = met.brewer("VanGogh2", 3)) +
  labs(x = "Muestreos", y = "Biomasa total (log response ratio (wp/p))") +
  theme_bw() + theme(legend.position = "none",
                     panel.grid = element_blank(),
                     strip.background = element_blank(),
                     strip.text = element_text(face = "bold"),
                     text = element_text(size = 12))

ggarrange(gglogrr_wp_w_biomass, gglogrr_wp_p_biomass, ncol = 2, nrow = 1, labels = c("a)", "b)"))



########### LogRR del CV #############

for (i in 1:length(samps)) {
  subset_c <- subset(mean_sd_biomass_dynamics, sampling == samps[i] & treatment == "c")
  mean_sd_biomass_dynamics$ref_c_cv[mean_sd_biomass_dynamics$sampling == samps[i]] <- mean(subset_c$cv)
  
  subset_w <- subset(mean_sd_biomass_dynamics, sampling == samps[i] & treatment == "w")
  mean_sd_biomass_dynamics$ref_w_cv[mean_sd_biomass_dynamics$sampling == samps[i]] <- mean(subset_w$cv)
  
  subset_p <- subset(mean_sd_biomass_dynamics, sampling == samps[i] & treatment == "p")
  mean_sd_biomass_dynamics$ref_p_cv[mean_sd_biomass_dynamics$sampling == samps[i]] <- mean(subset_p$cv)
  
}


RR_biomass_ref_c_cv <- mean_sd_biomass_dynamics %>%
  filter(!treatment %in% "c")
RR_biomass_ref_c_cv$logRR_cv <- round(log(RR_biomass_ref_c_cv$cv / RR_biomass_ref_c_cv$ref_c_cv), 2)

treatment.labs <- c("Warming", "Perturbation", "Warming and perturbation")
names(treatment.labs) <- c("w", "p", "wp")

ggplot(RR_biomass_ref_c_cv, aes(x = as.factor(sampling), y = logRR_cv, group = treatment)) +
  facet_grid(~ treatment, labeller = labeller(treatment = treatment.labs)) +
  geom_path(group = 1, aes(color = treatment), linewidth = 0.8, alpha = 0.5)+
  geom_point(aes(color = treatment), fill = "white", position = position_dodge(width = 0.5),
             size = 2.25, shape = 21, alpha = 0.5) +
  geom_smooth(se = F, aes(color = treatment, fill = treatment)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.7) +
  #scale_colour_manual(values = met.brewer("VanGogh2", 3)) +
  scale_colour_manual(values = c("p" = "blue4", "w" = "red3", "wp" = "purple2")) +
  labs(x = "Sampling time", y = "CV (log response ratio)") +
  theme_bw() + theme(legend.position = "none",
                     panel.grid = element_blank(),
                     strip.background = element_blank(),
                     strip.text = element_text(face = "bold"),
                     text = element_text(size = 12))



################## Valores morfológicos de la ecuación ####

RR_biomass_morph <-  flora_morph %>%    ## Datos morfológicos agregados por réplicas (media altura por plot y muestreo)
 group_by(sampling, date, month, treatment, plot) %>%
 reframe(height = mean(height, na.rm = T),
         Ah = mean(Ah, na.rm = T), 
         Ab = mean(Ab, na.rm = T),
         n_species = n_species,  #total number of species per plot
         abundance = sum(abundance, na.rm = T),
         biomass_i = sum(biomass_i, na.rm =T)) %>% # total coverage of plot
 distinct(sampling, date, month, plot, treatment, height, Ah, Ab, n_species, biomass_i, abundance)



#RR_biomass_morph <- flora_morph  #Datos morfológicos sin agregar por réplicas. 
samps <- unique(RR_biomass_morph$sampling)

RR_biomass_morph_ref_c <- RR_biomass_morph
RR_biomass_morph_ref_w <- RR_biomass_morph
RR_biomass_morph_ref_p <- RR_biomass_morph

for (i in 1:length(samps)) {
  subset_c <- subset(RR_biomass_morph, sampling == samps[i] & treatment == "c")
  subset_w <- subset(RR_biomass_morph, sampling == samps[i] & treatment == "w")
  subset_p <- subset(RR_biomass_morph, sampling == samps[i] & treatment == "p")
  
  
  RR_biomass_morph_ref_c$c_meanheight[RR_biomass_morph$sampling == samps[i]] <- mean(subset_c$height)
  RR_biomass_morph_ref_c$c_meanAh[RR_biomass_morph$sampling == samps[i]] <- mean(subset_c$Ah)
  RR_biomass_morph_ref_c$c_meanAb[RR_biomass_morph$sampling == samps[i]] <- mean(subset_c$Ab)
  
  RR_biomass_morph_ref_w$w_meanheight[RR_biomass_morph$sampling == samps[i]] <- mean(subset_w$height)
  RR_biomass_morph_ref_w$w_meanAh[RR_biomass_morph$sampling == samps[i]] <- mean(subset_w$Ah)
  RR_biomass_morph_ref_w$w_meanAb[RR_biomass_morph$sampling == samps[i]] <- mean(subset_w$Ab)
  
  RR_biomass_morph_ref_p$p_meanheight[RR_biomass_morph$sampling == samps[i]] <- mean(subset_p$height)
  RR_biomass_morph_ref_p$p_meanAh[RR_biomass_morph$sampling == samps[i]] <- mean(subset_p$Ah)
  RR_biomass_morph_ref_p$p_meanAb[RR_biomass_morph$sampling == samps[i]] <- mean(subset_p$Ab)
  
}


RR_biomass_morph_ref_c <- RR_biomass_morph_ref_c %>%
  filter(!treatment %in% "c")

RR_biomass_morph_ref_c$logRR_height <- round(log(RR_biomass_morph_ref_c$height/ RR_biomass_morph_ref_c$c_meanheight), 2)
RR_biomass_morph_ref_c$logRR_Ah <- round(log(RR_biomass_morph_ref_c$Ah/ RR_biomass_morph_ref_c$c_meanAh), 2)
RR_biomass_morph_ref_c$logRR_Ab <- round(log(RR_biomass_morph_ref_c$Ab/ RR_biomass_morph_ref_c$c_meanAb), 2)

RR_biomass_morph_ref_w <- RR_biomass_morph_ref_w %>%
  filter(!treatment %in% c( "w", "c", "p"))

RR_biomass_morph_ref_w$logRR_height <- round(log(RR_biomass_morph_ref_w$height/ RR_biomass_morph_ref_w$w_meanheight), 2)
RR_biomass_morph_ref_w$logRR_Ah <- round(log(RR_biomass_morph_ref_w$Ah/ RR_biomass_morph_ref_w$w_meanAh), 2)
RR_biomass_morph_ref_w$logRR_Ab <- round(log(RR_biomass_morph_ref_w$Ab/ RR_biomass_morph_ref_w$w_meanAb), 2)

RR_biomass_morph_ref_p <- RR_biomass_morph_ref_p %>%
  filter(!treatment %in% c( "w", "c", "p"))

RR_biomass_morph_ref_p$logRR_height <- round(log(RR_biomass_morph_ref_p$height/ RR_biomass_morph_ref_p$p_meanheight), 2)
RR_biomass_morph_ref_p$logRR_Ah <- round(log(RR_biomass_morph_ref_p$Ah/ RR_biomass_morph_ref_p$p_meanAh), 2)
RR_biomass_morph_ref_p$logRR_Ab <- round(log(RR_biomass_morph_ref_p$Ab/ RR_biomass_morph_ref_p$p_meanAb), 2)




#LogRR_biomass donde se compara los 3 tratamientos con el control
ggarrange(
  ggplot(RR_biomass_morph_ref_c, aes(x = sampling, y = logRR_height, fill = treatment)) +
    geom_boxplot() +
    labs(x = " ", y = "logRR(height)") +
    facet_grid(~ treatment) + 
    scale_fill_manual(values = c("w" = "#EE6363", "p" = "skyblue2", "wp" = "purple"))+
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8), 
  
  ggplot(RR_biomass_morph_ref_c, aes(x = sampling, y = logRR_Ah, fill = treatment)) +
    geom_boxplot() +
    labs(x = " ", y = "logRR(Ah)") +
    facet_grid(~ treatment) + 
    scale_fill_manual(values = c("w" = "#EE6363", "p" = "skyblue2", "wp" = "purple"))+
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8), 
  
  ggplot(RR_biomass_morph_ref_c, aes(x = sampling, y = logRR_Ab, fill = treatment)) +
    geom_boxplot() +
    labs(x = " ", y = "logRR(Ab)") +
    facet_grid(~ treatment) + 
    scale_fill_manual(values = c("w" = "#EE6363", "p" = "skyblue2", "wp" = "purple"))+
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8), 
  
  ncol = 1, nrow = 3)


ggplot(RR_biomass_morph_ref_c, aes(x = treatment, y = logRR_height, fill = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "logRR(height)") +
  facet_wrap(~ code, nrow = 7, ncol = 7) + 
  scale_fill_manual(values = c("w" = "#EE6363", "p" = "skyblue2", "wp" = "purple"))+
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8)



# LogRR_biomass donde se compara el tratamiento wp con w
ggarrange(
  ggplot(RR_biomass_morph_ref_w, aes(x = sampling, y = logRR_height, fill = treatment)) +
    geom_boxplot() +
    labs(x = " ", y = "Height: logRR(wp/w)") +
    scale_fill_manual(values = c("wp" = "indianred3"))+
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8), 
  
  ggplot(RR_biomass_morph_ref_w, aes(x = sampling, y = logRR_Ah, fill = treatment)) +
    geom_boxplot() +
    labs(x = " ", y = "Ah: logRR(wp/w)") +
    scale_fill_manual(values = c("wp" = "indianred3"))+
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8), 
  
  ggplot(RR_biomass_morph_ref_w, aes(x = sampling, y = logRR_Ab, fill = treatment)) +
    geom_boxplot() +
    labs(x = " ", y = "Ab: logRR(wp/w)") +
    scale_fill_manual(values = c("wp" = "indianred3"))+
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8), 
  
  ncol = 1, nrow = 3)

#LogRR_biomass donde se compara el tratamiento wp con p
ggarrange(
  ggplot(RR_biomass_morph_ref_p, aes(x = sampling, y = logRR_height, fill = treatment)) +
    geom_boxplot() +
    labs(x = " ", y = "Height: logRR(wp/p)") +
    scale_fill_manual(values = c("wp" = "maroon4"))+
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8), 
  
  ggplot(RR_biomass_morph_ref_p, aes(x = sampling, y = logRR_Ah, fill = treatment)) +
    geom_boxplot() +
    labs(x = " ", y = "Ah: logRR(wp/p)") +
    scale_fill_manual(values = c("wp" = "maroon4"))+
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8), 
  
  ggplot(RR_biomass_morph_ref_p, aes(x = sampling, y = logRR_Ab, fill = treatment)) +
    geom_boxplot() +
    labs(x = " ", y = "Ab: logRR(wp/p)") +
    scale_fill_manual(values = c("wp" = "maroon4"))+
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8), 
  
  ncol = 1, nrow = 3)





