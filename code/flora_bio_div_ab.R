
# Cosas que hacer ####
# MODIFICAR TODA LA BASE DE DATOS EN BASE A BIOMASS (cambiar la estética de los gráficos)


#Packages   ####
library(stringr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(readr)
library(ggpubr)
library(tidyverse)
library(gridExtra)
library(MetBrewer)

#Scripts 
source('code/tools/basicFun.R')
source("code/first_script.R")
radcoeff_df <- read.csv("data/radcoeff_df.csv")



### OUTLIERS ##### (check "Dynamics and outliers.Rmd" for more info)

############### Transformation database ############

## Se trabaja con "flora" pero podríamos trabajar con "flora1". El problema de "flora1" es que, al eliminar los NA de biomasa,
# no tenemos datos para ninguna variable de los muestreos 0, 1 y 2. 
 #Removing species amaranthus



#Agrupar por sampling, plot y treatment primero. 
#Aquí se aplica, para biomasa, el siguiente criterio: se estima la biomasa de cada especie multiplicando su abundancia por 
#la masa promedio de los individuos medidos para dicha especie. 

ab_rich_dynamics <-  flora %>%
  group_by(sampling, date, month, treatment, plot) %>%
  reframe(biomass =  biomass_total,#total abundance per plot (m2) (replicate of treatment)
          richness = richness,  #total number of species per plot
          abundance = abundance_total) %>% # total coverage of plot
  distinct(sampling, date, month, plot, treatment, biomass, richness, abundance)

  
ab_rich_dynamics <- merge(ab_rich_dynamics, radcoeff_df)   

mean_sd_abrich_dynamics<- ab_rich_dynamics %>%
  group_by(treatment, sampling) %>%
  summarize(mean_richness = mean(richness),
            sd_richness = sd(richness),
            mean_abundance = mean(abundance),
            sd_abundance = sd(abundance),
            mean_yzipf = mean(Y_zipf),
            sd_yzipf = sd(Y_zipf),
            mean_mulog = mean(mu_log),
            sd_mulog = sd(mu_log),
            mean_sigmalog = mean(sigma_log),
            sd_sigmalog = sd(sigma_log))

mean_sd_abrich_dynamics$cv_richness <- mean_sd_abrich_dynamics$sd_richness /mean_sd_abrich_dynamics$mean_richness





hist(ab_rich_dynamics$richness)
hist(ab_rich_dynamics$abundance)
hist(ab_rich_dynamics$biomass)



# Gráficos por muestreo y tratamiento####

# Other way with facet_grid

theme_set(theme_bw()+ theme(legend.position = "NULL"))



treatment_labs_dynamics <- c("Control", "Warming", "Perturbation", "Warming and perturbation")
names(treatment_labs_dynamics) <- c("c","w", "p", "wp")

##Hacer los gráficos con un loop y guardarlos en una lista


ggplot(mean_sd_abrich_dynamics, aes(x = as.factor(sampling), y = mean_richness, group = sampling)) +
  facet_grid(~ treatment, labeller = labeller(treatment = treatment_labs_dynamics)) +
  geom_errorbar(aes(ymin = mean_richness - sd_richness, ymax = mean_richness + sd_richness,
                    color = treatment, alpha = 0.5),
                position = position_dodge(width = 0.5), width = 0.25, size = 0.75, alpha = 0.4) +
  geom_point(data = ab_rich_dynamics, aes(x = as.factor(sampling), y = richness, color = treatment),
             position = position_dodge(width = 0.5), size = 1.5, alpha = 0.2) +
  geom_path(group = 1, aes(color = treatment), linewidth = 0.7)+
  geom_point(aes(color = treatment), fill = "white", position = position_dodge(width = 0.5), size = 2.25, shape = 21) +
  #scale_colour_manual(values = met.brewer("VanGogh3",4)) +
  scale_colour_manual(values = c("c" = "green4", "p" = "blue4", "w" = "red3", "wp" = "purple2")) +
  scale_x_discrete(breaks = levels(as.factor(mean_sd_abrich_dynamics$sampling))[seq(1, length(levels(as.factor(mean_sd_abrich_dynamics$sampling))), by = 2)]) +
  labs(x = " ", y = "Richness") +
  theme_bw() + theme(legend.position = "none",
                     panel.grid = element_blank(),
                     strip.background = element_blank(),
                     strip.text = element_text(face = "bold"),
                     text = element_text(size = 11))

ggplot(mean_sd_abrich_dynamics, aes(x = as.factor(sampling), y = mean_abundance, group = sampling)) +
  facet_grid(~ treatment, labeller = labeller(treatment = treatment_labs_dynamics)) +
  geom_errorbar(aes(ymin = mean_abundance - sd_abundance, ymax = mean_abundance + sd_abundance,
                    color = treatment, alpha = 0.5),
                position = position_dodge(width = 0.5), width = 0.25, size = 0.75, alpha = 0.4) +
  geom_point(data = ab_rich_dynamics, aes(x = as.factor(sampling), y = abundance, color = treatment),
             position = position_dodge(width = 0.5), size = 1.5, alpha = 0.2) +
  geom_path(group = 1, aes(color = treatment), linewidth = 0.7)+
  geom_point(aes(color = treatment), fill = "white", position = position_dodge(width = 0.5), size = 2.25, shape = 21) +
  #scale_colour_manual(values = met.brewer("VanGogh3",4)) +
  scale_colour_manual(values = c("c" = "green4", "p" = "blue4", "w" = "red3", "wp" = "purple2")) +
  scale_x_discrete(breaks = levels(as.factor(mean_sd_abrich_dynamics$sampling))[seq(1, length(levels(as.factor(mean_sd_abrich_dynamics$sampling))), by = 2)]) +
  labs(x = " ", y = "Abundance") +
  theme_bw() + theme(legend.position = "none",
                     panel.grid = element_blank(),
                     strip.background = element_blank(),
                     strip.text = element_text(face = "bold"),
                     text = element_text(size = 11))

ggplot(mean_sd_abrich_dynamics, aes(x = as.factor(sampling), y = mean_yzipf, group = sampling)) +
  facet_grid(~ treatment, labeller = labeller(treatment = treatment_labs_dynamics)) +
  geom_errorbar(aes(ymin = mean_yzipf - sd_yzipf, ymax = mean_yzipf + sd_yzipf,
                    color = treatment, alpha = 0.5),
                position = position_dodge(width = 0.5), width = 0.25, size = 0.75, alpha = 0.4) +
  geom_point(data = ab_rich_dynamics, aes(x = as.factor(sampling), y = Y_zipf, color = treatment),
             position = position_dodge(width = 0.5), size = 1.5, alpha = 0.2) +
  geom_path(group = 1, aes(color = treatment), linewidth = 0.7)+
  geom_point(aes(color = treatment), fill = "white", position = position_dodge(width = 0.5), size = 2.25, shape = 21) +
  #scale_colour_manual(values = met.brewer("VanGogh3",4)) +
  scale_colour_manual(values = c("c" = "green4", "p" = "blue4", "w" = "red3", "wp" = "purple2")) +
  scale_x_discrete(breaks = levels(as.factor(mean_sd_abrich_dynamics$sampling))[seq(1, length(levels(as.factor(mean_sd_abrich_dynamics$sampling))), by = 2)]) +
  labs(x = " ", y = "yzipf") +
  theme_bw() + theme(legend.position = "none",
                     panel.grid = element_blank(),
                     strip.background = element_blank(),
                     strip.text = element_text(face = "bold"),
                     text = element_text(size = 11))



# Sampling 0

ggS0 <- 
ggarrange(
ggplot(subset(ab_rich_dynamics, sampling == "0"), aes(x = treatment, y = richness, fill = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "Richness") +
  scale_fill_manual(values = c("c" = "darkolivegreen2", "p" = "#1C86EE", "w" = "#EE6363", "wp" = "purple")),

ggplot(subset(ab_rich_dynamics, sampling == "0"), aes(x = treatment, y = abundance, fill = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "Abundance") +
  scale_fill_manual(values = c("c" = "darkolivegreen2", "p" = "#1C86EE", "w" = "#EE6363", "wp" = "purple")),

ggplot(subset(ab_rich_dynamics, sampling == "0"), aes(x = treatment, y = Y_zipf, fill = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "Y_zipf") +
  scale_fill_manual(values = c("c" = "darkolivegreen2", "p" = "#1C86EE", "w" = "#EE6363", "wp" = "purple")),

ggplot(subset(ab_rich_dynamics, sampling == "0"), aes(x = treatment, y = mu_log, fill = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "mu_log") +
  scale_fill_manual(values = c("c" = "darkolivegreen2", "p" = "#1C86EE", "w" = "#EE6363", "wp" = "purple")),

ggplot(subset(ab_rich_dynamics, sampling == "0"), aes(x = treatment, y = sigma_log, fill = treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "sigma_log") +
  scale_fill_manual(values = c("c" = "darkolivegreen2", "p" = "#1C86EE", "w" = "#EE6363", "wp" = "purple")),

nrow = 1, ncol = 5)




##### RESPONSE RATIO  Log(RR); RR = variable at point i / variable at point reference #######################
# Hacer esto como lo he hecho en abundance

RR_ab_rich <- ab_rich_dynamics
samps <- unique(RR_ab_rich$sampling)

#Sampling 2 has several plots with very few species, which generates problems at RADs
# At plot 15, we have only 1 species so there is no RAD
# At plot 13, we have 2 species, so the RAD values are not reliable. Im deleting this data by hand: 
RR_ab_rich$Y_zipf[RR_ab_rich$sampling == "2" & RR_ab_rich$plot == "13"] <- NA
RR_ab_rich$mu_log[RR_ab_rich$sampling == "2" & RR_ab_rich$plot == "13"] <- NA
RR_ab_rich$sigma_log[RR_ab_rich$sampling == "2" & RR_ab_rich$plot == "13"] <- NA

#100% puedo reducir todas las lineas de código al incluir un vector que incluya las variabes. Tipo:
# variables <- c(colnames(ab_rich_dynamics[, 7:11])) investigar si eso

for (i in 1:length(samps)) {
  subset_c <- subset(RR_ab_rich, sampling == samps[i] & treatment == "c")
  RR_ab_rich$ref_c_mean_richness[RR_ab_rich$sampling == samps[i]] <- mean(subset_c$richness)
  RR_ab_rich$ref_c_mean_abundance[RR_ab_rich$sampling == samps[i]] <- mean(subset_c$abundance)
  RR_ab_rich$ref_c_mean_yzipf[RR_ab_rich$sampling == samps[i]] <- mean(subset_c$Y_zipf)
  RR_ab_rich$ref_c_mean_mulog[RR_ab_rich$sampling == samps[i]] <- mean(subset_c$mu_log)
  RR_ab_rich$ref_c_mean_sigmalog[RR_ab_rich$sampling == samps[i]] <- mean(subset_c$sigma_log)
  
  
  subset_w <- subset(RR_ab_rich, sampling == samps[i] & treatment == "w")
  RR_ab_rich$ref_w_mean_richness[RR_ab_rich$sampling == samps[i]] <- mean(subset_w$richness)
  RR_ab_rich$ref_w_mean_abundance[RR_ab_rich$sampling == samps[i]] <- mean(subset_w$abundance)
  RR_ab_rich$ref_w_mean_yzipf[RR_ab_rich$sampling == samps[i]] <- mean(subset_w$Y_zipf)
  RR_ab_rich$ref_w_mean_mulog[RR_ab_rich$sampling == samps[i]] <- mean(subset_w$mu_log)
  RR_ab_rich$ref_w_mean_sigmalog[RR_ab_rich$sampling == samps[i]] <- mean(subset_w$sigma_log)
  
  
  subset_p <- subset(RR_ab_rich, sampling == samps[i] & treatment == "p")
  RR_ab_rich$ref_p_mean_richness[RR_ab_rich$sampling == samps[i]] <- mean(subset_p$richness)
  RR_ab_rich$ref_p_mean_abundance[RR_ab_rich$sampling == samps[i]] <- mean(subset_p$abundance)
  RR_ab_rich$ref_p_mean_yzipf[RR_ab_rich$sampling == samps[i]] <- mean(subset_p$Y_zipf)
  RR_ab_rich$ref_p_mean_mulog[RR_ab_rich$sampling == samps[i]] <- mean(subset_p$mu_log)
  RR_ab_rich$ref_p_mean_sigmalog[RR_ab_rich$sampling == samps[i]] <- mean(subset_p$sigma_log)

  
  
  rm(subset_c)
  rm(subset_w)
  rm(subset_p)

}


## Hacer con un loop las bases de datos y los ggplots?

treatment.labs <- c("Warming", "Perturbation", "Warming and perturbation")
names(treatment.labs) <- c("w", "p", "wp")



RR_ref_c <- RR_ab_rich %>%
  filter(!treatment %in% "c")
RR_ref_c$logRR_abundance <- round(log(RR_ref_c$abundance / RR_ref_c$ref_c_mean_abundance), 2)
RR_ref_c$logRR_richness <- round(log(RR_ref_c$richness / RR_ref_c$ref_c_mean_richness), 2)
RR_ref_c$logRR_yzipf <- round(log(RR_ref_c$Y_zipf / RR_ref_c$ref_c_mean_yzipf), 2)
RR_ref_c$logRR_mulog <- round(log(RR_ref_c$mu_log / RR_ref_c$ref_c_mean_mulog), 2)
RR_ref_c$logRR_sigmalog <- round(log(RR_ref_c$sigma_log / RR_ref_c$ref_c_mean_sigmalog), 2)

mean_sd_data_ref_c <- RR_ref_c %>%
  group_by(treatment, sampling) %>%
  summarize(mean_abundance = mean(logRR_abundance),
            sd_abundance = sd(logRR_abundance),
            mean_richness = mean(logRR_richness),
            sd_richness = sd(logRR_richness),
            mean_yzipf = mean(logRR_yzipf),
            sd_yzipf = sd(logRR_yzipf),
            mean_mulog = mean(logRR_mulog),
            sd_mulog = sd(logRR_mulog),
            mean_sigmalog= mean(logRR_sigmalog),
            sd_sigmalog = sd(logRR_sigmalog))

ggplot(mean_sd_data_ref_c, aes(x = as.factor(sampling), y = mean_richness, group = sampling)) +
  facet_grid(~ treatment, labeller = labeller(treatment = treatment.labs)) +
  geom_errorbar(aes(ymin = mean_richness - sd_richness, ymax = mean_richness + sd_richness,
                    color = treatment, alpha = 0.5),
                position = position_dodge(width = 0.5), width = 0.25, size = 0.75) +
  geom_point(data = RR_ref_c, aes(x = as.factor(sampling), y = logRR_richness, color = treatment),
             position = position_dodge(width = 0.5), size = 2, alpha = 0.4) +
  geom_path(group = 1, aes(color = treatment), linewidth = 0.8)+
  geom_point(aes(color = treatment), fill = "white", position = position_dodge(width = 0.5), size = 2.25, shape = 21) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.7) +
  #scale_colour_manual(values = met.brewer("VanGogh2", 3)) +
  scale_colour_manual(values = c("p" = "blue4", "w" = "red3", "wp" = "purple2")) +
  scale_x_discrete(breaks = levels(as.factor(mean_sd_data_ref_c$sampling))[seq(1, length(levels(as.factor(mean_sd_data_ref_c$sampling))), by = 2)]) +
  labs(x = "Sampling time", y = "Richness (log response ratio)") +
  theme_bw() + theme(legend.position = "none",
                     panel.grid = element_blank(),
                     strip.background = element_blank(),
                     strip.text = element_text(face = "bold"),
                     text = element_text(size = 12))





RR_ref_w <- RR_ab_rich %>%
  filter(!treatment %in% c( "w", "c", "p"))
RR_ref_w$logRR_abundance <- round(log(RR_ref_w$abundance / RR_ref_w$ref_w_mean_abundance), 2)
RR_ref_w$logRR_richness <- round(log(RR_ref_w$richness / RR_ref_w$ref_w_mean_richness), 2)
RR_ref_w$logRR_yzipf <- round(log(RR_ref_w$Y_zipf / RR_ref_w$ref_w_mean_yzipf), 2)
RR_ref_w$logRR_mulog <- round(log(RR_ref_w$mu_log / RR_ref_w$ref_w_mean_mulog), 2)
RR_ref_w$logRR_sigmalog <- round(log(RR_ref_w$sigma_log / RR_ref_w$ref_w_mean_sigmalog), 2)

mean_sd_data_ref_w <- RR_ref_w %>%
  group_by(treatment, sampling) %>%
  summarize(mean_abundance = mean(logRR_abundance),
            sd_abundance = sd(logRR_abundance),
            mean_richness = mean(logRR_richness),
            sd_richness = sd(logRR_richness),
            mean_yzipf = mean(logRR_yzipf),
            sd_yzipf = sd(logRR_yzipf),
            mean_mulog = mean(logRR_mulog),
            sd_mulog = sd(logRR_mulog),
            mean_abundance = mean(logRR_abundance),
            sd_sigmalog = sd(logRR_sigmalog))

gglogrr_richness_wp_w <-  
 ggplot(mean_sd_data_ref_w, aes(x = as.factor(sampling), y = mean_richness, group = sampling)) +
  facet_grid(~ treatment, labeller = labeller(treatment = treatment.labs)) +
  geom_errorbar(aes(ymin = mean_richness - sd_richness, ymax = mean_richness + sd_richness,
                    color = treatment, alpha = 0.5),
                position = position_dodge(width = 0.5), width = 0.25, size = 0.75) +
  geom_point(data = RR_ref_w, aes(x = as.factor(sampling), y = logRR_richness, color = treatment),
             position = position_dodge(width = 0.5), size = 2, alpha = 0.4) +
  geom_path(group = 1, aes(color = treatment), linewidth = 0.6) +
  geom_point(aes(color = treatment), fill = "white", position = position_dodge(width = 0.5), size = 2.25, shape = 21) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.7) +
  scale_colour_manual(values = met.brewer("VanGogh2", 3)) +
  labs(x = "Sampling time", y = "Species richness (log response ratio (wp/w))") +
  theme_bw() + theme(legend.position = "none",
                     panel.grid = element_blank(),
                     strip.background = element_blank(),
                     strip.text = element_text(face = "bold"),
                     text = element_text(size = 12))

RR_ref_p <- RR_ab_rich %>%
  filter(!treatment %in% c( "p", "c", "w"))
RR_ref_p$logRR_abundance <- round(log(RR_ref_p$abundance / RR_ref_p$ref_p_mean_abundance), 2)
RR_ref_p$logRR_richness <- round(log(RR_ref_p$richness / RR_ref_p$ref_p_mean_richness), 2)
RR_ref_p$logRR_yzipf <- round(log(RR_ref_p$Y_zipf / RR_ref_p$ref_p_mean_yzipf), 2)
RR_ref_p$logRR_mulog <- round(log(RR_ref_p$mu_log / RR_ref_p$ref_p_mean_mulog), 2)
RR_ref_p$logRR_sigmalog <- round(log(RR_ref_p$sigma_log / RR_ref_p$ref_p_mean_sigmalog), 2)

mean_sd_data_ref_p <- RR_ref_p %>%
  group_by(treatment, sampling) %>%
  summarize(mean_abundance = mean(logRR_abundance),
            sd_abundance = sd(logRR_abundance),
            mean_richness = mean(logRR_richness),
            sd_richness = sd(logRR_richness),
            mean_yzipf = mean(logRR_yzipf),
            sd_yzipf = sd(logRR_yzipf),
            mean_mulog = mean(logRR_mulog),
            sd_mulog = sd(logRR_mulog),
            mean_abundance = mean(logRR_abundance),
            sd_sigmalog = sd(logRR_sigmalog))

gglogrr_richness_wp_p <- 

ggplot(mean_sd_data_ref_p, aes(x = as.factor(sampling), y = mean_richness, group = sampling)) +
  facet_grid(~ treatment, labeller = labeller(treatment = treatment.labs)) +
  geom_errorbar(aes(ymin = mean_richness - sd_richness, ymax = mean_richness + sd_richness,
                    color = treatment, alpha = 0.5),
                position = position_dodge(width = 0.5), width = 0.25, size = 0.75) +
  geom_point(data = RR_ref_p, aes(x = as.factor(sampling), y = logRR_richness, color = treatment),
             position = position_dodge(width = 0.5), size = 2, alpha = 0.4) +
  geom_path(group = 1, aes(color = treatment), linewidth = 0.6) +
  geom_point(aes(color = treatment), fill = "white", position = position_dodge(width = 0.5), size = 2.25, shape = 21) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.7) +
  scale_colour_manual(values = met.brewer("VanGogh2", 3)) +
  labs(x = "Sampling time", y = "Species richness (log response ratio (wp/p))") +
  theme_bw() + theme(legend.position = "none",
                     panel.grid = element_blank(),
                     strip.background = element_blank(),
                     strip.text = element_text(face = "bold"),
                     text = element_text(size = 12))

ggarrange( gglogrr_richness_wp_w, gglogrr_richness_wp_p, nrow = 1, ncol = 2, labels = c("A", "B"))




# RR: Control as reference of W, P and WP

ggRRcontrol<- 
ggarrange(
  ggplot(RR_ab_rich[RR_ab_rich$treatment != "c", ], aes(x = sampling, y = RR_abundance_C, fill = treatment)) +
    geom_boxplot() +
    labs(x = " ", y = "logRR(abundance)") +
    facet_grid(~ treatment) + 
    scale_fill_manual(values = c("w" = "#EE6363", "p" = "skyblue2", "wp" = "purple"))+
    geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
  
  ggplot(RR_ab_rich[RR_ab_rich$treatment != "c", ], aes(x = sampling, y = RR_richness_C, fill = treatment)) +
    geom_boxplot() +
    labs(x = " ", y = "logRR(richness)") +
    facet_grid(~ treatment) + 
    scale_fill_manual(values = c("w" = "#EE6363", "p" = "skyblue2", "wp" = "purple"))+
    geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
  
  nrow = 2, ncol = 1)


ggRRcontrol_evenness<- 
  ggarrange(
    ggplot(RR_ab_rich[RR_ab_rich$treatment != "c", ], aes(x = sampling, y = RR_yzipf_C, fill = treatment)) +
      geom_boxplot() +
      labs(x = " ", y = "logRR(Y)") +
      facet_grid(~ treatment) + 
      scale_fill_manual(values = c("w" = "#EE6363", "p" = "skyblue2", "wp" = "purple"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
    
    ggplot(RR_ab_rich[RR_ab_rich$treatment != "c", ], aes(x = sampling, y = RR_mulog_C, fill = treatment)) +
      geom_boxplot() +
      labs(x = " ", y = "logRR(mu)") +
      facet_grid(~ treatment) + 
      scale_fill_manual(values = c("w" = "#EE6363", "p" = "skyblue2", "wp" = "purple"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
    
    ggplot(RR_ab_rich[RR_ab_rich$treatment != "c", ], aes(x = sampling, y = RR_sigmalog_C, fill = treatment)) +
      geom_boxplot() +
      labs(x = " ", y = "logRR(sigma)") +
      facet_grid(~ treatment) + 
      scale_fill_manual(values = c("w" = "#EE6363", "p" = "skyblue2", "wp" = "purple"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
    
    nrow = 3, ncol = 1)


# P and W as references of WP. 

RR_wp_ab <- pivot_longer(RR_ab_rich, cols = c("RR_abundance_W", "RR_abundance_P"), 
                         names_to = "RR_ab_treatment", values_to = "RR_ab_values")

RR_wp_rich <- pivot_longer(RR_ab_rich, cols = c("RR_richness_W", "RR_richness_P"), 
                           names_to = "RR_rich_treatment", values_to = "RR_rich_values")

RR_wp_yzipf <- pivot_longer(RR_ab_rich, cols = c("RR_yzipf_W", "RR_yzipf_P"), 
                           names_to = "RR_yzipf_treatment", values_to = "RR_yzipf_values")

RR_wp_mulog <- pivot_longer(RR_ab_rich, cols = c("RR_mulog_W", "RR_mulog_P"), 
                            names_to = "RR_mulog_treatment", values_to = "RR_mulog_values")

RR_wp_sigmalog <- pivot_longer(RR_ab_rich, cols = c("RR_sigmalog_W", "RR_sigmalog_P"), 
                            names_to = "RR_sigmalog_treatment", values_to = "RR_sigmalog_values")



ggRRwp <- 
ggarrange(
ggplot(subset(RR_wp_ab, treatment == "wp"), aes(x = sampling, y = RR_ab_values, fill = RR_ab_treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "loggRR(abundance)") +
  facet_grid(~ RR_ab_treatment) + 
  scale_fill_manual(values = c("RR_abundance_W" = "pink", "RR_abundance_P" = "seagreen2"))+
  geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),

ggplot(subset(RR_wp_rich, treatment == "wp"), aes(x = sampling, y = RR_rich_values, fill = RR_rich_treatment)) +
  geom_boxplot() +
  labs(x = " ", y = "logRR(richness)") +
  facet_grid(~ RR_rich_treatment) + 
  scale_fill_manual(values = c("RR_richness_W" = "pink", "RR_richness_P" = "seagreen2"))+
  geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),

nrow = 2, ncol = 1)


ggRRwp_evenness <- 
  ggarrange(
    ggplot(subset(RR_wp_yzipf, treatment == "wp"), aes(x = sampling, y = RR_yzipf_values, fill = RR_yzipf_treatment)) +
      geom_boxplot() +
      labs(x = " ", y = "loggRR(Y)") +
      facet_grid(~ RR_yzipf_treatment) + 
      scale_fill_manual(values = c("RR_yzipf_W" = "pink", "RR_yzipf_P" = "seagreen2"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
    
    ggplot(subset(RR_wp_mulog, treatment == "wp"), aes(x = sampling, y = RR_mulog_values, fill = RR_mulog_treatment)) +
      geom_boxplot() +
      labs(x = " ", y = "logRR(mu)") +
      facet_grid(~ RR_mulog_treatment) + 
      scale_fill_manual(values = c("RR_mulog_W" = "pink", "RR_mulog_P" = "seagreen2"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
    
    ggplot(subset(RR_wp_sigmalog, treatment == "wp"), aes(x = sampling, y = RR_sigmalog_values, fill = RR_sigmalog_treatment)) +
      geom_boxplot() +
      labs(x = " ", y = "logRR(sigma)") +
      facet_grid(~ RR_sigmalog_treatment) + 
      scale_fill_manual(values = c("RR_sigmalog_W" = "pink", "RR_sigmalog_P" = "seagreen2"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
    
    nrow = 3, ncol = 1)


# COEFFICIENT OF VARIATION: CV = Standard deviation(x) / mean(x) ##########



for (i in 1:length(samps)) {
  subset_c <- subset(mean_sd_abrich_dynamics, sampling == samps[i] & treatment == "c")
  mean_sd_abrich_dynamics$ref_c_cv[mean_sd_abrich_dynamics$sampling == samps[i]] <- mean(subset_c$cv_richness)
  
}


RR_ref_c_cv <- mean_sd_abrich_dynamics %>%
  filter(!treatment %in% "c")
RR_ref_c_cv$logRR_cv <- round(log(RR_ref_c_cv$cv_richness / RR_ref_c_cv$ref_c_cv), 2)

treatment.labs <- c("Warming", "Perturbation", "Warming and perturbation")
names(treatment.labs) <- c("w", "p", "wp")

ggplot(RR_ref_c_cv, aes(x = as.factor(sampling), y = logRR_cv, group = treatment)) +
  facet_grid(~ treatment, labeller = labeller(treatment = treatment.labs)) +
  geom_path(group = 1, aes(color = treatment), linewidth = 0.8, alpha = 0.5)+
  geom_point(aes(color = treatment), fill = "white", position = position_dodge(width = 0.5),
             size = 2.25, shape = 21, alpha = 0.5) +
  geom_smooth(se = F, aes(color = treatment, fill = treatment)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.7) +
  #scale_colour_manual(values = met.brewer("VanGogh2", 3)) +
  scale_colour_manual(values = c("p" = "blue4", "w" = "red3", "wp" = "purple2")) +
  labs(x = "Sampling time", y = "CV richness (log response ratio)") +
  theme_bw() + theme(legend.position = "none",
                     panel.grid = element_blank(),
                     strip.background = element_blank(),
                     strip.text = element_text(face = "bold"),
                     text = element_text(size = 12))




flora_cv <- summarise(group_by(ab_rich_dynamics, sampling, treatment),
                               mean_biomass = mean(biomass, na.rm = T),
                               sd_biomass = sd(biomass, na.rm = T),
                               mean_richness = mean(richness, na.rm = T),
                               sd_richness = sd(richness, na.rm = T),
                               mean_abundance = mean(abundance, na.rm = T), 
                               sd_abundance = sd(abundance, na.rm = T), 
                               mean_yzipf = mean(Y_zipf, na.rm = T), 
                               sd_yzipf = sd(Y_zipf, na.rm = T), 
                               mean_mulog = mean(mu_log, na.rm = T), 
                               sd_mulog = sd(mu_log, na.rm = T), 
                               mean_sigmalog = mean(sigma_log, na.rm = T), 
                               sd_sigmalog = sd(sigma_log, na.rm = T))

flora_cv$CV_biomass <- round(flora_cv$sd_biomass/flora_cv$mean_biomass, 2) 
flora_cv$CV_richness <- round(flora_cv$sd_richness/flora_cv$mean_richness, 2)
flora_cv$CV_abundance <- round(flora_cv$sd_abundance/flora_cv$mean_abundance, 2)
flora_cv$CV_yzipf <- round(flora_cv$sd_yzipf/flora_cv$mean_yzipf, 2)
flora_cv$CV_mulog <- round(flora_cv$sd_mulog/flora_cv$mean_mulog, 2)
flora_cv$CV_sigmalog <- round(flora_cv$sd_sigmalog/flora_cv$mean_sigmalog, 2)



ggCVgrid <- 
  ggarrange(
    
    ggplot(flora_cv, aes(x = sampling, y = CV_abundance, color = treatment, group = treatment)) +
      geom_point() +
      geom_line() +
      geom_smooth(se = F)+
      facet_grid(~ treatment)+
      labs(x = " ", y = "CV abundance") + 
      scale_color_manual(values = c("c" = "green4", "p" = "blue4", "w" = "red4", "wp" = "purple2"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8),
    
    ggplot(flora_cv, aes(x = sampling, y = CV_richness, color = treatment, group = treatment)) +
      geom_point() +
      geom_line() +
      geom_smooth(se = F)+
      facet_grid(~ treatment)+
      labs(x = " ", y = "CV richness ") + 
      scale_color_manual(values = c("c" = "green4", "p" = "blue4", "w" = "red4", "wp" = "purple2"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8),
    
    nrow = 2, ncol = 1)



ggCVgrid_evenness <- 
  ggarrange(
    
    ggplot(flora_cv, aes(x = sampling, y = CV_yzipf, color = treatment, group = treatment)) +
      geom_point() +
      geom_line() +
      geom_smooth(se = F)+
      facet_grid(~ treatment)+
      labs(x = " ", y = "CV yzipf") + 
      scale_color_manual(values = c("c" = "green4", "p" = "blue4", "w" = "red4", "wp" = "purple2"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8),
    
    ggplot(flora_cv, aes(x = sampling, y = CV_mulog, color = treatment, group = treatment)) +
      geom_point() +
      geom_line() +
      geom_smooth(se = F)+
      facet_grid(~ treatment)+
      labs(x = " ", y = "CV mulog") + 
      scale_color_manual(values = c("c" = "green4", "p" = "blue4", "w" = "red4", "wp" = "purple2"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8),
    
    ggplot(flora_cv, aes(x = sampling, y = CV_sigmalog, color = treatment, group = treatment)) +
      geom_point() +
      geom_line() +
      geom_smooth(se = F)+
      facet_grid(~ treatment)+
      labs(x = " ", y = "CV sigma") + 
      scale_color_manual(values = c("c" = "green4", "p" = "blue4", "w" = "red4", "wp" = "purple2"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8),
    
    nrow = 3, ncol = 1)
 
# Response ratio of CV  

for (i in 1:length(samps)) {
  subset_c <- subset(flora_cv, sampling == samps[i] & treatment == "c")
  subset_w <- subset(flora_cv, sampling == samps[i] & treatment == "w")
  subset_p <- subset(flora_cv, sampling == samps[i] & treatment == "p")
  subset_wp <- subset(flora_cv, sampling == samps[i] & treatment == "wp")
 
  
   flora_cv$RR_CV_abundance_C[flora_cv$sampling == samps[i]] <-
    round(log(flora_cv$CV_abundance[flora_cv$sampling == samps[i]] /mean(subset_c$CV_abundance)),2)
   flora_cv$RR_CV_abundance_W[flora_cv$sampling == samps[i]] <-
     round(log(flora_cv$CV_abundance[flora_cv$sampling == samps[i]]/mean(subset_w$CV_abundance)),2)
   flora_cv$RR_CV_abundance_P[flora_cv$sampling == samps[i]] <-
     round(log(flora_cv$CV_abundance[flora_cv$sampling == samps[i]]/mean(subset_p$CV_abundance)),2)
   flora_cv$RR_CV_abundance_WP[flora_cv$sampling == samps[i]] <-
     round(log(flora_cv$CV_abundance[flora_cv$sampling == samps[i]]/mean(subset_wp$CV_abundance)),2)
   
   flora_cv$RR_CV_richness_C[flora_cv$sampling == samps[i]] <-
     round(log(flora_cv$CV_richness[flora_cv$sampling == samps[i]]/mean(subset_c$CV_richness)),2)
   flora_cv$RR_CV_richness_W[flora_cv$sampling == samps[i]] <-
     round(log(flora_cv$CV_richness[flora_cv$sampling == samps[i]]/mean(subset_w$CV_richness)),2)
   flora_cv$RR_CV_richness_P[flora_cv$sampling == samps[i]] <-
     round(log(flora_cv$CV_richness[flora_cv$sampling == samps[i]]/mean(subset_p$CV_richness)),2)
   flora_cv$RR_CV_richness_WP[flora_cv$sampling == samps[i]] <-
     round(log(flora_cv$CV_richness[flora_cv$sampling == samps[i]]/mean(subset_wp$CV_richness)),2)
   
   flora_cv$RR_CV_yzipf_C[flora_cv$sampling == samps[i]] <-
     round(log(flora_cv$CV_yzipf[flora_cv$sampling == samps[i]]/mean(subset_c$CV_yzipf)),2)
   flora_cv$RR_CV_yzipf_W[flora_cv$sampling == samps[i]] <-
     round(log(flora_cv$CV_yzipf[flora_cv$sampling == samps[i]]/mean(subset_w$CV_yzipf)),2)
   flora_cv$RR_CV_yzipf_P[flora_cv$sampling == samps[i]] <-
     round(log(flora_cv$CV_yzipf[flora_cv$sampling == samps[i]]/mean(subset_p$CV_yzipf)),2)
   flora_cv$RR_CV_yzipf_WP[flora_cv$sampling == samps[i]] <-
     round(log(flora_cv$CV_yzipf[flora_cv$sampling == samps[i]]/mean(subset_wp$CV_yzipf)),2)
   
   flora_cv$RR_CV_mulog_C[flora_cv$sampling == samps[i]] <-
     round(log(flora_cv$CV_mulog[flora_cv$sampling == samps[i]]/mean(subset_c$CV_mulog)),2)
   flora_cv$RR_CV_mulog_W[flora_cv$sampling == samps[i]] <-
     round(log(flora_cv$CV_mulog[flora_cv$sampling == samps[i]]/mean(subset_w$CV_mulog)),2)
   flora_cv$RR_CV_mulog_P[flora_cv$sampling == samps[i]] <-
     round(log(flora_cv$CV_mulog[flora_cv$sampling == samps[i]]/mean(subset_p$CV_mulog)),2)
   flora_cv$RR_CV_mulog_WP[flora_cv$sampling == samps[i]] <-
     round(log(flora_cv$CV_mulog[flora_cv$sampling == samps[i]]/mean(subset_wp$CV_mulog)),2)
   
   flora_cv$RR_CV_sigmalog_C[flora_cv$sampling == samps[i]] <-
     round(log(flora_cv$CV_sigmalog[flora_cv$sampling == samps[i]]/mean(subset_c$CV_sigmalog)),2)
   flora_cv$RR_CV_sigmalog_W[flora_cv$sampling == samps[i]] <-
     round(log(flora_cv$CV_sigmalog[flora_cv$sampling == samps[i]]/mean(subset_w$CV_sigmalog)),2)
   flora_cv$RR_CV_sigmalog_P[flora_cv$sampling == samps[i]] <-
     round(log(flora_cv$CV_sigmalog[flora_cv$sampling == samps[i]]/mean(subset_p$CV_sigmalog)),2)
   flora_cv$RR_CV_sigmalog_WP[flora_cv$sampling == samps[i]] <-
     round(log(flora_cv$CV_sigmalog[flora_cv$sampling == samps[i]]/mean(subset_wp$CV_sigmalog)),2)
   
  rm(subset_c)
  rm(subset_w)
  rm(subset_p)
  rm(subset_wp)
}


ggRRcv <- 
ggarrange(
  ggplot(flora_cv[flora_cv$treatment != "c", ], aes(x = sampling, y = RR_CV_abundance_C, color = treatment, group = treatment)) +
    geom_point() +
    geom_line() +
    geom_smooth(se = F)+ #use method = "loess" by default
    facet_grid(~ treatment)+
    labs(x = " ", y = "logRR(CV abundance)") + 
    scale_color_manual(values = c("c" = "green4", "p" = "blue3", "w" = "red4", "wp" = "purple2"))+
    geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
  
ggplot(flora_cv[flora_cv$treatment != "c", ], aes(x = sampling, y = RR_CV_richness_C, color = treatment, group = treatment)) +
  geom_point() +
  geom_line() +
  geom_smooth(se = F)+
  facet_grid(~ treatment)+
  labs(x = " ", y = "logRR(CV richness)") + 
  scale_color_manual(values = c("c" = "green4", "p" = "blue3", "w" = "red4", "wp" = "purple2"))+
  geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),

nrow=2, ncol=1)


ggRRcv_eveness <- 
  ggarrange(
    ggplot(flora_cv[flora_cv$treatment != "c", ], aes(x = sampling, y = RR_CV_yzipf_C, color = treatment, group = treatment)) +
      geom_point() +
      geom_line() +
      geom_smooth(se = F)+
      facet_grid(~ treatment)+
      labs(x = " ", y = "logRR(CV yzipf)") + 
      scale_color_manual(values = c("c" = "green4", "p" = "blue3", "w" = "red4", "wp" = "purple2"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
    
    ggplot(flora_cv[flora_cv$treatment != "c", ], aes(x = sampling, y = RR_CV_mulog_C, color = treatment, group = treatment)) +
      geom_point() +
      geom_line() +
      geom_smooth(se = F)+
      facet_grid(~ treatment)+
      labs(x = " ", y = "logRR(CV mulog)") + 
      scale_color_manual(values = c("c" = "green4", "p" = "blue3", "w" = "red4", "wp" = "purple2"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
    
    ggplot(flora_cv[flora_cv$treatment != "c", ], aes(x = sampling, y = RR_CV_sigmalog_C, color = treatment, group = treatment)) +
      geom_point() +
      geom_line() +
      geom_smooth(se = F)+
      facet_grid(~ treatment)+
      labs(x = " ", y = "logRR(CV sigmalog)") + 
      scale_color_manual(values = c("c" = "green4", "p" = "blue3", "w" = "red4", "wp" = "purple2"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
    
    nrow=3, ncol=1)
  
  
# LogRR (CV) Reference = w and p. Comparison with wp

RR_cv_wp_ab <- pivot_longer(flora_cv, cols = c("RR_CV_abundance_W", "RR_CV_abundance_P"), 
                            names_to = "RR_ab_treatment", values_to = "RR_ab_values")

RR_cv_wp_rich <- pivot_longer(flora_cv, cols = c("RR_CV_richness_W", "RR_CV_richness_P"), 
                              names_to = "RR_rich_treatment", values_to = "RR_rich_values")



RR_cv_wp_mulog <- pivot_longer(flora_cv, cols = c("RR_CV_mulog_W", "RR_CV_mulog_P"), 
                            names_to = "RR_mulog_treatment", values_to = "RR_mulog_values")

RR_cv_wp_sigmalog <- pivot_longer(flora_cv, cols = c("RR_CV_sigmalog_W", "RR_CV_sigmalog_P"), 
                               names_to = "RR_sigmalog_treatment", values_to = "RR_sigmalog_values")


ggRRcv_wp <- 
ggarrange(
  ggplot(subset(RR_cv_wp_ab, treatment == "wp"), aes(x = sampling, y = RR_ab_values, color = RR_ab_treatment, group = RR_ab_treatment)) +
    geom_point() +
    geom_line() + 
    geom_smooth(se = F) +
    labs(x = " ", y = "logRR(CV abundance)") +
    facet_grid(~ RR_ab_treatment) + 
    scale_color_manual(values = c("RR_CV_abundance_W" = "pink4", "RR_CV_abundance_P" = "seagreen4"))+
    geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
  
  ggplot(subset(RR_cv_wp_rich, treatment == "wp"), aes(x = sampling, y = RR_rich_values, color = RR_rich_treatment, group = RR_rich_treatment)) +
    geom_point() +
    geom_line() +
    geom_smooth(se = F) +
    labs(x = " ", y = "logRR(CV richness)") +
    facet_grid(~ RR_rich_treatment) + 
    scale_color_manual(values = c("RR_CV_richness_W" = "pink4", "RR_CV_richness_P" = "seagreen4"))+
    geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
  
  nrow = 2, ncol = 1)

ggRRcv_wp_evenness <- 
  ggarrange(
    ggplot(subset(RR_cv_wp_yzipf, treatment == "wp"), aes(x = sampling, y = RR_yzipf_values, color = RR_yzipf_treatment, group = RR_yzipf_treatment)) +
      geom_point() +
      geom_line() + 
      geom_smooth(se = F) +
      labs(x = " ", y = "logRR(CV yzipf)") +
      facet_grid(~ RR_yzipf_treatment) + 
      scale_color_manual(values = c("RR_CV_yzipf_W" = "pink4", "RR_CV_yzipf_P" = "seagreen4"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
    
    ggplot(subset(RR_cv_wp_mulog, treatment == "wp"), aes(x = sampling, y = RR_mulog_values, color = RR_mulog_treatment, group = RR_mulog_treatment)) +
      geom_point() +
      geom_line() +
      geom_smooth(se = F) +
      labs(x = " ", y = "logRR(CV mulog)") +
      facet_grid(~ RR_mulog_treatment) + 
      scale_color_manual(values = c("RR_CV_mulog_W" = "pink4", "RR_CV_mulog_P" = "seagreen4"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
    
    ggplot(subset(RR_cv_wp_sigmalog, treatment == "wp"), aes(x = sampling, y = RR_sigmalog_values, color = RR_sigmalog_treatment, group = RR_sigmalog_treatment)) +
      geom_point() +
      geom_line() +
      geom_smooth(se = F) +
      labs(x = " ", y = "logRR(CV sigmalog)") +
      facet_grid(~ RR_sigmalog_treatment) + 
      scale_color_manual(values = c("RR_CV_sigmalog_W" = "pink4", "RR_CV_sigmalog_P" = "seagreen4"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
    
    
    nrow = 3, ncol = 1)


## CV but when CV = sd(x)^2/mean(x)^2 #######

flora_cv$squaredCV_biomass <- round((flora_cv$sd_biomass)^2/(flora_cv$mean_biomass)^2, 2) 
flora_cv$squaredCV_richness <- round((flora_cv$sd_richness)^2/(flora_cv$mean_richness)^2, 2)
flora_cv$squaredCV_abundance <- round((flora_cv$sd_abundance)^2/(flora_cv$mean_abundance)^2, 2)
flora_cv$squaredCV_yzipf <- round((flora_cv$sd_yzipf)^2/(flora_cv$mean_yzipf)^2, 2)  #absolute values 
flora_cv$squaredCV_mulog <- round((flora_cv$sd_mulog)^2/(flora_cv$mean_mulog)^2, 2)
flora_cv$squaredCV_sigmalog <- round((flora_cv$sd_sigmalog)^2/(flora_cv$mean_sigmalog)^2, 2)




ggCVgrid_squared<- 
  ggarrange(
    
    ggplot(flora_cv, aes(x = sampling, y = squaredCV_abundance, color = treatment, group = treatment)) +
      geom_point() +
      geom_line() +
      geom_smooth(se = F)+
      facet_grid(~ treatment)+
      labs(x = " ", y = "CV abundance (squared)") + 
      scale_color_manual(values = c("c" = "green4", "p" = "blue4", "w" = "red4", "wp" = "purple2"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8),
    
    ggplot(flora_cv, aes(x = sampling, y = squaredCV_richness, color = treatment, group = treatment)) +
      geom_point() +
      geom_line() +
      geom_smooth(se = F)+
      facet_grid(~ treatment)+
      labs(x = " ", y = "CV richness (squared)") + 
      scale_color_manual(values = c("c" = "green4", "p" = "blue4", "w" = "red4", "wp" = "purple2"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8),
    
    nrow = 2, ncol = 1)



ggCVgrid_evenness_squared <- 
  ggarrange(
    
    ggplot(flora_cv, aes(x = sampling, y = squaredCV_yzipf, color = treatment, group = treatment)) +
      geom_point() +
      geom_line() +
      geom_smooth(se = F)+
      facet_grid(~ treatment)+
      labs(x = " ", y = "CV yzipf (squared)") + 
      scale_color_manual(values = c("c" = "green4", "p" = "blue4", "w" = "red4", "wp" = "purple2"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8),
    
    ggplot(flora_cv, aes(x = sampling, y = squaredCV_mulog, color = treatment, group = treatment)) +
      geom_point() +
      geom_line() +
      geom_smooth(se = F)+
      facet_grid(~ treatment)+
      labs(x = " ", y = "CV mulog (squared)") + 
      scale_color_manual(values = c("c" = "green4", "p" = "blue4", "w" = "red4", "wp" = "purple2"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8),
    
    ggplot(flora_cv, aes(x = sampling, y = squaredCV_sigmalog, color = treatment, group = treatment)) +
      geom_point() +
      geom_line() +
      geom_smooth(se = F)+
      facet_grid(~ treatment)+
      labs(x = " ", y = "CV sigma (squared)") + 
      scale_color_manual(values = c("c" = "green4", "p" = "blue4", "w" = "red4", "wp" = "purple2"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8),
    
    nrow = 3, ncol = 1)

# Response ratio of CV  

for (i in 1:length(samps)) {
  subset_c <- subset(flora_cv, sampling == samps[i] & treatment == "c")
  subset_w <- subset(flora_cv, sampling == samps[i] & treatment == "w")
  subset_p <- subset(flora_cv, sampling == samps[i] & treatment == "p")
  subset_wp <- subset(flora_cv, sampling == samps[i] & treatment == "wp")
  
  
  flora_cv$RR_squaredCV_abundance_C[flora_cv$sampling == samps[i]] <-
    round(log(flora_cv$squaredCV_abundance[flora_cv$sampling == samps[i]] /mean(subset_c$squaredCV_abundance)),2)
  flora_cv$RR_squaredCV_abundance_W[flora_cv$sampling == samps[i]] <-
    round(log(flora_cv$squaredCV_abundance[flora_cv$sampling == samps[i]]/mean(subset_w$squaredCV_abundance)),2)
  flora_cv$RR_squaredCV_abundance_P[flora_cv$sampling == samps[i]] <-
    round(log(flora_cv$squaredCV_abundance[flora_cv$sampling == samps[i]]/mean(subset_p$squaredCV_abundance)),2)
  flora_cv$RR_squaredCV_abundance_WP[flora_cv$sampling == samps[i]] <-
    round(log(flora_cv$squaredCV_abundance[flora_cv$sampling == samps[i]]/mean(subset_wp$squaredCV_abundance)),2)
  
  flora_cv$RR_squaredCV_richness_C[flora_cv$sampling == samps[i]] <-
    round(log(flora_cv$squaredCV_richness[flora_cv$sampling == samps[i]]/mean(subset_c$squaredCV_richness)),2)
  flora_cv$RR_squaredCV_richness_W[flora_cv$sampling == samps[i]] <-
    round(log(flora_cv$squaredCV_richness[flora_cv$sampling == samps[i]]/mean(subset_w$squaredCV_richness)),2)
  flora_cv$RR_squaredCV_richness_P[flora_cv$sampling == samps[i]] <-
    round(log(flora_cv$squaredCV_richness[flora_cv$sampling == samps[i]]/mean(subset_p$squaredCV_richness)),2)
  flora_cv$RR_squaredCV_richness_WP[flora_cv$sampling == samps[i]] <-
    round(log(flora_cv$squaredCV_richness[flora_cv$sampling == samps[i]]/mean(subset_wp$squaredCV_richness)),2)
  
  flora_cv$RR_squaredCV_yzipf_C[flora_cv$sampling == samps[i]] <-
    round(log(flora_cv$squaredCV_yzipf[flora_cv$sampling == samps[i]]/mean(subset_c$squaredCV_yzipf)),2)
  flora_cv$RR_squaredCV_yzipf_W[flora_cv$sampling == samps[i]] <-
    round(log(flora_cv$squaredCV_yzipf[flora_cv$sampling == samps[i]]/mean(subset_w$squaredCV_yzipf)),2)
  flora_cv$RR_squaredCV_yzipf_P[flora_cv$sampling == samps[i]] <-
    round(log(flora_cv$squaredCV_yzipf[flora_cv$sampling == samps[i]]/mean(subset_p$squaredCV_yzipf)),2)
  flora_cv$RR_squaredCV_yzipf_WP[flora_cv$sampling == samps[i]] <-
    round(log(flora_cv$squaredCV_yzipf[flora_cv$sampling == samps[i]]/mean(subset_wp$squaredCV_yzipf)),2)
  
  flora_cv$RR_squaredCV_mulog_C[flora_cv$sampling == samps[i]] <-
    round(log(flora_cv$squaredCV_mulog[flora_cv$sampling == samps[i]]/mean(subset_c$squaredCV_mulog)),2)
  flora_cv$RR_squaredCV_mulog_W[flora_cv$sampling == samps[i]] <-
    round(log(flora_cv$squaredCV_mulog[flora_cv$sampling == samps[i]]/mean(subset_w$squaredCV_mulog)),2)
  flora_cv$RR_squaredCV_mulog_P[flora_cv$sampling == samps[i]] <-
    round(log(flora_cv$squaredCV_mulog[flora_cv$sampling == samps[i]]/mean(subset_p$squaredCV_mulog)),2)
  flora_cv$RR_squaredCV_mulog_WP[flora_cv$sampling == samps[i]] <-
    round(log(flora_cv$squaredCV_mulog[flora_cv$sampling == samps[i]]/mean(subset_wp$squaredCV_mulog)),2)
  
  flora_cv$RR_squaredCV_sigmalog_C[flora_cv$sampling == samps[i]] <-
    round(log(flora_cv$squaredCV_sigmalog[flora_cv$sampling == samps[i]]/mean(subset_c$squaredCV_sigmalog)),2)
  flora_cv$RR_squaredCV_sigmalog_W[flora_cv$sampling == samps[i]] <-
    round(log(flora_cv$squaredCV_sigmalog[flora_cv$sampling == samps[i]]/mean(subset_w$squaredCV_sigmalog)),2)
  flora_cv$RR_squaredCV_sigmalog_P[flora_cv$sampling == samps[i]] <-
    round(log(flora_cv$squaredCV_sigmalog[flora_cv$sampling == samps[i]]/mean(subset_p$squaredCV_sigmalog)),2)
  flora_cv$RR_squaredCV_sigmalog_WP[flora_cv$sampling == samps[i]] <-
    round(log(flora_cv$squaredCV_sigmalog[flora_cv$sampling == samps[i]]/mean(subset_wp$squaredCV_sigmalog)),2)
  
  rm(subset_c)
  rm(subset_w)
  rm(subset_p)
  rm(subset_wp)
}


ggRRcv_squared <- 
  ggarrange(
    ggplot(flora_cv[flora_cv$treatment != "c", ], aes(x = sampling, y = RR_squaredCV_abundance_C, color = treatment, group = treatment)) +
      geom_point() +
      geom_line() +
      geom_smooth(se = F)+ #use method = "loess" by default
      facet_grid(~ treatment)+
      labs(x = " ", y = "logRR(CV abundance) (squared)") + 
      scale_color_manual(values = c("c" = "green4", "p" = "blue3", "w" = "red4", "wp" = "purple2"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
    
    ggplot(flora_cv[flora_cv$treatment != "c", ], aes(x = sampling, y = RR_squaredCV_richness_C, color = treatment, group = treatment)) +
      geom_point() +
      geom_line() +
      geom_smooth(se = F)+
      facet_grid(~ treatment)+
      labs(x = " ", y = "logRR(CV richness) (squared)") + 
      scale_color_manual(values = c("c" = "green4", "p" = "blue3", "w" = "red4", "wp" = "purple2"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
    
    nrow=2, ncol=1)


ggRRcv_eveness_squared <- 
  ggarrange(
    ggplot(flora_cv[flora_cv$treatment != "c", ], aes(x = sampling, y = RR_squaredCV_yzipf_C, color = treatment, group = treatment)) +
      geom_point() +
      geom_line() +
      geom_smooth(se = F)+
      facet_grid(~ treatment)+
      labs(x = " ", y = "logRR(CV yzipf) (squared)") + 
      scale_color_manual(values = c("c" = "green4", "p" = "blue3", "w" = "red4", "wp" = "purple2"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
    
    ggplot(flora_cv[flora_cv$treatment != "c", ], aes(x = sampling, y = RR_squaredCV_mulog_C, color = treatment, group = treatment)) +
      geom_point() +
      geom_line() +
      geom_smooth(se = F)+
      facet_grid(~ treatment)+
      labs(x = " ", y = "logRR(CV mulog) (squared)") + 
      scale_color_manual(values = c("c" = "green4", "p" = "blue3", "w" = "red4", "wp" = "purple2"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
    
    ggplot(flora_cv[flora_cv$treatment != "c", ], aes(x = sampling, y = RR_squaredCV_sigmalog_C, color = treatment, group = treatment)) +
      geom_point() +
      geom_line() +
      geom_smooth(se = F)+
      facet_grid(~ treatment)+
      labs(x = " ", y = "logRR(CV sigmalog) (squared)") + 
      scale_color_manual(values = c("c" = "green4", "p" = "blue3", "w" = "red4", "wp" = "purple2"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
    
    nrow=3, ncol=1)


# LogRR (CV) Reference = w and p. Comparison with wp

RR_squaredcv_wp_ab <- pivot_longer(flora_cv, cols = c("RR_squaredCV_abundance_W", "RR_squaredCV_abundance_P"), 
                                names_to = "RR_ab_treatment", values_to = "RR_ab_values")

RR_squaredcv_wp_rich <- pivot_longer(flora_cv, cols = c("RR_squaredCV_richness_W", "RR_squaredCV_richness_P"), 
                                  names_to = "RR_rich_treatment", values_to = "RR_rich_values")

RR_squaredcv_wp_yzipf <- pivot_longer(flora_cv, cols = c("RR_squaredCV_yzipf_W", "RR_squaredCV_yzipf_P"), 
                                   names_to = "RR_yzipf_treatment", values_to = "RR_yzipf_values")

RR_squaredcv_wp_mulog <- pivot_longer(flora_cv, cols = c("RR_squaredCV_mulog_W", "RR_squaredCV_mulog_P"), 
                                   names_to = "RR_mulog_treatment", values_to = "RR_mulog_values")

RR_squaredcv_wp_sigmalog <- pivot_longer(flora_cv, cols = c("RR_squaredCV_sigmalog_W", "RR_squaredCV_sigmalog_P"), 
                                      names_to = "RR_sigmalog_treatment", values_to = "RR_sigmalog_values")


ggRRcv_wp_squared <- 
  ggarrange(
    ggplot(subset(RR_squaredcv_wp_ab, treatment == "wp"), aes(x = sampling, y = RR_ab_values, color = RR_ab_treatment, group = RR_ab_treatment)) +
      geom_point() +
      geom_line() + 
      geom_smooth(se = F) +
      labs(x = " ", y = "logRR(CV abundance) (squared)") +
      facet_grid(~ RR_ab_treatment) + 
      scale_color_manual(values = c("RR_squaredCV_abundance_W" = "pink4", "RR_squaredCV_abundance_P" = "seagreen4"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
    
    ggplot(subset(RR_squaredcv_wp_rich, treatment == "wp"), aes(x = sampling, y = RR_rich_values, color = RR_rich_treatment, group = RR_rich_treatment)) +
      geom_point() +
      geom_line() +
      geom_smooth(se = F) +
      labs(x = " ", y = "logRR(CV richness) (squared)") +
      facet_grid(~ RR_rich_treatment) + 
      scale_color_manual(values = c("RR_squaredCV_richness_W" = "pink4", "RR_squaredCV_richness_P" = "seagreen4"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
    
    nrow = 2, ncol = 1)

ggRRcv_wp_evenness_squared <- 
  ggarrange(
    ggplot(subset(RR_squaredcv_wp_yzipf, treatment == "wp"), aes(x = sampling, y = RR_yzipf_values, color = RR_yzipf_treatment, group = RR_yzipf_treatment)) +
      geom_point() +
      geom_line() + 
      geom_smooth(se = F) +
      labs(x = " ", y = "logRR(CV yzipf) (squared)") +
      facet_grid(~ RR_yzipf_treatment) + 
      scale_color_manual(values = c("RR_squaredCV_yzipf_W" = "pink4", "RR_squaredCV_yzipf_P" = "seagreen4"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
    
    ggplot(subset(RR_squaredcv_wp_mulog, treatment == "wp"), aes(x = sampling, y = RR_mulog_values, color = RR_mulog_treatment, group = RR_mulog_treatment)) +
      geom_point() +
      geom_line() +
      geom_smooth(se = F) +
      labs(x = " ", y = "logRR(CV mulog) (squared)") +
      facet_grid(~ RR_mulog_treatment) + 
      scale_color_manual(values = c("RR_squaredCV_mulog_W" = "pink4", "RR_squaredCV_mulog_P" = "seagreen4"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
    
    ggplot(subset(RR_squaredcv_wp_sigmalog, treatment == "wp"), aes(x = sampling, y = RR_sigmalog_values, color = RR_sigmalog_treatment, group = RR_sigmalog_treatment)) +
      geom_point() +
      geom_line() +
      geom_smooth(se = F) +
      labs(x = " ", y = "logRR(CV sigmalog) (squared)") +
      facet_grid(~ RR_sigmalog_treatment) + 
      scale_color_manual(values = c("RR_squaredCV_sigmalog_W" = "pink4", "RR_squaredCV_sigmalog_P" = "seagreen4"))+
      geom_vline(xintercept = 1.5, linetype = "dotted", color = "maroon", size = 0.8) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8),
    
    
    nrow = 3, ncol = 1)

# ALL PLOTS ###############

#ggDynamics
#ggCVgrid
#ggCVgrid_squared
#ggDynamics_evenness
#ggCVgrid_evenness
#ggCVgrid_evenness_squared
#ggS0
#ggRRcontrol
#ggRRcv
#ggRRcv_squared
#ggRRcontrol_evenness
#ggRRcv_eveness
#ggRRcv_eveness_squared
#ggRRwp
#ggRRcv_wp
#ggRRcv_wp_squared
#ggRRwp_evenness
#ggRRcv_wp_evenness
#ggRRcv_wp_evenness_squared








