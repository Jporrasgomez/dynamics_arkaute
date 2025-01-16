
#Añadir muestreos 18 y 19


rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)
pacman::p_load(dplyr,reshape2,tidyverse, lubridate, ggplot2, ggpubr, gridExtra, MetBrewer) #Cargamos los paquetes que necesitamos

#Scripts 
source("code/first_script.R")
radcoeff_df <- read.csv("data/radcoeff_df.csv")

# Organizing database ####

# Database for abundance and richness analysis

ab_rich_dynamics <-  flora %>%
  group_by(sampling, sampling_date, omw_date, date, month, treatment, plot) %>%
  reframe(richness = richness,  #total number of species per plot
          abundance = abundance_total) %>% # total coverage of plot
  distinct(sampling, sampling_date, omw_date, date, month, plot, treatment, richness, abundance)


ab_rich_dynamics <- merge(ab_rich_dynamics, radcoeff_df)   ## Algo pasa aquí. Se reduce la base de datos. REVISAR!

mean_sd_abrich_dynamics<- ab_rich_dynamics %>%
  group_by(treatment, sampling, sampling_date, date, month) %>%
  summarize(mean_richness = mean(richness),
            sd_richness = sd(richness),
            mean_abundance = mean(abundance),
            sd_abundance = sd(abundance),
            mean_Y_zipf = mean(Y_zipf),
            sd_Y_zipf = sd(Y_zipf),
            mean_mu_log = mean(mu_log),
            sd_mu_log = sd(mu_log),
            mean_sigma_log = mean(sigma_log),
            sd_sigma_log = sd(sigma_log))



# Database for biomass

biomass_dynamics <- flora %>%
  filter(!sampling %in% c("0", "1", "2", "12"))  %>%
  rename(biomass = biomass_total) %>%
  group_by(sampling, sampling_date, omw_date,  date, month, treatment, plot) %>%
  reframe(biomass = biomass) %>% # total coverage of plot
  distinct(sampling, sampling_date, omw_date, date, month, plot, treatment, biomass)



mean_sd_biomass_dynamics<- biomass_dynamics %>%
  group_by(treatment, sampling, sampling_date, date, month) %>%
  summarize(mean_biomass = mean(biomass),
            sd_biomass = sd(biomass))


# Data characteristics

hist(ab_rich_dynamics$richness)
hist(ab_rich_dynamics$abundance)
hist(biomass_dynamics$biomass)



# Response ratio ####


RR_ab_rich <- ab_rich_dynamics


#Sampling 2 has several plots with very few species, which generates problems at RADs
# At plot 15, we have only 1 species so there is no RAD
# At plot 13, we have 2 species, so the RAD values are not reliable. Im deleting this data by hand: 
RR_ab_rich$Y_zipf[RR_ab_rich$sampling == "2" & RR_ab_rich$plot == "13"] <- NA
RR_ab_rich$mu_log[RR_ab_rich$sampling == "2" & RR_ab_rich$plot == "13"] <- NA
RR_ab_rich$sigma_log[RR_ab_rich$sampling == "2" & RR_ab_rich$plot == "13"] <- NA


samps <- unique(RR_ab_rich$sampling)

for (i in 1:length(samps)) {
  subset_c <- subset(RR_ab_rich, sampling == samps[i] & treatment == "c")
  RR_ab_rich$ref_c_mean_richness[RR_ab_rich$sampling == samps[i]] <- mean(subset_c$richness)
  RR_ab_rich$ref_c_mean_abundance[RR_ab_rich$sampling == samps[i]] <- mean(subset_c$abundance)
  RR_ab_rich$ref_c_mean_Y_zipf[RR_ab_rich$sampling == samps[i]] <- mean(subset_c$Y_zipf)
  RR_ab_rich$ref_c_mean_mu_log[RR_ab_rich$sampling == samps[i]] <- mean(subset_c$mu_log)
  RR_ab_rich$ref_c_mean_sigma_log[RR_ab_rich$sampling == samps[i]] <- mean(subset_c$sigma_log)
  
  
  subset_w <- subset(RR_ab_rich, sampling == samps[i] & treatment == "w")
  RR_ab_rich$ref_w_mean_richness[RR_ab_rich$sampling == samps[i]] <- mean(subset_w$richness)
  RR_ab_rich$ref_w_mean_abundance[RR_ab_rich$sampling == samps[i]] <- mean(subset_w$abundance)
  RR_ab_rich$ref_w_mean_Y_zipf[RR_ab_rich$sampling == samps[i]] <- mean(subset_w$Y_zipf)
  RR_ab_rich$ref_w_mean_mu_log[RR_ab_rich$sampling == samps[i]] <- mean(subset_w$mu_log)
  RR_ab_rich$ref_w_mean_sigma_log[RR_ab_rich$sampling == samps[i]] <- mean(subset_w$sigma_log)
  
  
  subset_p <- subset(RR_ab_rich, sampling == samps[i] & treatment == "p")
  RR_ab_rich$ref_p_mean_richness[RR_ab_rich$sampling == samps[i]] <- mean(subset_p$richness)
  RR_ab_rich$ref_p_mean_abundance[RR_ab_rich$sampling == samps[i]] <- mean(subset_p$abundance)
  RR_ab_rich$ref_p_mean_Y_zipf[RR_ab_rich$sampling == samps[i]] <- mean(subset_p$Y_zipf)
  RR_ab_rich$ref_p_mean_mu_log[RR_ab_rich$sampling == samps[i]] <- mean(subset_p$mu_log)
  RR_ab_rich$ref_p_mean_sigma_log[RR_ab_rich$sampling == samps[i]] <- mean(subset_p$sigma_log)
  
  
  
  rm(subset_c)
  rm(subset_w)
  rm(subset_p)
  
}

#RR_ab_rich contains NA's for the reference of p in sampling 1, due to no pressence of flora in that sampling

RR_ref_c <- RR_ab_rich %>%
  filter(!treatment %in% "c")
RR_ref_c$logRR_abundance <- round(log(RR_ref_c$abundance / RR_ref_c$ref_c_mean_abundance), 2)
RR_ref_c$logRR_richness <- round(log(RR_ref_c$richness / RR_ref_c$ref_c_mean_richness), 2)
RR_ref_c$logRR_Y_zipf <- round(log(RR_ref_c$Y_zipf / RR_ref_c$ref_c_mean_Y_zipf), 2)
RR_ref_c$logRR_mu_log <- round(log(RR_ref_c$mu_log / RR_ref_c$ref_c_mean_mu_log), 2)  # Warning message: Na's produced
RR_ref_c$logRR_sigma_log <- round(log(RR_ref_c$sigma_log / RR_ref_c$ref_c_mean_sigma_log), 2)

mean_sd_data_ref_c <- RR_ref_c %>%
  group_by(treatment, sampling, sampling_date, month) %>%
  summarize(mean_abundance = mean(logRR_abundance),
            sd_abundance = sd(logRR_abundance),
            mean_richness = mean(logRR_richness),
            sd_richness = sd(logRR_richness),
            mean_Y_zipf = mean(logRR_Y_zipf),
            sd_Y_zipf = sd(logRR_Y_zipf),
            mean_mu_log = mean(logRR_mu_log),
            sd_mu_log = sd(logRR_mu_log),
            mean_sigma_log= mean(logRR_sigma_log),
            sd_sigma_log = sd(logRR_sigma_log))


RR_ref_w <- RR_ab_rich %>%
  filter(!treatment %in% c("w", "c", "p"))
RR_ref_w$logRR_abundance <- round(log(RR_ref_w$abundance / RR_ref_w$ref_w_mean_abundance), 2)
RR_ref_w$logRR_richness <- round(log(RR_ref_w$richness / RR_ref_w$ref_w_mean_richness), 2)
RR_ref_w$logRR_Y_zipf <- round(log(RR_ref_w$Y_zipf / RR_ref_w$ref_w_mean_Y_zipf), 2)
RR_ref_w$logRR_mu_log <- round(log(RR_ref_w$mu_log / RR_ref_w$ref_w_mean_mu_log), 2)
RR_ref_w$logRR_sigma_log <- round(log(RR_ref_w$sigma_log / RR_ref_w$ref_w_mean_sigma_log), 2)

mean_sd_data_ref_w <- RR_ref_w %>%
  group_by(treatment, sampling, sampling_date, month) %>%
  summarize(mean_abundance = mean(logRR_abundance),
            sd_abundance = sd(logRR_abundance),
            mean_richness = mean(logRR_richness),
            sd_richness = sd(logRR_richness),
            mean_Y_zipf = mean(logRR_Y_zipf),
            sd_Y_zipf = sd(logRR_Y_zipf),
            mean_mu_log = mean(logRR_mu_log),
            sd_mu_log = sd(logRR_mu_log),
            mean_sigma_log= mean(logRR_sigma_log),
            sd_sigma_log = sd(logRR_sigma_log))


RR_ref_p <- RR_ab_rich %>%
  filter(!treatment %in% c("w", "c", "p"))
RR_ref_p$logRR_abundance <- round(log(RR_ref_p$abundance / RR_ref_p$ref_p_mean_abundance), 2)
RR_ref_p$logRR_richness <- round(log(RR_ref_p$richness / RR_ref_p$ref_p_mean_richness), 2)
RR_ref_p$logRR_Y_zipf <- round(log(RR_ref_p$Y_zipf / RR_ref_p$ref_p_mean_Y_zipf), 2)
RR_ref_p$logRR_mu_log <- round(log(RR_ref_p$mu_log / RR_ref_p$ref_p_mean_mu_log), 2)
RR_ref_p$logRR_sigma_log <- round(log(RR_ref_p$sigma_log / RR_ref_p$ref_p_mean_sigma_log), 2)

mean_sd_data_ref_p <- RR_ref_p %>%
  group_by(treatment, sampling, sampling_date, month) %>%
  summarize(mean_abundance = mean(logRR_abundance),
            sd_abundance = sd(logRR_abundance),
            mean_richness = mean(logRR_richness),
            sd_richness = sd(logRR_richness),
            mean_Y_zipf = mean(logRR_Y_zipf),
            sd_Y_zipf = sd(logRR_Y_zipf),
            mean_mu_log = mean(logRR_mu_log),
            sd_mu_log = sd(logRR_mu_log),
            mean_sigma_log= mean(logRR_sigma_log),
            sd_sigma_log = sd(logRR_sigma_log))


RR_biomass <- biomass_dynamics

samps_biomass <- unique(biomass_dynamics$sampling)

for (i in 1:length(samps_biomass)) {
  subset_c <- subset(RR_biomass, sampling == samps_biomass[i] & treatment == "c")
  RR_biomass$ref_c_mean_biomass[RR_biomass$sampling == samps_biomass[i]] <- mean(subset_c$biomass)
  
  subset_w <- subset(RR_biomass, sampling == samps_biomass[i] & treatment == "w")
  RR_biomass$ref_w_mean_biomass[RR_biomass$sampling == samps_biomass[i]] <- mean(subset_w$biomass)
  
  subset_p <- subset(RR_biomass, sampling == samps_biomass[i] & treatment == "p")
  RR_biomass$ref_p_mean_biomass[RR_biomass$sampling == samps_biomass[i]] <- mean(subset_p$biomass)
  
}



RR_biomass_ref_c <- RR_biomass %>%
  filter(!treatment %in% "c")
RR_biomass_ref_c$logRR_biomass <- round(log(RR_biomass_ref_c$biomass / RR_biomass_ref_c$ref_c_mean_biomass), 2)
mean_sd_biomass_ref_c <- RR_biomass_ref_c %>%
  group_by(treatment, sampling, sampling_date, month) %>%
  summarize(mean_biomass = mean(logRR_biomass),
            sd_biomass = sd(logRR_biomass))


RR_biomass_ref_w <- RR_biomass %>%
  filter(!treatment %in% c( "w", "c", "p"))
RR_biomass_ref_w$logRR_biomass <- round(log(RR_biomass_ref_w$biomass / RR_biomass_ref_w$ref_w_mean_biomass), 2)
mean_sd_biomass_ref_w <- RR_biomass_ref_w %>%
  group_by(treatment, sampling, sampling_date, month) %>%
  summarize(mean_biomass = mean(logRR_biomass),
            sd_biomass = sd(logRR_biomass))

RR_biomass_ref_p <- RR_biomass %>%
  filter(!treatment %in% c("p", "c", "w"))
RR_biomass_ref_p$logRR_biomass <- round(log(RR_biomass_ref_p$biomass / RR_biomass_ref_p$ref_p_mean_biomass), 2)
mean_sd_biomass_ref_p <- RR_biomass_ref_p %>%
  group_by(treatment, sampling, sampling_date, month) %>%
  summarize(mean_biomass = mean(logRR_biomass),
            sd_biomass = sd(logRR_biomass))


# CV and LogRR of CV #####

mean_sd_abrich_dynamics$cv_richness <- mean_sd_abrich_dynamics$sd_richness /mean_sd_abrich_dynamics$mean_richness 
mean_sd_abrich_dynamics$cv_abundance <- mean_sd_abrich_dynamics$sd_abundance /mean_sd_abrich_dynamics$mean_abundance 
mean_sd_abrich_dynamics$cv_Y_zipf <- mean_sd_abrich_dynamics$sd_Y_zipf /mean_sd_abrich_dynamics$mean_Y_zipf
mean_sd_abrich_dynamics$cv_sigma_log <- mean_sd_abrich_dynamics$sd_sigma_log /mean_sd_abrich_dynamics$mean_sigma_log
mean_sd_abrich_dynamics$cv_mu_log <- mean_sd_abrich_dynamics$sd_mu_log /mean_sd_abrich_dynamics$mean_mu_log

mean_sd_biomass_dynamics$cv_biomass <- mean_sd_biomass_dynamics$sd_biomass / mean_sd_biomass_dynamics$mean_biomass



for (i in 1:length(samps)) {
  subset_c <- subset(mean_sd_abrich_dynamics, sampling == samps[i] & treatment == "c")
  mean_sd_abrich_dynamics$ref_c_cv_richness[mean_sd_abrich_dynamics$sampling == samps[i]] <- mean(subset_c$cv_richness)
  mean_sd_abrich_dynamics$ref_c_cv_abundance[mean_sd_abrich_dynamics$sampling == samps[i]] <- mean(subset_c$cv_abundance)
  mean_sd_abrich_dynamics$ref_c_cv_Y_zipf[mean_sd_abrich_dynamics$sampling == samps[i]] <- mean(subset_c$cv_Y_zipf)
  mean_sd_abrich_dynamics$ref_c_cv_mu_log[mean_sd_abrich_dynamics$sampling == samps[i]] <- mean(subset_c$cv_mu_log)
  mean_sd_abrich_dynamics$ref_c_cv_sigma_log[mean_sd_abrich_dynamics$sampling == samps[i]] <- mean(subset_c$cv_sigma_log)
  
  rm(subset_c)
}

logRR_cv_dynamics <- mean_sd_abrich_dynamics %>%
  filter(!treatment %in% "c")
logRR_cv_dynamics$logRR_cv_richness <- log(logRR_cv_dynamics$cv_richness/logRR_cv_dynamics$ref_c_cv_richness)
logRR_cv_dynamics$logRR_cv_abundance <- log(logRR_cv_dynamics$cv_abundance/logRR_cv_dynamics$ref_c_cv_abundance)
logRR_cv_dynamics$logRR_cv_Y_zipf <- log(logRR_cv_dynamics$cv_Y_zipf/logRR_cv_dynamics$ref_c_cv_Y_zipf)
logRR_cv_dynamics$logRR_cv_sigma_log <- log(logRR_cv_dynamics$cv_sigma_log/logRR_cv_dynamics$ref_c_cv_sigma_log)
logRR_cv_dynamics$logRR_cv_mu_log <- log(logRR_cv_dynamics$cv_mu_log/logRR_cv_dynamics$ref_c_cv_mu_log)




for (i in 1:length(samps_biomass)){
  
  subset_c <- subset(mean_sd_biomass_dynamics, sampling == samps_biomass[i] & treatment == "c")
  mean_sd_biomass_dynamics$ref_c_cv_biomass[mean_sd_biomass_dynamics$sampling == samps_biomass[i]] <- mean(subset_c$cv_biomass)
  
  rm(subset_c)
}

logRR_cv_biomass <- mean_sd_biomass_dynamics %>%
  filter(!treatment %in% "c")

logRR_cv_biomass$logRR_cv_biomass <-log(logRR_cv_biomass$cv_biomass / logRR_cv_biomass$ref_c_cv_biomass)
  
# Data visualization ####
#Añadir linea entre 0 y 1 

types <- c("richness", "abundance", "sigma_log", "mu_log", "Y_zipf", "biomass")

# Dynamics
source("code/plots_functions_flora/plot_dynamics.R") 


ggdynamics_richness <- plot_dynamics(ab_rich_dynamics, mean_sd_abrich_dynamics, types[1])
ggdynamics_abundance <- plot_dynamics(ab_rich_dynamics, mean_sd_abrich_dynamics, types[2])
ggdynamics_sigmalog <- plot_dynamics(ab_rich_dynamics, mean_sd_abrich_dynamics, types[3])
ggdynamics_mulog <- plot_dynamics(ab_rich_dynamics, mean_sd_abrich_dynamics, types[4])
ggdynamics_yzipf <- plot_dynamics(ab_rich_dynamics, mean_sd_abrich_dynamics, types[5])
ggdynamics_biomass <- plot_dynamics(biomass_dynamics, mean_sd_biomass_dynamics, types[6])


# Response ratio dynamics
source("code/plots_functions_flora/plot_logRR.R")

gglogRR_richness <- plot_logRR(RR_ref_c, mean_sd_data_ref_c, types[1])
gglogRR_abundance <- plot_logRR(RR_ref_c, mean_sd_data_ref_c, types[2])
gglogRR_sigmalog <- plot_logRR(RR_ref_c, mean_sd_data_ref_c, types[3]) #Quedan datos sueltos
gglogRR_mulog <- plot_logRR(RR_ref_c, mean_sd_data_ref_c, types[4]) #Quedan datos sueltos
gglogRR_yzipf <- plot_logRR(RR_ref_c, mean_sd_data_ref_c, types[5]) #Quedan datos sueltos
gglogRR_biomass <- plot_logRR(RR_biomass_ref_c, mean_sd_biomass_ref_c, types[6])

# CV

source("code/plots_functions_flora/plot_dynamics_cv.R")

ggcv_richness <- plot_dynamics_cv(mean_sd_abrich_dynamics, types[1])
ggcv_abundance <- plot_dynamics_cv(mean_sd_abrich_dynamics, types[2])
ggcv_sigmalog <- plot_dynamics_cv(mean_sd_abrich_dynamics, types[3])
ggcv_mulog <- plot_dynamics_cv(mean_sd_abrich_dynamics, types[4])
ggcv_yzipf <- plot_dynamics_cv(mean_sd_abrich_dynamics, types[5])
ggcv_biomass <- plot_dynamics_cv(mean_sd_biomass_dynamics, types[6])

# Log response ratio of CV

source("code/plots_functions_flora/plot_logRR_cv.R")

gglogRRcv_richness <- plot_logRR_cv(logRR_cv_dynamics, types[1])
gglogRRcv_abundance <- plot_logRR_cv(logRR_cv_dynamics, types[2])
gglogRRcv_sigmalog <- plot_logRR_cv(logRR_cv_dynamics, types[3])
gglogRRcv_mulog <- plot_logRR_cv(logRR_cv_dynamics, types[4])
gglogRRcv_yzipf <- plot_logRR_cv(logRR_cv_dynamics, types[5])
gglogRRcv_biomass <- plot_logRR_cv(logRR_cv_biomass, types[6])


#Trying to generate the big plot

abrich_ref_c <- RR_ref_c %>%
  select(c(plot, treatment, sampling_date, month, starts_with("logRR_")))

biomass_ref_c <- RR_biomass_ref_c %>%
  select(c(plot, treatment, sampling_date, month, logRR_biomass))

RR_big_raw <- full_join(abrich_ref_c, biomass_ref_c)

RR_long <- RR_big_raw %>%
  pivot_longer(
    cols = -c(plot, treatment, sampling_date),
    names_to = "logRR_variable",
    values_to = "logRR_value"
  )
  
treatment_labs_dynamics <- c("Control", "Warming", "Perturbation", "Warming and perturbation")
names(treatment_labs_dynamics) <- c("c","w", "p", "wp")
  
ggplot(RR_long, aes( x = logRR_value, y = logRR_variable)) + 
  facet_grid(~ treatment, labeller = labeller(treatment = treatment_labs_dynamics)) +  
  geom_violin( aes(color = "black", fill = treatment)) + 
  stat_summary(fun = "mean", geom = "crossbar", width = 0.5, color = "black", size = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  scale_colour_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) 


 

rr_abrich_summer <- abrich_ref_c %>%
  filter(month %in%c("Jun", "Jul", "Aug"))
rr_biomass_summer <- biomass_ref_c %>%
  filter(month %in%c("Jun", "Jul", "Aug"))
RR_summer <- full_join(rr_abrich_summer, rr_biomass_summer)
RR_long_summer <- RR_summer %>%
  pivot_longer(
    cols = -c(plot, treatment, sampling_date, month),
    names_to = "logRR_variable",
    values_to = "logRR_value"
  )

ggplot(RR_long_summer, aes( x = logRR_value, y = logRR_variable)) + 
  facet_grid(~ treatment, labeller = labeller(treatment = treatment_labs_dynamics)) +  
  geom_violin( aes(color = treatment, fill = treatment)) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  scale_colour_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) 


#Removing all elements from the environment but the ggplots

#rm(list = setdiff(ls(), grep("gg", ls(), value = TRUE)))
