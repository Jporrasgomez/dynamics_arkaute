
#rm(list = ls(all.names = TRUE))  #Se limpia el environment
#pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)
#pacman::p_load(dplyr,reshape2,tidyverse, tidyr, lubridate, ggplot2, ggpubr, gridExtra, MetBrewer) #Cargamos los paquetes que necesitamos


 
# Apertura bases de datos y comprobar si son parecidas
# Novogene nos ha dado 4 ficheros (2 por región). La diferencia es el tratamiento de los datos que ellos han hecho, pero los raw data
# se supone que son iguales. Vamos a comprobarlos

fungi_total1 <- read.csv("data/micro/ITS_F001_total.csv")
fungi_total2 <- read.csv("data/micro/ITS_F003_total.csv")

identical(fungi_total1, fungi_total2) #No son idénticos
fungi_diff <- fungi_total1[, sapply(fungi_total1, is.numeric)] - fungi_total2[, sapply(fungi_total1, is.numeric)]
hist(as.vector(t(colSums(fungi_diff, na.rm = TRUE)))) # Distribución de diferencias totales. 



prokariota_total1 <- read.csv("data/micro/16S_F002_total.csv")
prokariota_total2 <- read.csv("data/micro/16S_F004_total.csv")

identical(prokariota_total1, prokariota_total2)
prokariota_diff <- prokariota_total1[, sapply(prokariota_total1, is.numeric)] - prokariota_total2[, sapply(prokariota_total1, is.numeric)]
hist(as.vector(t(colSums(prokariota_diff, na.rm = TRUE)))) # Distribución de diferencias totales. 


rm(fungi_diff); rm(prokariota_diff); rm(fungi_total2); rm(prokariota_total2)

fungi_total <- fungi_total1
prokariota_total <- prokariota_total1 

rm(fungi_total1); rm(prokariota_total1)

# Trabajo con bases de datos

label_plots <- read.csv("data/micro/labels_plots.csv") %>%
  select(plot, dna_epp_FINAL_LABEL_BACTERIA_BATCH, dna_epp_FINAL_LABEL_FUNGHI_BATCH)
plots <- read.csv("data/plots.csv") %>%
  select(plot, treatment) 

sampling_dates <- read.csv("data/sampling_dates.csv") %>%
  select(date, sampling, label_micro)%>%
  rename(micro_sampling = label_micro)%>%
  filter(!is.na(micro_sampling))

sampling_dates$sampling <- as.factor(sampling_dates$sampling)
sampling_dates$date <-  ymd(sampling_dates$date)
sampling_dates$month <- month(sampling_dates$date, label = TRUE)
sampling_dates$day <- day(sampling_dates$date)
sampling_dates$year <- year(sampling_dates$date)


label_plots <- merge(label_plots, plots, by = "plot") %>%
  mutate(micro_sampling = substr(dna_epp_FINAL_LABEL_BACTERIA_BATCH, 1, 1))  # Extract the first letter

micro_samplings_data <- merge(label_plots, sampling_dates, by = "micro_sampling")
rm(sampling_dates); rm(label_plots); rm(plots)

fungi_samplings_data <- micro_samplings_data %>%
  select(!(dna_epp_FINAL_LABEL_BACTERIA_BATCH))%>%
  rename(sample = dna_epp_FINAL_LABEL_FUNGHI_BATCH)

prokariota_samplings_data <- micro_samplings_data %>%
  select(!(dna_epp_FINAL_LABEL_FUNGHI_BATCH))%>%
  rename(sample = dna_epp_FINAL_LABEL_BACTERIA_BATCH)




# fungi REGION BATCH ####

fungi_total0 <- fungi_total %>%
  pivot_longer(
    cols = -c(Taxonomy, X.OTU_num),   # Keep Taxonomy and X.OTU_num unchanged
    names_to = "sample",              # Name for the new "variable" column
    values_to = "abundance"               # Name for the new "value" column
  ) 


fungi_total0 <- merge(fungi_total0, fungi_samplings_data, by = "sample") %>%
  rename(ASV_num = X.OTU_num)

fungi_total0 <- fungi_total0 %>% #Deletting all rowns where abundance = 0
  filter(!abundance == 0)

fungi_total0$ASV_num <- as.factor(fungi_total0$ASV_num)
fungi_total0$treatment <- as.factor(fungi_total0$treatment)
fungi_total0$plot <- as.factor(fungi_total0$plot)


fungi_total0 <- fungi_total0 %>%
  group_by(plot, treatment, sampling, micro_sampling, date, month, day, year) %>%
  summarise(abundance = sum(abundance),
            richness = sum(n_distinct(ASV_num)))%>%
  distinct(plot, sampling, treatment, micro_sampling, date, month, day, year, abundance, richness)

mean_sd_fungi <- fungi_total0 %>%
  group_by(treatment, sampling) %>%
  summarize(mean_richness = mean(richness),
            sd_richness = sd(richness))


samps <- unique(fungi_total0$micro_sampling)
plots <- unique(fungi_total0$plot)
for(i in 1:length(samps)){ 
  for (j in 1:length(plots)){
    
  result <- sum(subset(fungi_total0, micro_sampling == samps[i] & plot == plots[j])$abundance)
  
  print(result)
  }
} # Loop donde compruebo que todas las abundancias relativas suman casi 1 en cada muestreo (sampling y plot) 


# prokariota REGION BATCH ####


prokariota_total0 <- prokariota_total %>%
  pivot_longer(
    cols = -c(Taxonomy, X.OTU_num),   # Keep Taxonomy and X.OTU_num unchanged
    names_to = "sample",              # Name for the new "variable" column
    values_to = "abundance"               # Name for the new "value" column
  ) 


prokariota_total0 <- merge(prokariota_total0, prokariota_samplings_data, by = "sample") %>%
  rename(ASV_num = X.OTU_num)

prokariota_total0 <- prokariota_total0 %>% #Deletting all rowns where abundance = 0
  filter(!abundance == 0)

prokariota_total0$ASV_num <- as.factor(prokariota_total0$ASV_num)
prokariota_total0$treatment <- as.factor(prokariota_total0$treatment)
prokariota_total0$plot <- as.factor(prokariota_total0$plot)


prokariota_total0 <- prokariota_total0 %>%
  group_by(plot, treatment, sampling, micro_sampling, date, month, day, year) %>%
  summarise(abundance = sum(abundance),
            richness = sum(n_distinct(ASV_num)))%>%
  distinct(plot, sampling, treatment, micro_sampling, date, month, day, year, abundance, richness)

mean_sd_prokariota <- prokariota_total0 %>%
  group_by(treatment, sampling) %>%
  summarize(mean_richness = mean(richness),
            sd_richness = sd(richness))   ## Incluir CV



samps <- unique(prokariota_total0$micro_sampling)
plots <- unique(prokariota_total0$plot)
for(i in 1:length(samps)){ 
  for (j in 1:length(plots)){
    
    result <- sum(subset(prokariota_total0, micro_sampling == samps[i] & plot == plots[j])$abundance)
    
    print(result)
  }
} # Loop donde compruebo que todas las abundancias relativas suman casi 1 en cada muestreo (sampling y plot) 


# Richness data visualization

#theme_set(theme_bw()+ theme(legend.position = "NULL"))

treatment_labs_dynamics <- c("Control", "Warming", "Perturbation", "Warming and perturbation")
names(treatment_labs_dynamics) <- c("c","w", "p", "wp")


ggdynamics_prokariota <- ggplot(mean_sd_prokariota, aes(x = as.factor(sampling), y = mean_richness, group = sampling)) +
  facet_grid(~ treatment, labeller = labeller(treatment = treatment_labs_dynamics)) +
  geom_errorbar(aes(ymin = mean_richness - sd_richness, ymax = mean_richness + sd_richness,
                    color = treatment, alpha = 0.5),
                position = position_dodge(width = 0.5), width = 0.25, size = 0.75, alpha = 0.4) +
  geom_point(data = prokariota_total0, aes(x = as.factor(sampling), y = richness, color = treatment),
             position = position_dodge(width = 0.5), size = 1.5, alpha = 0.2) +
  geom_path(group = 1, aes(color = treatment), linewidth = 0.7)+
  geom_point(aes(color = treatment), fill = "white", position = position_dodge(width = 0.5), size = 2.25, shape = 21) +
  #scale_colour_manual(values = met.brewer("VanGogh3",4)) +
  scale_colour_manual(values = c("c" = "green4", "p" = "blue4", "w" = "red3", "wp" = "purple2")) +
  #scale_x_discrete(breaks = levels(as.factor(mean_sd_prokariota$sampling))[seq(1, length(levels(as.factor(mean_sd_prokariota$sampling))), by = 2)]) +
  labs(x = " ", y = "Prokariota richness (number of ASV)") +
  theme_bw() + theme(legend.position = "none",
                     panel.grid = element_blank(),
                     strip.background = element_blank(),
                     strip.text = element_text(face = "bold"),
                     text = element_text(size = 11))



ggdynamics_fungi <- ggplot(mean_sd_fungi, aes(x = as.factor(sampling), y = mean_richness, group = sampling)) +
  facet_grid(~ treatment, labeller = labeller(treatment = treatment_labs_dynamics)) +
  geom_errorbar(aes(ymin = mean_richness - sd_richness, ymax = mean_richness + sd_richness,
                    color = treatment, alpha = 0.5),
                position = position_dodge(width = 0.5), width = 0.25, size = 0.75, alpha = 0.4) +
  geom_point(data = fungi_total0, aes(x = as.factor(sampling), y = richness, color = treatment),
             position = position_dodge(width = 0.5), size = 1.5, alpha = 0.2) +
  geom_path(group = 1, aes(color = treatment), linewidth = 0.7)+
  geom_point(aes(color = treatment), fill = "white", position = position_dodge(width = 0.5), size = 2.25, shape = 21) +
  #scale_colour_manual(values = met.brewer("VanGogh3",4)) +
  scale_colour_manual(values = c("c" = "green4", "p" = "blue4", "w" = "red3", "wp" = "purple2")) +
  #scale_x_discrete(breaks = levels(as.factor(mean_sd_fungi$sampling))[seq(1, length(levels(as.factor(mean_sd_fungi$sampling))), by = 2)]) +
  labs(x = " ", y = "Fungi richness (number of ASV)") +
  theme_bw() + theme(legend.position = "none",
                     panel.grid = element_blank(),
                     strip.background = element_blank(),
                     strip.text = element_text(face = "bold"),
                     text = element_text(size = 11))


RR_fungi <- fungi_total0
RR_prokariota <- prokariota_total0

samps <- unique(RR_fungi$sampling)

for (i in 1:length(samps)) {
  subset_c <- subset(RR_fungi, sampling == samps[i] & treatment == "c")
  RR_fungi$ref_c_mean_richness[RR_fungi$sampling == samps[i]] <- mean(subset_c$richness)

  subset_w <- subset(RR_fungi, sampling == samps[i] & treatment == "w")
  RR_fungi$ref_w_mean_richness[RR_fungi$sampling == samps[i]] <- mean(subset_w$richness)

  
  subset_p <- subset(RR_fungi, sampling == samps[i] & treatment == "p")
  RR_fungi$ref_p_mean_richness[RR_fungi$sampling == samps[i]] <- mean(subset_p$richness)
  
  
  
  rm(subset_c)
  rm(subset_w)
  rm(subset_p)
  
}



RR_fungi_ref_c <- RR_fungi %>%
  filter(!treatment %in% "c")
RR_fungi_ref_c$logRR_richness <- round(log(RR_fungi_ref_c$richness / RR_fungi_ref_c$ref_c_mean_richness), 2)
mean_sd_fungi_ref_c <- RR_fungi_ref_c %>%
  group_by(treatment, sampling) %>%
  summarize(mean_richness = mean(logRR_richness),
            sd_richness = sd(logRR_richness))


RR_fungi_ref_w <- RR_fungi %>%
  filter(!treatment %in% c( "w", "c", "p"))
RR_fungi_ref_w$logRR_richness <- round(log(RR_fungi_ref_w$richness / RR_fungi_ref_w$ref_w_mean_richness), 2)
mean_sd_fungi_ref_w <- RR_fungi_ref_w %>%
  group_by(treatment, sampling) %>%
  summarize(mean_richness = mean(logRR_richness),
            sd_richness = sd(logRR_richness))

RR_fungi_ref_p <- RR_fungi %>%
  filter(!treatment %in% c( "w", "c", "p"))
RR_fungi_ref_p$logRR_richness <- round(log(RR_fungi_ref_p$richness / RR_fungi_ref_p$ref_p_mean_richness), 2)
mean_sd_fungi_ref_p <- RR_fungi_ref_p %>%
  group_by(treatment, sampling) %>%
  summarize(mean_richness = mean(logRR_richness),
            sd_richness = sd(logRR_richness))

RR.treatment.labs <- c("Warming", "Perturbation", "Warming and perturbation")
names(RR.treatment.labs) <- c("w", "p", "wp")


gglogRR_fungi <- ggplot(mean_sd_fungi_ref_c, aes(x = as.factor(sampling), y = mean_richness, group = sampling)) +
  facet_grid(~ treatment, labeller = labeller(treatment = RR.treatment.labs)) +
  geom_errorbar(aes(ymin = mean_richness - sd_richness, ymax = mean_richness + sd_richness,
                    color = treatment, alpha = 0.5),
                position = position_dodge(width = 0.5), width = 0.25, size = 0.75) +
  geom_point(data = RR_fungi_ref_c, aes(x = as.factor(sampling), y = logRR_richness, color = treatment),
             position = position_dodge(width = 0.5), size = 2, alpha = 0.4) +
  geom_path(group = 1, aes(color = treatment), linewidth = 0.8)+
  geom_point(aes(color = treatment), fill = "white", position = position_dodge(width = 0.5), size = 2.25, shape = 21) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.7) +
  #scale_colour_manual(values = met.brewer("VanGogh2", 3)) +
  scale_colour_manual(values = c("p" = "blue4", "w" = "red3", "wp" = "purple2")) +
  #scale_x_discrete(breaks = levels(as.factor(mean_sd_data_ref_c$sampling))[seq(1, length(levels(as.factor(mean_sd_data_ref_c$sampling))), by = 2)]) +
  labs(x = "Sampling time", y = "Fungi richness (LogRR of ASV number)") +
  theme_bw() + theme(legend.position = "none",
                     panel.grid = element_blank(),
                     strip.background = element_blank(),
                     strip.text = element_text(face = "bold"),
                     text = element_text(size = 12))





for (i in 1:length(samps)) {
subset_c <- subset(RR_prokariota, sampling == samps[i] & treatment == "c")
RR_prokariota$ref_c_mean_richness[RR_prokariota$sampling == samps[i]] <- mean(subset_c$richness)

subset_w <- subset(RR_prokariota, sampling == samps[i] & treatment == "w")
RR_prokariota$ref_w_mean_richness[RR_prokariota$sampling == samps[i]] <- mean(subset_w$richness)


subset_p <- subset(RR_prokariota, sampling == samps[i] & treatment == "p")
RR_prokariota$ref_p_mean_richness[RR_prokariota$sampling == samps[i]] <- mean(subset_p$richness)


rm(subset_c)
rm(subset_w)
rm(subset_p)

}



RR_prokariota_ref_c <- RR_prokariota %>%
  filter(!treatment %in% "c")
RR_prokariota_ref_c$logRR_richness <- round(log(RR_prokariota_ref_c$richness / RR_prokariota_ref_c$ref_c_mean_richness), 2)
mean_sd_prokariota_ref_c <- RR_prokariota_ref_c %>%
  group_by(treatment, sampling) %>%
  summarize(mean_richness = mean(logRR_richness),
            sd_richness = sd(logRR_richness))


RR_prokariota_ref_w <- RR_prokariota %>%
  filter(!treatment %in% c( "w", "c", "p"))
RR_prokariota_ref_w$logRR_richness <- round(log(RR_prokariota_ref_w$richness / RR_prokariota_ref_w$ref_w_mean_richness), 2)
mean_sd_prokariota_ref_w <- RR_prokariota_ref_w %>%
  group_by(treatment, sampling) %>%
  summarize(mean_richness = mean(logRR_richness),
            sd_richness = sd(logRR_richness))

RR_prokariota_ref_p <- RR_prokariota %>%
  filter(!treatment %in% c( "w", "c", "p"))
RR_prokariota_ref_p$logRR_richness <- round(log(RR_prokariota_ref_p$richness / RR_prokariota_ref_p$ref_p_mean_richness), 2)
mean_sd_prokariota_ref_p <- RR_prokariota_ref_p %>%
  group_by(treatment, sampling) %>%
  summarize(mean_richness = mean(logRR_richness),
            sd_richness = sd(logRR_richness))

RR.treatment.labs <- c("Warming", "Perturbation", "Warming and perturbation")
names(RR.treatment.labs) <- c("w", "p", "wp")


gglogRR_prokariota <- ggplot(mean_sd_prokariota_ref_c, aes(x = as.factor(sampling), y = mean_richness, group = sampling)) +
  facet_grid(~ treatment, labeller = labeller(treatment = RR.treatment.labs)) +
  geom_errorbar(aes(ymin = mean_richness - sd_richness, ymax = mean_richness + sd_richness,
                    color = treatment, alpha = 0.5),
                position = position_dodge(width = 0.5), width = 0.25, size = 0.75) +
  geom_point(data = RR_prokariota_ref_c, aes(x = as.factor(sampling), y = logRR_richness, color = treatment),
             position = position_dodge(width = 0.5), size = 2, alpha = 0.4) +
  geom_path(group = 1, aes(color = treatment), linewidth = 0.8)+
  geom_point(aes(color = treatment), fill = "white", position = position_dodge(width = 0.5), size = 2.25, shape = 21) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.7) +
  #scale_colour_manual(values = met.brewer("VanGogh2", 3)) +
  scale_colour_manual(values = c("p" = "blue4", "w" = "red3", "wp" = "purple2")) +
  #scale_x_discrete(breaks = levels(as.factor(mean_sd_data_ref_c$sampling))[seq(1, length(levels(as.factor(mean_sd_data_ref_c$sampling))), by = 2)]) +
  labs(x = "Sampling time", y = "Prokariota richness (LogRR of ASV number)") +
  theme_bw() + theme(legend.position = "none",
                     panel.grid = element_blank(),
                     strip.background = element_blank(),
                     strip.text = element_text(face = "bold"),
                     text = element_text(size = 12))


#Removing all elements from the environment but the ggplots

rm(list = setdiff(ls(), grep("gg", ls(), value = TRUE)))

