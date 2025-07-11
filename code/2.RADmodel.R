



rm(list = ls(all.names = TRUE))

flora_abrich <- read.csv("data/flora_abrich.csv")

source("code/palettes_labels.R")

pacman::p_load(tidyverse, vegan, ggpubr)


flora_rad <- flora_abrich %>% 
  select(plot, sampling, treatment, code, abundance_s) %>%
  mutate(abundance_s = ifelse(abundance_s < 1, 1, abundance_s)) %>%   # Radfit function does not work with 0.x values.  
  filter(!(sampling == 1 & treatment %in% c("p", "wp")))  # Remove rows where treatment is p or wp only for sampling 1.


##| We are going yo use the function radfit of package vegan. This function iterates over a matrix or vector of abundances
##| and it is able to fit the RADs to 5 models: Brokenstick, Preemption, Lognormal, Zipf and Manddelbrot(Improvement of Zipf).
##|  Which model to use can be decided based on several aspects: 
##|  
##|  
##|  
##| 1) AIC coefficient
##|   the lowest it is, the best it fits. 
##|  
##| 2) The purpose of each model
##|  
##|   Preemption: assumes that species compete for limited resources, with their abundance shaped by how much
##|        of the resource is preempted by other species. It is ideal for competitive environments where the available resources restrict
##|         the number of individuals that can coexist, leading to dominance by a few species.
##|         
##|   Log-Normal: assumes species abundances are influenced by numerous small, independent factors,
##|       leading to a normal distribution when data is log-transformed. It works well in communities with a broad range of species 
##|         abundance, where most species have intermediate abundance levels, with a few being very common or very rare.
##|         
##|   Zipf model: follows a power law distribution, where a small number of species dominate in abundance, and most others are rare.
##|        It is suitable for communities where there is a clear rank-abundance pattern, with a few dominant species and many species occurring infrequently.
##|        
##|   Mandelbrot model:  is a flexible power law-based distribution that fits more complex community structures, where species abundance
##|        follow a self-similar or fractal-like pattern. It is useful in heterogeneous environments with intricate species interactions,
##|          where abundance distributions are not easily captured by simpler models like Zipf or Lognormal.
##|          
##|          
##| 3) How disperse is the explanation of the model, aka how many coefficients each model has to explain the fitness of the model: 
##| 
##|     Preemtion: one coefficient (alpha)
##|     Log-normal: 2 coefficients (mu and sigma)
##|     Zipf: 1 coefficient (gamma)
##|     Mandlebrot: 2 coefficients (gamma and beta)
##|     
##|     For us is better with less coeffcients
##|     
##|     


# Taking a general look to the data. Lets see the general RAD for the experiment

rad_all <- flora_rad %>% 
  group_by(code) %>% 
  summarise(abundance_s = round(mean(abundance_s), 0)) %>% 
  as.data.frame() 

ggplot(rad_all, aes(x = reorder(code, -abundance_s), y = abundance_s)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

radfit_all <- rad_all %>% 
  pivot_wider(
    names_from = code,
    values_from = abundance_s,
    values_fill = 0) %>% 
  as.data.frame() %>% 
  radfit() %>% 
  print()


radfit_all$models$Preemption
radfit_all$models$Lognormal
radfit_all$models$Zipf
radfit_all$models$Mandelbrot

rad_all <- rad_all %>% 
  mutate(rank = rank(-abundance_s, ties.method = "first")) %>% 
  mutate(
    pre_alpha = as.numeric(radfit_all$models$Preemption$coefficients[1]),
    pre_AIC = as.numeric(round(radfit_all$models$Preemption$aic, 2)),
    log_mu = as.numeric(radfit_all$models$Lognormal$coefficients[1]),
    log_sigma = as.numeric(radfit_all$models$Lognormal$coefficients[2]),
    log_AIC = as.numeric(round(radfit_all$models$Lognormal$aic, 2)),
    zipf_p1 = as.numeric(radfit_all$models$Zipf$coefficients[1]),
    zipf_gamma = as.numeric(radfit_all$models$Zipf$coefficients[2]),
    zipf_AIC = as.numeric(round(radfit_all$models$Zipf$aic, 2)),
    mand_c = as.numeric(radfit_all$models$Mandelbrot$coefficients[1]), 
    mand_gamma = as.numeric(radfit_all$models$Mandelbrot$coefficients[2]), 
    mand_beta = as.numeric(radfit_all$models$Mandelbrot$coefficients[3]),
    mand_AIC = as.numeric(round(radfit_all$models$Mandelbrot$aic, 2)),
    total_abundance = sum(abundance_s)
  ) %>% 
  mutate(
    preemption_fit = total_abundance * pre_alpha*(1-pre_alpha)^(rank-1),
    log_fit = exp(log_mu + log_sigma * qnorm(abundance_s/(max(abundance_s) + 1))),
    #log_fit = exp(log_mu + log_sigma * qnorm(rank/(max(rank) + 1))),
    #log_fit = exp(log_mu + log_sigma * log(abundance_s)),
    zipf_fit = total_abundance*zipf_p1 * (rank^zipf_gamma),
    mandelbrot_fit = total_abundance * mand_c * (rank + mand_beta)^mand_gamma
  )

ggplot(rad_all, aes(x = reorder(code, -abundance_s), y = abundance_s)) +
  geom_point() +
  geom_line(aes(x = rank, y = preemption_fit), color = "blue") +
  geom_line(aes(x = rank, y = log_fit), color = "red3") +
  geom_line(aes(x = rank, y = zipf_fit), color = "green3") +
  geom_line(aes(x = rank, y = mandelbrot_fit), color = "pink4") +
  labs(x = "Species rank" , y = "Mean abundance",
       title = "RAD whole experiment",
       subtitle = paste0("AIC values: \n",
                         "- Zipf: ", rad_all$zipf_AIC,
                         "\n", "- Mandelbrot: ", rad_all$mand_AIC, "\n",
                         "- Lognormal: ", rad_all$log_AIC, "\n",
                         "- Preemption: ", rad_all$pre_AIC)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Log normal no se ajusta bien del todo porque abundance_s no es estrictamente una distribución log-normal: 
hist(rad_all$abundance_s, breaks = 12)
hist(log(rad_all$abundance_s), breaks = 12)  






# And aslo, per treatment. 


rad_treat_list <- list()
treats <- unique(flora_rad$treatment)

for(i in 1:length(treats)) {
  
  rad_treat <- flora_rad %>% 
    filter(treatment == treats[i]) %>% 
    group_by(code) %>% 
    summarise(abundance_s = round(mean(abundance_s), 0)) %>% 
    as.data.frame() 
  
  radfit_treat <- rad_treat %>% 
    pivot_wider(
      names_from = code,
      values_from = abundance_s,
      values_fill = 0) %>% 
    as.data.frame() %>% 
    radfit() 
  
  rad_treat_list[[i]] <- rad_treat %>% 
    mutate(rank = rank(-abundance_s, ties.method = "first")) %>% 
    mutate(
      pre_alpha = as.numeric(radfit_treat$models$Preemption$coefficients[1]),
      pre_AIC = as.numeric(round(radfit_treat$models$Preemption$aic, 2)),
      log_mu = as.numeric(radfit_treat$models$Lognormal$coefficients[1]),
      log_sigma = as.numeric(radfit_treat$models$Lognormal$coefficients[2]),
      log_AIC = as.numeric(round(radfit_treat$models$Lognormal$aic, 2)),
      zipf_p1 = as.numeric(radfit_treat$models$Zipf$coefficients[1]),
      zipf_gamma = as.numeric(radfit_treat$models$Zipf$coefficients[2]),
      zipf_AIC = as.numeric(round(radfit_treat$models$Zipf$aic, 2)),
      mand_c = as.numeric(radfit_treat$models$Mandelbrot$coefficients[1]), 
      mand_gamma = as.numeric(radfit_treat$models$Mandelbrot$coefficients[2]), 
      mand_beta = as.numeric(radfit_treat$models$Mandelbrot$coefficients[3]),
      mand_AIC = as.numeric(round(radfit_treat$models$Mandelbrot$aic, 2)),
      total_abundance = sum(abundance_s)
    ) %>% 
    mutate(
      preemption_fit = total_abundance * pre_alpha*(1-pre_alpha)^(rank-1),
      log_fit = exp(log_mu + log_sigma * qnorm(abundance_s/(max(abundance_s) + 1))),
      #log_fit = exp(log_mu + log_sigma * qnorm(rank/(max(rank) + 1))),
      #log_fit = exp(log_mu + log_sigma * log(abundance_s)),
      zipf_fit = total_abundance*zipf_p1 * (rank^zipf_gamma),
      mandelbrot_fit = total_abundance * mand_c * (rank + mand_beta)^mand_gamma
    ) %>% 
    mutate(
      treatment = treats[i]
    )
}




rad_treat_db <- do.call(rbind, rad_treat_list)


ggplot(rad_treat_db, aes(x = rank, y = abundance_s)) +
  facet_wrap(~treatment, labeller = labeller(treatment = labels3)) + 
  geom_point() +
  geom_line(aes(x = rank, y = preemption_fit), color = "blue3") +
  geom_line(aes(x = rank, y = log_fit), color = "red3") +
  geom_line(aes(x = rank, y = zipf_fit), color = "green3", size = 1, linetype = "dashed") +
  geom_line(aes(x = rank, y = mandelbrot_fit), color = "pink3") +
  labs(x = "Rank", y = "Mean abundance", title = "RAD per treatment") +
  geom_text(aes(x = max(rank) * 0.5, y = max(abundance_s) * 0.9, 
                label = paste("Preemption AIC:", round(pre_AIC, 2))),
            color = "blue3", size = 3.5, hjust = 0) +
  
  geom_text(aes(x = max(rank) * 0.5, y = max(abundance_s) * 0.85, 
                label = paste("Lognormal AIC:", round(log_AIC, 2))),
            color = "red3", size = 3.5, hjust = 0) +
  
  geom_text(aes(x = max(rank) * 0.5, y = max(abundance_s) * 0.8, 
                label = paste("Zipf AIC:", round(zipf_AIC, 2))),
            color = "green4", size = 3.5, hjust = 0) +
  
  geom_text(aes(x = max(rank) * 0.5, y = max(abundance_s) * 0.75, 
                label = paste("Mandelbrot AIC:", round(mand_AIC, 2))),
            color = "pink3", size = 3.5, hjust = 0) +
  
  theme(strip.text = element_text(size = 10))




# We choose ZIPF

{gg_rads_treatment <- 
ggplot(rad_treat_db, aes(x = rank, y = abundance_s)) +
    
  facet_wrap(~treatment, labeller = labeller(treatment = labels1), ncol = 4, nrow = 1) + 
    
  geom_point(aes(color = treatment)) +
    
  geom_line(aes(x = rank, y = zipf_fit, color = treatment), size = 1) +
    
  labs(x = "Rank", y = "Mean abundance") +
    
  scale_color_manual(values = palette_CB) + 
    
  geom_text(aes(x = max(rank) * 0.7, y = max(abundance_s) * 0.9, 
                label = paste("Gamma:", round(zipf_gamma, 4))), size = 3.2) +
    
    theme(
      panel.grid = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      text = element_text(size = 15),
      legend.position = "none"
    )
  
print(gg_rads_treatment)
ggsave("results/Plots/protofinal/RADs_treatment.png", plot = gg_rads_treatment, dpi = 300)
}



ggplot(rad_treat_db, aes(x = rank, y = abundance_s, color = treatment)) +
  geom_point(size = 1.5) +
  geom_line(aes(x = rank, y = zipf_fit, color = treatment), size = 1) +
  scale_color_manual(values = palette5, name = "Treatment", labels = labels3) + 
  labs(x = "Rank", y = "Mean abundance", title = "RAD per treatment") +
  geom_text(aes(x = max(rank) * 0.7, 
                y = max(abundance_s) * 0.99 + as.numeric(factor(treatment)) * 0.05 * max(abundance_s), 
                label = paste("Zipf gamma:", round(zipf_gamma, 4))), 
            size = 4, hjust = 0, show.legend = FALSE) +
  theme(legend.position = "bottom")+ 
  guides(color = guide_legend(override.aes = list(linetype = "solid", shape = 16)))  # Solo muestra punto y línea




# PLOT LEVEL

# Choosing model


#Sampling 1 gives problems since there are no species for p and wp

### AIC comparison 

samps <- sort(unique(flora_rad$sampling))
plots <- sort(unique(flora_rad$plot))

rad_plot_AIC <- matrix(nrow = (length(samps)*length(plots)), ncol = 6)
colnames(rad_plot_AIC) <- c("sampling", "plot", "AIC_pree", "AIC_log", "AIC_zipf", "AIC_man")
rad_plot_AIC <-  as.data.frame(rad_plot_AIC)

count <- 0
for (i in 1:length(samps)){
  for (j in 1:length(plots)){
    
    if (samps[i] == 1 && plots[j] %in% c(3, 6, 10, 15, 4, 5, 12, 13)) {
      next  # Avoiding plots from treatment p and wp in sampling 1 because there are no data
    }
  
    
    count <- count + 1
  
    rad <- flora_rad %>% 
      filter(sampling == samps[i],
             plot == plots[j]) %>% 
      group_by(code) %>% 
      summarise(abundance_s = round(mean(abundance_s), 0)) %>% 
      pivot_wider(
        names_from = code,
        values_from = abundance_s,
        values_fill = 0) %>% 
      as.data.frame()
    
    rad_fit <- radfit(rad)
      
    
    rad_plot_AIC$sampling[count] <- samps[i]
    rad_plot_AIC$plot[count] <- plots[j]
    rad_plot_AIC$AIC_pree[count] <- rad_fit$models$Preemption$aic
    rad_plot_AIC$AIC_log[count] <- rad_fit$models$Lognormal$aic
    rad_plot_AIC$AIC_zipf[count] <- rad_fit$models$Zipf$aic
    rad_plot_AIC$AIC_man[count] <- rad_fit$models$Mandelbrot$aic
    
  }
}

rad_plot_AIC <- pivot_longer(rad_plot_AIC, cols = c("AIC_pree", "AIC_log","AIC_zipf","AIC_man"), 
                           names_to = "model", values_to = "AIC")
ggplot(rad_plot_AIC, aes(x = model, y = AIC))+
  geom_boxplot()

# Again, we decide to use zipf because it only has one explanatory coefficient of the curve (gamma)


rad_plot_list <- list()
count <- 0

for(i in 1:length(samps)) {
  for(j in 1:length(plots)){
    
    if (samps[i] == 1 && plots[j] %in% c(3, 6, 10, 15, 4, 5, 12, 13)) {
      next  # Avoiding plots from treatment p and wp in sampling 1 because there are no data
    }
    
    count = count + 1
    
    rad_plot <- flora_rad %>% 
      filter(sampling == samps[i]) %>% 
      filter(plot == plots[j])
    
    treatment <- droplevels(unique(rad_plot$treatment))
    
      rad_plot <- rad_plot %>% 
        group_by(code) %>% 
        summarise(abundance_s = round(mean(abundance_s), 0)) %>% 
        as.data.frame() 
    
    radfit_plot <- rad_plot %>% 
      pivot_wider(
        names_from = code,
        values_from = abundance_s,
        values_fill = 0) %>% 
      as.data.frame() %>% 
      radfit() 
    
    rad_plot_list[[count]] <- rad_plot %>% 
      mutate(rank = rank(-abundance_s, ties.method = "first")) %>% 
      mutate(
        zipf_p1 = as.numeric(radfit_plot$models$Zipf$coefficients[1]),
        zipf_gamma = as.numeric(radfit_plot$models$Zipf$coefficients[2]),
        total_abundance = sum(abundance_s)
      ) %>% 
      mutate(
        zipf_fit = total_abundance*zipf_p1 * (rank^zipf_gamma),
      ) %>% 
      mutate(
        treatment = treatment, 
        sampling = samps[i],
        plot = plots[j]
      )
  }
}

rad_plot <- do.call(rbind, rad_plot_list) %>% 
  mutate(plot_treat = paste0(treatment, "-", plot))  
  #mutate(zipf_gamma = ifelse(0, is.na, zipf_gamma))

{i = 10
gg_rad_eg <- 
rad_plot %>% 
  filter(sampling == samps[i]) %>% 
  ggplot(aes(x = rank, y = abundance_s, color = treatment)) +
  facet_wrap(~ plot_treat) + 
  geom_point() +
  geom_line(aes(x = rank, y = zipf_fit), color = "black") +
  scale_color_manual(values = palette5)+
  labs(x = "Rank", y = "Mean abundance", title = paste0("RAD per plot at sampling ", samps[i])) +
  geom_text(aes(x = max(rank) * 0.7, y = max(abundance_s) * 0.9, 
                label = paste("Zipf gamma:", round(zipf_gamma, 4))), size = 3.2)
print(gg_rad_eg)
ggsave("results/Plots/protofinal/RAD_sampling_eg.png", plot = gg_rad_eg, dpi = 300)}




# Means

radcoeff_db_plot <- rad_plot %>% 
  distinct(plot, sampling, treatment, zipf_gamma) %>% 
  select(plot, sampling, treatment, zipf_gamma) %>% 
  rename(Y_zipf = zipf_gamma)



{dummy_rows_p <- matrix(nrow = 4, ncol = 4)
  colnames(dummy_rows_p) <- c("sampling", "plot", "treatment", "Y_zipf")
  dummy_rows_p <- as.data.frame(dummy_rows_p)
  
  dummy_rows_p[] <- NA
  dummy_rows_p[, 1] <- 1
  dummy_rows_p[, 2] <- c(3, 6, 10, 15)
  dummy_rows_p[, 3] <- "p"
  
  
  dummy_rows_wp <- matrix(nrow = 4, ncol = 4)
  colnames(dummy_rows_wp) <- c("sampling", "plot", "treatment", "Y_zipf")
  dummy_rows_wp <- as.data.frame(dummy_rows_wp)
  
  dummy_rows_wp[] <- NA
  dummy_rows_wp[, 1] <- 1
  dummy_rows_wp[, 2] <- c(4, 5, 12, 13)
  dummy_rows_wp[, 3] <- "wp"
  
  
  dummy_rows <- bind_rows(dummy_rows_p, dummy_rows_wp)%>% 
    mutate(treatment = as.factor(treatment), 
           sampling = as.factor(sampling), 
           plot = as.factor(plot))
 


radcoeff_db_plot <- bind_rows(radcoeff_db_plot, dummy_rows) }


sampling_dates <- read.csv("data/sampling_dates.csv") %>% 
  mutate(sampling = as.factor(sampling),
         date = ymd(date), 
         year = year(date)) %>% 
  select(sampling, date, year, one_month_window, omw_date) %>% 
  mutate(across(where(is.character), as.factor))

radcoeff_db_plot <- radcoeff_db_plot %>% 
  left_join(sampling_dates)

radcoeff_db_plot %>% write.csv("data/radcoeff_db_plot.csv", row.names = FALSE)

rm(flora_rad)
rm(flora_rad)
rm(flora_s1cw)
rm(plots)
rm(radcoeff_s1cw)



