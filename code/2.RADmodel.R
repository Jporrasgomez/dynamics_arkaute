



source("code/1.first_script.R")

rm(list = setdiff(ls(), c("flora_abrich")))


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


rad_treat_list[[2]] %>% View()
rad_treat_db <- do.call(rbind, rad_treat_list)


ggplot(rad_treat_db, aes(x = rank, y = abundance_s)) +
  facet_wrap(~treatment) + 
  geom_point() +
  geom_line(aes(x = rank, y = preemption_fit), color = "blue") +
  geom_line(aes(x = rank, y = log_fit), color = "red3") +
  geom_line(aes(x = rank, y = zipf_fit), color = "green3", size = 1, linetype = "dashed") +
  geom_line(aes(x = rank, y = mandelbrot_fit), color = "pink4") +
  labs(x = "Rank" , y = "Mean abundance",
       title = "RAD per treatment") +
  # Agregar texto con los valores AIC para cada modelo
  annotate("text", x = max(rad_treat_db$rank) * 0.7, y = max(rad_treat_db$abundance_s) * 0.9, 
           label = paste("Preemption AIC:", round(rad_treat_db$pre_AIC[1], 2)), color = "blue", size = 4) +
  annotate("text", x = max(rad_treat_db$rank) * 0.7, y = max(rad_treat_db$abundance_s) * 0.85, 
           label = paste("Lognormal AIC:", round(rad_treat_db$log_AIC[1], 2)), color = "red3", size = 4) +
  annotate("text", x = max(rad_treat_db$rank) * 0.7, y = max(rad_treat_db$abundance_s) * 0.8, 
           label = paste("Zipf AIC:", round(rad_treat_db$zipf_AIC[1], 2)), color = "green4", size = 4) +
  annotate("text", x = max(rad_treat_db$rank) * 0.7, y = max(rad_treat_db$abundance_s) * 0.75, 
           label = paste("Mandelbrot AIC:", round(rad_treat_db$mand_AIC[1], 2)), color = "pink4", size = 4)






# Now create a ggplot with observed abundance and fitted curves for each model
ggplot(rad_c, aes(x = reorder(code, -abundance_s), y = abundance_s)) +
  geom_col() +  # Observed data
  geom_line(data = fit_values, aes(x = x, y = preemption), color = "blue", size = 1, linetype = "dashed") +
  geom_line(data = fit_values, aes(x = x, y = lognormal), color = "green", size = 1, linetype = "dashed") +
  geom_line(data = fit_values, aes(x = x, y = zipf), color = "red", size = 1, linetype = "dashed") +
  geom_line(data = fit_values, aes(x = x, y = mandelbrot), color = "purple", size = 1, linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Model Fits (Preemption, Lognormal, Zipf, Mandelbrot)",
    x = "Species Code",
    y = "Abundance"
  )



















# CHOOSING MODEL


#Sampling 1 gives problems since there are no species for p and wp

### AIC comparison 

rad_dfplot <- matrix(nrow = (length(samps)*length(plots)), ncol = 6)
colnames(rad_dfplot) <- c("sampling", "plot", "AIC_pree", "AIC_log", "AIC_zipf", "AIC_man")
rad_dfplot <-  as.data.frame(rad_dfplot)

samps <- sort(unique(flora_rad$sampling))
plots <- sort(unique(flora_rad$plot))

count <- 0
for (i in 1:length(samps)){
  for (j in 1:length(plots)){
    
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
      
    
    rad_dfplot$sampling[count] <- samps[i]
    rad_dfplot$plot[count] <- plots[j]
    rad_dfplot$AIC_brok[count] <- rad_fit$models$Null$aic
    rad_dfplot$AIC_pree[count] <- rad_fit$models$Preemption$aic
    rad_dfplot$AIC_log[count] <- rad_fit$models$Lognormal$aic
    rad_dfplot$AIC_zipf[count] <- rad_fit$models$Zipf$aic
    rad_dfplot$AIC_man[count] <- rad_fit$models$Mandelbrot$aic
    
  }
}

rad_dfplot <- pivot_longer(rad_dfplot, cols = c("AIC_pree", "AIC_log","AIC_zipf","AIC_man"), 
                           names_to = "model", values_to = "AIC")
ggplot(rad_dfplot, aes(x = model, y = AIC))+
  geom_boxplot()

# No differences. We decide to use zipf because it only has one explanatory coefficient of the curve (gamma)

# Applying Zipf and lognormal to the dataset: ############

radcoeff_df <- matrix(nrow = (length(unique(flora_rad$sampling))*length(unique(flora_rad$plot))), ncol = 5)
colnames(radcoeff_df) <- c("plot", "sampling", "Y_zipf", "mu_log", "sigma_log")
radcoeff_df <- as.data.frame(radcoeff_df)

count = 0
for(i in 1:length(unique(flora_rad$sampling))){
  for(j in 1:length(unique(flora_rad$plot))){
    
    count <- count + 1
    subset_data <- subset(flora_rad, sampling == unique(flora_rad$sampling)[i] & plot == unique(flora_rad$plot)[j])
    subrad <- summarise(group_by(subset_data, code),
                        abundance = round(mean(abundance), 0)) 
    subrad <- pivot_wider(subrad, names_from = code, values_from = abundance, values_fill = 0)
    subrad <- as.data.frame(subrad)
    rad_sub <- vegan::radfit(subrad)
    
    radcoeff_df$plot[count] <- unique(flora_rad$plot)[j]
    radcoeff_df$sampling[count] <- unique(flora_rad$sampling)[i]
    radcoeff_df$Y_zipf[count] <- rad_sub$models$Zipf$coefficients[2]
    radcoeff_df$mu_log[count] <- rad_sub$models$Lognormal$coefficients[1]
    radcoeff_df$sigma_log[count] <- rad_sub$models$Lognormal$coefficients[2]
    
    rm(subrad)
    rm(subset_data)
    rm(rad_sub)
  }
}

# *** #mean here is "fake". Actually we keep the same number of abundance per species. However,
# we have several replicates of the same species per plot due to the different morphological measuremens
# for biomass. The mean of the same number is the same number. 

#Plot 15 (t = p) del sampling 2 aparece como NA. Hacer individual dar valores de i = 5 y j = 7
# (*la posicion de los niveles no corresponde con el valor del muestreo o plot). Da valores de "inf". 


# Al hacer el loop los samplings van del 1 al 12 en vez del 0 al 11
radcoeff_df$sampling <- factor(as.integer(radcoeff_df$sampling) - 1,
                           levels = 0:max(as.integer(radcoeff_df$sampling)) - 1)
radcoeff_df$plot <- as.factor(radcoeff_df$plot )
radcoeff_df$sampling <- as.factor(radcoeff_df$sampling)

plots <- read.csv("data/plots.csv") %>% 
  mutate(across(where(is.character), as.factor)) %>% 
  mutate(plot = nplot, treatment = treatment_code) %>% 
  select(treatment, plot)


radcoeff_df <- merge(radcoeff_df, plots, by = "plot")

# Adding by hand sampling == 1 and treatments w and c. ######

flora_s1cw <- flora_rad[(flora_rad$sampling == 1 & (flora_rad$treatment == "c" | flora_rad$treatment == "w")), ]

radcoeff_s1cw <- matrix(nrow = (length(unique(flora_s1cw$sampling))*length(unique(flora_s1cw$plot))), ncol = 5)
colnames(radcoeff_s1cw) <- c("plot", "sampling", "Y_zipf", "mu_log", "sigma_log")
radcoeff_s1cw <- as.data.frame(radcoeff_s1cw)

count = 0 
for(i in 1:length(unique(flora_s1cw$plot))){
  count <- count + 1
  subset_data <- subset(flora_s1cw, sampling == "1" & plot == unique(flora_s1cw$plot)[i])
  subrad <- summarise(group_by(subset_data, code),
                      abundance = round(mean(abundance), 0))
  subrad <- pivot_wider(subrad, names_from = code, values_from = abundance, values_fill = 0)
  subrad <- as.data.frame(subrad)
  rad_sub <- radfit(subrad)
  
  radcoeff_s1cw$plot[count] <- unique(flora_s1cw$plot)[i]
  radcoeff_s1cw$sampling[count] <- "1"
  radcoeff_s1cw$Y_zipf[count] <- rad_sub$models$Zipf$coefficients[2]
  radcoeff_s1cw$mu_log[count] <- rad_sub$models$Lognormal$coefficients[1]
  radcoeff_s1cw$sigma_log[count] <- rad_sub$models$Lognormal$coefficients[2]
  
  rm(subrad)
  rm(subset_data)
  rm(rad_sub)
}

#15 warnings due to plot 1. But it works

radcoeff_s1cw$plot <- as.factor(radcoeff_s1cw$plot )
radcoeff_s1cw$sampling <- as.factor(radcoeff_s1cw$sampling)

radcoeff_s1cw <- merge(radcoeff_s1cw, plots, by = "plot")



#Adding muestreo 1 de c y w. 

radcoeff_df <- rbind(radcoeff_df, radcoeff_s1cw)
radcoeff_df$treatment <- as.factor(radcoeff_df$treatment)

radcoeff_df$plot <- factor(radcoeff_df$plot, levels = sort(unique(radcoeff_df$plot)))
radcoeff_df$treatment <- as.factor(radcoeff_df$treatment) 
radcoeff_df$treatment <- factor(radcoeff_df$treatment, levels = c("c", "w", "p", "wp"))
radcoeff_df$sampling <- factor(radcoeff_df$sampling, levels = sort(unique(radcoeff_df$sampling)))  #Sort from lowest to highest


dummy_rows_p <- matrix(nrow = 4, ncol = 6)
  colnames(dummy_rows_p) <- c("sampling", "plot", "treatment", "Y_zipf", "mu_log", "sigma_log")
  dummy_rows_p <- as.data.frame(dummy_rows_p)
  
  dummy_rows_p[] <- NA
  dummy_rows_p[, 1] <- 1
  dummy_rows_p[, 2] <- c(3, 6, 10, 15)
  dummy_rows_p[, 3] <- "p"
  
  
  dummy_rows_wp <- matrix(nrow = 4, ncol = 6)
  colnames(dummy_rows_wp) <- c("sampling", "plot", "treatment", "Y_zipf", "mu_log", "sigma_log")
  dummy_rows_wp <- as.data.frame(dummy_rows_wp)
  
  dummy_rows_wp[] <- NA
  dummy_rows_wp[, 1] <- 1
  dummy_rows_wp[, 2] <- c(4, 5, 12, 13)
  dummy_rows_wp[, 3] <- "wp"
  
  
  dummy_rows <- bind_rows(dummy_rows_p, dummy_rows_wp)%>% 
    mutate(treatment = as.factor(treatment), 
           sampling = as.factor(sampling), 
           plot = as.factor(plot))
 


radcoeff_df <- bind_rows(radcoeff_df, dummy_rows) 


radcoeff_df %>% write.csv("data/radcoeff_df.csv", row.names = FALSE)

rm(flora_rad)
rm(flora_rad)
rm(flora_s1cw)
rm(plots)
rm(radcoeff_s1cw)



