


source("code/1.first_script.R")

radcoeff_df <- read.csv("data/radcoeff_df.csv") %>% 
  mutate(treatment = as.factor(treatment), 
         sampling = as.factor(sampling), 
         plot = as.factor(plot))

radcoeff_df<- radcoeff_df %>% 
  mutate(Y_zipf = Y_zipf + abs(min(radcoeff_df$Y_zipf, na.rm = T)) + 1,
         mu_log = mu_log + abs(min(radcoeff_df$mu_log, na.rm = T)) + 1, 
         sigma_log = sigma_log - min(radcoeff_df$sigma_log, na.rm = T) + 1)



join <- full_join(flora_abrich, radcoeff_df)   


ab_rich_treatmeans <- join %>% 
  distinct(treatment, plot, sampling, date, .keep_all = TRUE) %>% 
  group_by(treatment) %>% 
  mutate(
    mean_richness = mean(richness, na.rm = T),
    sd_richness = sd(richness, na.rm = T),
    mean_abundance = mean(abundance, na.rm = T),
    sd_abundance = sd(abundance, na.rm = T),
    mean_Y_zipf = mean(Y_zipf, na.rm = T),
    sd_Y_zipf = sd(Y_zipf, na.rm = T),
    mean_mu_log = mean(mu_log, na.rm = T),
    sd_mu_log = sd(mu_log, na.rm = T),
    mean_sigma_log = mean(sigma_log, na.rm = T),
    sd_sigma_log = sd(sigma_log, na.rm = T), 
    n = n()
    )%>% 
  mutate(
    cv_richness = sd_richness/mean_richness,
    cv_abundance = sd_abundance/mean_abundance,
    cv_Y_zipf = sd_Y_zipf/mean_Y_zipf,
    cv_mu_log = sd_mu_log/mean_mu_log,
    cv_sigma_log = sd_sigma_log/mean_sigma_log
  ) %>% 
  select(treatment, n, mean_richness, sd_richness, mean_abundance, sd_abundance, mean_Y_zipf, 
         sd_Y_zipf, mean_mu_log, sd_mu_log, mean_sigma_log, sd_sigma_log, cv_richness,
         cv_abundance, cv_Y_zipf, cv_mu_log, cv_sigma_log) %>% 
  distinct(treatment, n, mean_richness, sd_richness, mean_abundance, sd_abundance, mean_Y_zipf, 
         sd_Y_zipf, mean_mu_log, sd_mu_log, mean_sigma_log, sd_sigma_log, cv_richness,
         cv_abundance, cv_Y_zipf, cv_mu_log, cv_sigma_log)
 
  


source("code/meta_function/effect_treatment.R")

effect_size_treatment(ab_rich_treatmeans, "richness")
gg_delta_RR

