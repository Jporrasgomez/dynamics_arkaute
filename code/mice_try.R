








biomass_mice <- biomass %>% 
  select(code, family, richness, abundance, abundance_community, biomass_i, nind_m2) %>% 
  mutate(across(where(is.character), as.factor))

# We use mice package
##| method = "rf". Random forest models allow us to use both numerical and factorial variables in the imputation. Therefore, code, sampling and plot are used
##| m = 10 is the number of datasets that mice creates. This is, 10 values of nind_m2
##| maxit is the number of times that mice iterates through each dataset. The higher, the more stable (in complex database)


#biomass_mice_imputed <- mice(biomass_mice, method = "rf", m = 10, maxit = 300)
#saveRDS(biomass_mice_imputed, "data/biomass_mice_imputed.rds")
biomass_mice_imputed <- readRDS("data/biomass_mice_imputed.rds")

#summary(biomass_mice_imputed)
#plot(biomass_mice_imputed) # Check if lines stabilize
##stripplot(biomass_mice_imputed, pch = 20, cex = 1.2) #If imputed values (blue dots) align well with observed values, MICE is working well.
#densityplot(biomass_mice_imputed)


#i = 9
#imputed_dbi <- complete(biomass_mice_imputed, action = i)
#ggplot() +
#  geom_density(aes(x = biomass$nind_m2), color = "blue3") +
#  geom_density(aes(x = imputed_dbi$nind_m2), color = "red3") +
#  labs(title = "Density Plot of Original (Red) vs Imputed (Blue) nind_m2")


imputed_db <- complete(biomass_mice_imputed, action = "long")

# hay que intentar entender bien este gráfico
ggdensity <- ggplot() +
  geom_density(data = imputed_db, aes(x = nind_m2, color = as.factor(.imp))) +
  geom_density(data = biomass, aes(x = nind_m2), color = "black",
               size = 0.8, linetype = "dashed") +
  labs(title = "Density Plot of Original (Blue) vs Imputed (Colored by .imp)",
       x = "nind_m2",
       y = "Density",
       color = "Imputation Number") +
  theme_minimal()

# Calculation of average value for 10 imputations of nind_m2 (m = 10) and its sd. 

imputed_db <- imputed_db %>% 
  group_by(plot, treatment, sampling, code) %>% 
  mutate(nind_m2_imputed = round(mean(nind_m2, 0)),
         sd_imputation = sd(nind_m2)) %>% 
  select(plot, treatment, sampling, code, nind_m2_imputed, sd_imputation) %>% 
  distinct()

imputed_db$label_imputation <- ifelse(is.na(biomass$nind_m2), 1, 0)

# Here I plot the CV (Coefficient of variation) of the imputed values (n = 10) So I can check the stability of the imputed
# data. The more CV < 1, the more stable it is


imput_stability_db <- imputed_db %>% 
  filter(label_imputation == 1) %>% 
  mutate(CV = sd_imputation / nind_m2_imputed) %>% 
  as.data.frame()

imput_stability <- ggplot(imput_stability_db, aes(x = CV)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = 1), color = "black", linetype = "dashed", size = 1) +
  scale_x_continuous(
    expand = expansion(mult = c(0.05, 0.1)),
    breaks = seq(0, max(imputed_db$sd_imputation / imputed_db$nind_m2_imputed, na.rm = TRUE), by = 0.2) # Adjust "by" as needed
  ) +
  labs(title = "Distribution of Coefficient of Variation for Imputed Values",
       x = "(SD/mean) Ratio",
       y = "Count") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))


imput_stability_boxplot <- 
  ggplot(imput_stability_db, aes(y = CV)) +
  geom_boxplot(fill = "steelblue") +
  geom_hline(aes(yintercept = 1), color = "red3", linetype = "dashed", size = 1) +
  labs(title = "Distribution of Coefficient of Variation for Imputed Values",
       x = "(SD/mean) Ratio",
       y = "Count") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))


mice_results <-
  ggplot(imputed_db, aes( x = sampling, y = nind_m2_imputed, color = as.factor(label_imputation))) +
  facet_wrap(~code, ncol = 7, nrow = 6)+
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("0" = "cyan3", "1" = "red3")) +
  scale_x_discrete(breaks = levels(imputed_db$sampling)[seq(1, length(levels(imputed_db$sampling)), by = 5)]) +  # Show every 2nd label
  labs(color = "Imputation")