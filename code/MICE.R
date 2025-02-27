

library(mice)

biomass_mice <- biomass_nolm %>% 
  select(year, sampling, plot, treatment, code, family, richness, abundance, abundance_community,
         height, Ah, Ab, biomass_i, nind_m2) %>% 
  mutate(across(where(is.character), as.factor))

# We use mice package
##| method = "rf". Random forest models allow us to use both numerical and factorial variables in the imputation. Therefore, code, sampling and plot are used
##| m = 10 is the number of datasets that mice creates. This is, 10 values of nind_m2
##| maxit is the number of times that mice iterates through each dataset. The higher, the more stable (in complex database)


#biomass_mice_imputed <- mice(biomass_mice, method = "rf", m = 10, maxit = 300)

saveRDS(biomass_mice_imputed, "data/biomass_mice_imputed.rds")
biomass_mice_imputed <- readRDS("data/biomass_mice_imputed.rds")

plot(biomass_mice_imputed) # Check if lines stabilize
stripplot(biomass_mice_imputed, pch = 20, cex = 1.2) #If imputed values (blue dots) align well with observed values, MICE is working well.
densityplot(biomass_mice_imputed)


imputed_db1 <- complete(biomass_mice_imputed, action = 1)
ggplot() +
  geom_density(aes(x = biomass_nolm$nind_m2), color = "blue3") +
  geom_density(aes(x = imputed_db1$nind_m2), color = "red3") +
  labs(title = "Density Plot of Original (Red) vs Imputed (Blue) nind_m2")


imputed_db <- complete(biomass_mice_imputed, action = "long")

# hay que intentar entender bien este gráfico
ggplot() +
  geom_density(data = imputed_db, aes(x = nind_m2, color = as.factor(.imp))) +
  geom_density(data = biomass_nolm, aes(x = nind_m2), color = "black") +
  labs(title = "Density Plot of Original (Blue) vs Imputed (Colored by .imp)",
       x = "nind_m2",
       y = "Density",
       color = "Imputation Number") +
  theme_minimal()

imputed_db <- imputed_db %>% 
  group_by(plot, treatment, sampling, code) %>% 
  mutate(nind_m2_imputed = round(mean(nind_m2, 0)),
         sd_imputation = sd(nind_m2)) %>% 
  select(plot, treatment, sampling, code, nind_m2_imputed, sd_imputation) %>% 
  distinct()


imputed_db$label_imputation <- ifelse(is.na(biomass_nolm$nind_m2), 1, 0)

imputed_db %>% 
  filter(label_imputation == 1) %>% 
  mutate(CV = sd_imputation / nind_m2_imputed) %>% 
  ggplot(aes(x = CV)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = 1), color = "black", linetype = "dashed", size = 1) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  labs(title = "Distribution of Coefficient of Variation for Imputed Values",
       x = "SD / mean Ratio",
       y = "Count") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))



# Tries
try1 <- mice(biomass_mice, method = "rf", m = 5, maxit = 5)

plot(try1) # Check if lines stabilize
stripplot(try1, pch = 20, cex = 1.2) #If imputed values (blue dots) align well with observed values, MICE is working well.
densityplot(try1)

try1_db <- complete(try1, action = "long")

try1_db <- try1_db %>% 
  group_by(plot, treatment, sampling, code) %>% 
  mutate(nind_m2_imputed = mean(nind_m2),
         sd_imputation = sd(nind_m2)) %>% 
  select(plot, treatment, sampling, code, nind_m2_imputed, sd_imputation) %>% 
  distinct()


try1_db$zeroriginal_oneimputed <- ifelse(is.na(biomass_nolm$nind_m2), 1, 0)



# Reliability

# Checking reliability of mice imputation 
# Let's recreate
perc_NA <- ((biomass_nolm %>% 
               filter(is.na(nind_m2)) %>% 
               nrow())) / nrow(biomass_nolm)

nind_nona <- biomass_nolm %>% 
  filter(!is.na(nind_m2))

#Artificially creating the same percentage of NA as
# there are in my original database and keeping the same variables as in the original imputation
mice_check <- nind_nona %>%
  mutate(nind_m2 = ifelse(runif(n()) < perc_NA, NA, nind_m2)) %>% 
  select(year, sampling, plot, treatment, code, family, richness, abundance, abundance_community,
         height, Ah, Ab, biomass_i, nind_m2) %>% 
  mutate(across(where(is.character), as.factor))


mice_check %>% 
  filter(is.na(nind_m2)) %>% 
  nrow()

# Dejo creada la base de datos de los NA creados artificialmente para nind_m2
check_subset <- mice_check %>% 
  filter(is.na(nind_m2)) %>% 
  select(sampling, plot, treatment, code, abundance, abundance_community) %>%  # Keeping only necessary identifiers
  left_join(nind_nona %>% select(sampling, plot, treatment, code, abundance, abundance_community, nind_m2), 
            by = c("sampling", "plot", "treatment", "code", "abundance", "abundance_community")) %>% 
  rename(nind_m2_original = nind_m2)

common <- check_subset %>% 
  select(code, sampling, plot, treatment)

# Same imputation as original 
mice_check_imputed <- mice(mice_check,method = "rf", m = 10, maxit = 300) 
saveRDS(tmice_check_imputed, "data/tmice_check_imputed.rds")
tmice_check_imputed <- readRDS("data/tmice_check_imputed.rds")

check_imputed_db <- complete(mice_check_imputed, action = "long")

check_imputed_db_wide <- check_imputed_db %>%
  mutate(.imp = as.factor(.imp)) %>%  # Convert to factor
  pivot_wider(
    names_from = .imp, 
    values_from = nind_m2,
    names_prefix = "nind_m2_"
  ) %>% 
  left_join(check_subset) %>% 
  filter(!is.na(nind_m2_original)) %>% 
  ggplot(aes(x = nind_m2_original, y))


check_imputed_db<- check_imputed_db %>%
  mutate(.imp = as.factor(.imp)) %>%  # Convert to factor
  left_join(check_subset) %>% 
  filter(!is.na(nind_m2_original))

  ggplot(check_imputed_db, aes(x = nind_m2_original, y = nind_m2, color = .imp)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F) +
  theme_minimal()
  
  ggplot(check_imputed_db, aes(x = nind_m2_original, y = nind_m2, color = as.factor(.imp))) + 
    geom_point(alpha = 0.6) + 
    geom_smooth(method = "lm", se = FALSE) +
    stat_cor(aes(label = paste(after_stat(r.label), after_stat(p.label), sep = "~`,`~")), 
             method = "pearson", 
             label.x.npc = "left", 
             label.y.npc = "top") + # Adds R² and p-value
    theme_minimal() +
    labs(color = "Imputation Number") # Improve legend label




