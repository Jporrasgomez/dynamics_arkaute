



# Let's gap filling with MICE package ###########


biomass_nolm_mice <- biomass_nolm %>% 
  select(sampling, plot, treatment, code, richness, abundance, abundance_community, nind_m2)


biomass_nolm_mice <- biomass_nolm_mice %>% mutate(across(where(is.character), as.factor))


library(mice)

# Aplicar imputación con Random Forest

# We use mice package
##| method = "rf". Random forest models allow us to use both numerical and factorial variables in the imputation. Therefore, code, sampling and plot are used
##| m = 5 is the number of datasets that imce creates. This is, 5 values of nind_m2
##| maxit is the number of times that the imputation take place. 

biomass_nolm_mice <- mice(biomass_nolm_mice, method = "rf", m = 10, maxit = 100) # I tried with more iterations (maxit) but there was no difference

saveRDS(biomass_nolm_mice, "data/biomass_nolm_mice.rds")

biomass_nolm_mice <- readRDS("data/biomass_nolm_mice.rds")

plot(biomass_nolm_mice) # Check if lines stabilize
stripplot(biomass_nolm_mice, pch = 20, cex = 1.2) #If imputed values (blue dots) align well with observed values, MICE is working well.
densityplot(biomass_nolm_mice)


# Obtener dataset imputado
biomass_nolm_imputed <- complete(biomass_nolm_mice)
pool <- pool(biomas_nolm_mice)

summary(biomass_nolm_imputed)

ggplot() +
  geom_density(aes(x = biomass_nolm$nind_m2), color = "blue3") +
  geom_density(aes(x = biomass_nolm_imputed$nind_m2), color = "red3") +
  labs(title = "Density Plot of Original (Red) vs Imputed (Blue) nind_m2")


biomass_nolm_imputed$imputed_nind_m2 <- ifelse(is.na(biomass_nolm$nind_m2), 1, 0)
lm_model <- lm(nind_m2 ~ ., data = biomass_nolm_imputed)
summary(lm_model)








# Checking reliability of mice imputation 
# Let's recreate
perc_NA <- ((biomass_nolm %>% 
  filter(is.na(nind_m2)) %>% 
  nrow())) / nrow(biomass_nolm)

nind_nona <- biomass_nolm %>% 
  filter(!is.na(nind_m2))

mice_check <- nind_nona %>%
  mutate(nind_m2 = ifelse(runif(n()) < perc_NA, NA, nind_m2)) %>% 
  select(sampling, plot, treatment, code, richness, abundance, abundance_community, nind_m2)
  
  #Artificially creating the same percentage of NA as
  # there are in my original database and keeping the same variables as in the original imputation
  
mice_check %>% 
  filter(is.na(nind_m2)) %>% 
  nrow()

check_subset <- mice_check %>% 
  filter(is.na(nind_m2)) %>% 
  select(sampling, plot, treatment, code, abundance, abundance_community) %>%  # Keeping only necessary identifiers
  left_join(nind_nona %>% select(sampling, plot, treatment, code, abundance, abundance_community, nind_m2), 
            by = c("sampling", "plot", "treatment", "code", "abundance", "abundance_community")) %>% 
  rename(nind_m2_original = nind_m2)

common <- check_subset %>% 
  select(code, sampling, plot, treatment)


#mice_check_imputed <- mice(mice_check, method = "rf", m = 5, maxit = 200) # I tried with more iterations (maxit) but there was no difference


mice_check_imputed <- mice(mice_check, method = "rf", m = 10, maxit = 100)


#saveRDS(mice_check_imputed , "data/mice_check_imputed .rds")

mice_check_imputed  <- readRDS("data/mice_check_imputed .rds")

plot(mice_check_imputed) # Check if lines stabilize
stripplot(mice_check_imputed, pch = 20, cex = 1.2) #If imputed values (blue dots) align well with observed values, MICE is working well.
densityplot(mice_check_imputed)

mice_check_imputed_db <- complete(mice_check_imputed)

mice_check_imputed_db <- mice_check_imputed_db %>% 
  mutate(nind_m2_imputed = nind_m2) %>% 
  select(-nind_m2)

reliab <- right_join(mice_check_imputed_db, check_subset)

ggplot(reliab, aes(x = nind_m2_original, y = nind_m2_imputed)) +
  geom_point() +
  geom_smooth(method = "lm", se = T, aes(color = "red"))

summary(lm(nind_m2_imputed ~ nind_m2_original, reliab))
















mice_check_imputed <- mice(mice_check, method = "rf", m = 20, maxit = 10)

# Extract the imputed datasets
mice_check_imputed_complete <- complete(mice_check_imputed, action = "long")

# Merge with the original values to compare
check_subset$.imp <- 1  


reliab <- mice_check_imputed_complete %>%
  left_join(check_subset, by = c("sampling", "plot", "treatment", "code", "abundance", "abundance_community", ".imp")) %>%
  rename(nind_m2_original = nind_m2_original, 
         nind_m2_imputed = nind_m2) 

ggplot(reliab, aes(x = nind_m2_original, y = nind_m2_imputed)) +
  geom_point(alpha = 0.5, aes(color = .imp)) +
  geom_smooth(method = "lm", se = FALSE, aes(group = .imp, color = factor(.imp))) +
  labs(title = "Comparison of Imputed vs. Original Values",
       x = "Original nind_m2",
       y = "Imputed nind_m2") +
  theme_minimal()




mice_check_imputed_complete <- mice_check_imputed_complete %>%
  rename(nind_m2_imputed = nind_m2)



reliab <- mice_check_imputed_complete %>%
  left_join(select(mice_check, sampling, plot, treatment, code, abundance, abundance_community, nind_m2),
            by = c("sampling", "plot", "treatment", "code", "abundance", "abundance_community")) %>%
  rename(nind_m2_original = nind_m2)

ggplot(reliab, aes(x = nind_m2_original, y = nind_m2_imputed, color = factor(.imp))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, aes(group = .imp, color = factor(.imp))) +
  labs(title = "Comparison of Imputed vs. Original Values (20 Imputations)",
       x = "Original nind_m2",
       y = "Imputed nind_m2",
       color = "Imputation Iteration") +
  theme_minimal()


