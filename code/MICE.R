





#We calculate the mean area of the individual by taking Ah and Ab both and transform it into m2 (cm2/10000)
biomass <- flora_biomass_raw

biomass <- merge(biomass, species_code)

biomass <- left_join(biomass, nind)

sum(is.na(biomass$nind_m2)) 

sum(is.na(biomass$nind_m2)) /length(biomass$code) * 100


biomass$year <- year(biomass$date)



#Where are the NA's?




biomass %>% 
  mutate(cell_content = case_when(is.na(nind_m2) ~ "NA",
                                  !is.na(nind_m2)  ~ "nind_available")) %>%
  # we create a new variable capturing cell content
  # as we are interested in defining 3 different situations, we use the case_when function
  ggplot(aes(x = as.numeric(sampling))) +
  geom_histogram(aes(fill = cell_content),
                 binwidth = .5, center = 0) +
  scale_fill_discrete(drop = F) +
  labs(y = "Number of rows", y = "Sampling") +
  scale_x_continuous(breaks = scales::breaks_extended(n = 21))



biomass %>% 
  mutate(cell_content = case_when(is.na(nind_m2) ~ "NA",
                                  !is.na(nind_m2)  ~ "nind_available")) %>%
  ggplot(aes(x = as.numeric(plot))) +
  geom_histogram(aes(fill = cell_content),
                 binwidth = .5, center = 0) +
  scale_fill_discrete(drop = F) +
  labs(y = "Number of rows", x = "Plot") +
  scale_x_continuous(breaks = scales::breaks_extended(n = 16))


biomass %>% 
  mutate(cell_content = case_when(is.na(nind_m2) ~ "NA",
                                  !is.na(nind_m2)  ~ "nind_available")) %>%
  # we create a new variable capturing cell content
  # as we are interested in defining 3 different situations, we use the case_when function
  ggplot(aes(y = code)) +
  geom_bar(aes(fill = cell_content),
           binwidth = .5, center = 0) +
  scale_fill_discrete(drop = F) +
  labs(x = "Number of rows")

#NA's seem to be concentrated in the first year, as we already knew
# NA's are randomly distributed across plots
# NA's are randomly distributed across species, but we can see some that have more than others like: 
# poaceae, coar or shar



na <- biomass
summary(na)

na$nind_m2_na <- ifelse(is.na(na$nind_m2), -1, na$nind_m2)

plots <- sort(unique(biomass$plot))
nalist <- list()
count = 0

for(i in seq_along(plots)){
  
  count = count + 1
  
  nalist[[count]]<- 
    ggplot(subset(na, plot == plots[i]), aes(x = sampling, y = code, fill = nind_m2_na)) +
    geom_tile(color = "gray13") +  # Keep black grid lines
    geom_text(aes(label = nind_m2_na), size = 2.3) +  # Add text labels
    scale_fill_gradientn(
      colors = c("red3", "#A9D8F2", "#2A5D9C"),  # Very pale gray (#F0F0F0), white, orange
      values = scales::rescale(c(-1, 1, max(na$nind_m2, na.rm = TRUE))),
      name = "Observations"
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),  # Remove default grid lines
      axis.text.y = element_text(face = "italic"),  # Italicize species names
      legend.position = "bottom"  # Move legend to the bottom
    ) +
    labs(x = "Samplings", y = "Species", title = paste0("Plot ", plots[i]))
  
}

for (i in seq_along(plots)) {
  print(nalist[[i]])
  
}


# MICE #####


library(mice)

biomass_mice <- biomass %>% 
  select(year, sampling, plot, treatment, code, family, richness, abundance, abundance_community,
         height, Ah, Ab, biomass_i, nind_m2) %>% 
  mutate(across(where(is.character), as.factor))

# We use mice package
##| method = "rf". Random forest models allow us to use both numerical and factorial variables in the imputation. Therefore, code, sampling and plot are used
##| m = 10 is the number of datasets that mice creates. This is, 10 values of nind_m2
##| maxit is the number of times that mice iterates through each dataset. The higher, the more stable (in complex database)


#biomass_mice_imputed <- mice(biomass_mice, method = "rf", m = 10, maxit = 300)
#saveRDS(biomass_mice_imputed, "data/biomass_mice_imputed.rds")
biomass_mice_imputed <- readRDS("data/biomass_mice_imputed.rds")

summary(biomass_mice_imputed)
plot(biomass_mice_imputed) # Check if lines stabilize
#stripplot(biomass_mice_imputed, pch = 20, cex = 1.2) #If imputed values (blue dots) align well with observed values, MICE is working well.
densityplot(biomass_mice_imputed)


i = 9
imputed_dbi <- complete(biomass_mice_imputed, action = i)
ggplot() +
  geom_density(aes(x = biomass$nind_m2), color = "blue3") +
  geom_density(aes(x = imputed_dbi$nind_m2), color = "red3") +
  labs(title = "Density Plot of Original (Red) vs Imputed (Blue) nind_m2")


imputed_db <- complete(biomass_mice_imputed, action = "long")

# hay que intentar entender bien este gráfico
ggplot() +
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

imput_stability <- imputed_db %>% 
  filter(label_imputation == 1) %>% 
  mutate(CV = sd_imputation / nind_m2_imputed) %>% 
  ggplot(aes(x = CV)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = 1), color = "black", linetype = "dashed", size = 1) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  labs(title = "Distribution of Coefficient of Variation for Imputed Values",
       x = "(SD/mean) Ratio",
       y = "Count") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))



# Reliability

# Checking reliability of mice imputation 
# Let's recreate
perc_NA <- ((biomass %>% 
               filter(is.na(nind_m2)) %>% 
               nrow())) / nrow(biomass)

##| I create a database where there are no NA in nind_m2.
##| Also, in this db (nind_nona) i take out the species for which we always have found 1 individuals so they do not
##| play a role in the original imputation because always nind_m2 = 1. And if we randomly create an NA in one of these species
##| the imputation stability might be negatively and unnecessarilyaffected 

nind_nona <- biomass %>% 
  filter(!is.na(nind_m2)) %>%  
  filter(!code %in% one_ind_species)




#Artificially creating the same percentage of NA as
# there are in my original database  (perc_NA) and keeping the same variables as in the original imputation

mice_check <- nind_nona %>%
  mutate(nind_m2 = ifelse(runif(n()) < perc_NA, NA, nind_m2)) %>% 
  select(year, sampling, plot, treatment, code, family, richness, abundance, abundance_community,
         height, Ah, Ab, biomass_i, nind_m2) %>% 
  mutate(across(where(is.character), as.factor))



# Dejo creada la base de datos de los NA creados artificialmente para nind_m2
check_subset <- mice_check %>% 
  filter(is.na(nind_m2)) %>% 
  select(sampling, plot, treatment, code, abundance, abundance_community) %>%  # Keeping only necessary identifiers
  left_join(nind_nona %>% select(sampling, plot, treatment, code, abundance, abundance_community, nind_m2), 
            by = c("sampling", "plot", "treatment", "code", "abundance", "abundance_community")) %>% 
  rename(nind_m2_original = nind_m2)

# Same imputation as original 
mice_check_imputed <- mice(mice_check,method = "rf", m = 10, maxit = 5) 
#mice_check_imputed <- mice(mice_check,method = "rf", m = 10, maxit = 300) 

#saveRDS(mice_check_imputed, "data/tmice_check_imputed.rds")
#mice_check_imputed <- readRDS("data/mice_check_imputed.rds")


           

imput_reliability_test <- complete(mice_check_imputed, action = "long") %>%  
  mutate(.imp = as.factor(.imp)) %>%      # Convert to factor
  left_join(check_subset) %>%             # Adding original values of nind_m2
  filter(!is.na(nind_m2_original)) %>%    # Keeping only original values
  ggplot(aes(x = nind_m2_original, y = nind_m2, color = as.factor(.imp))) + 
    geom_point(alpha = 0.6) + 
    geom_smooth(method = "lm", se = FALSE) +
    stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),  
             method = "pearson", 
             label.x.npc = "left", 
             label.y.npc = "top") + # Displays R² and p-value
    theme_minimal() +
    labs(color = "Imputation Number") # Improve legend label
  
  



biomass_imp <- biomass %>% 
  select(year, date, sampling, plot, code, species_level, genus_level, family,
         abundance, height, Ah, Ab, x, biomass_i, richness, abundance_community, nind_m2) %>% 
  left_join(imputed_db)


# hay que capar el nind_m2 imputado, sobre todo por arriba. LA idea es que nind_m2_imputed para una especie nunca sea mayor
# que nind_m2 para esa misma especie en ese sampling

codes <- sort(unique(biomass_imp$code))
samps <- sort(unique(biomass_imp$samp))

for (i in seq_along(samps)){
  
  samp_i <- biomass_imp %>% 
    filter(sampling == samps[i])
  
  for (j in seq_along(codes)){
    
    # codigos presentes en ese muestreo
    code_j <- samp_i %>% 
      filter(code == codes[j]) %>% 
      mutate(nind_m2_try = max(nind_m2), na.rm = T)
    
   
    
  }
  
}



##library(broom)
##library(purrr)  # Ensure purrr is loaded for map_dbl()
##
### Step 1: Prepare Data
##data_for_lm <- complete(mice_check_imputed, action = "long") %>%
##  left_join(check_subset) %>%
##  filter(!is.na(nind_m2_original))
##
### Step 2: Compute R² and p-value for each imputation level
##lm_results <- data_for_lm %>%
##  group_by(.imp) %>%
##  summarise(
##    lm_model = list(lm(nind_m2 ~ nind_m2_original, data = pick(everything()))),  # Use pick()
##    .groups = "drop"
##  ) %>%
##  mutate(
##    r_squared = map_dbl(lm_model, ~summary(.)$r.squared),  # Extract R²
##    p_value = map_dbl(lm_model, ~glance(.)$p.value)
##    
##  ) %>%
##  select(.imp, r_squared, p_value)
##
### Step 3: Merge R² and p-value back into the dataset
##data_for_plot <- data_for_lm %>%
##  left_join(lm_results, by = ".imp")
##
### Step 4: Create the ggplot
##ggplot(data_for_plot, aes(x = nind_m2_original, y = nind_m2, color = as.factor(.imp))) + 
##  geom_point(alpha = 0.6) + 
##  geom_smooth(method = "lm", se = FALSE) +
##  geom_text(data = lm_results, 
##            aes(x = min(data_for_plot$nind_m2_original, na.rm = TRUE),  # Align to the left
##                y = max(data_for_plot$nind_m2, na.rm = TRUE) - (.imp * 2),  # Space labels out
##                label = paste0("R² = ", round(r_squared, 3), 
##                               ", p-value = ", signif(p_value, 3)), 
##                color = as.factor(.imp)),  # Match text color with regression color
##            size = 4, hjust = 0, vjust = 1) +  # Left align text
##  theme_minimal() +
##  labs(color = "Imputation Number")
