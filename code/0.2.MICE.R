





biomass <- flora_biomass_raw

biomass <- merge(biomass, species_code)

biomass <- left_join(biomass, nind)

sum(is.na(biomass$nind_m2)) 

sum(is.na(biomass$nind_m2)) /length(biomass$code) * 100


biomass$year <- year(biomass$date)



#Where are the NA's?




ggNA_sampling <- biomass %>% 
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



ggNA_plot <- biomass %>% 
  mutate(cell_content = case_when(is.na(nind_m2) ~ "NA",
                                  !is.na(nind_m2)  ~ "nind_available")) %>%
  ggplot(aes(x = as.numeric(plot))) +
  geom_histogram(aes(fill = cell_content),
                 binwidth = .5, center = 0) +
  scale_fill_discrete(drop = F) +
  labs(y = "Number of rows", x = "Plot") +
  scale_x_continuous(breaks = scales::breaks_extended(n = 16))


ggNA_species <- biomass %>% 
  mutate(cell_content = case_when(is.na(nind_m2) ~ "NA",
                                  !is.na(nind_m2)  ~ "nind_available")) %>%
  # we create a new variable capturing cell content
  # as we are interested in defining 3 different situations, we use the case_when function
  ggplot(aes(y = code)) +
  geom_bar(aes(fill = cell_content)) +
  scale_fill_discrete(drop = F) +
  labs(x = "Number of rows")

#NA's seem to be concentrated in the first year, as we already knew
# NA's are randomly distributed across plots
# NA's are randomly distributed across species, but we can see some that have more than others like: 
# poaceae, coar or shar



na <- biomass

na <- na %>% 
  mutate(nind_m2_na = ifelse(is.na(na$nind_m2), -1, na$nind_m2))


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

#for (i in seq_along(plots)) {
#  print(nalist[[i]])
#  
#}


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

######### LONG TIME TO RUN LOOP : ###########################################################
#source("code/0.2.loop_reliability.R")
######### LONG TIME TO RUN LOOP : ###########################################################


#stability_db_combined <- imap_dfr(stability_list, ~ mutate(.x, counter = .y)) 
#stability_db_combined %>% write.csv("data/stability_test.csv", row.names = F)
stability_test <- read.csv(here("data", "stability_test.csv"))


ggstability_test1 <- ggplot(stability_test, aes(x = CV)) +
  facet_wrap(~ counter, nrow = 2, ncol = 3 ) + 
  geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = 1), color = "red", linetype = "dashed", size = 1) +
  scale_x_continuous(
    expand = expansion(mult = c(0.05, 0.1)),
    breaks = seq(0, max(imputed_db$sd_imputation / imputed_db$nind_m2_imputed, na.rm = TRUE), by = 0.5) # Adjust "by" as needed
  ) +
  labs(title = "Distribution of Coefficient of Variation for Imputed Values",
       x = "(SD/mean) Ratio",
       y = "Count") 

ggstability_test2 <- ggplot(stability_test, aes(y = CV)) +
  facet_wrap(~ counter, nrow = 1, ncol = 6 ) + 
  geom_boxplot(fill = "steelblue", color = "black", alpha = 0.8) +
  geom_hline(aes(yintercept = 1), color = "red", linetype = "dashed", size = 1) +
  labs(title = "Distribution of Coefficient of Variation for Imputed Values",
       x = NULL,  # Removes x-axis label
       y = "(SD/mean) Ratio") +
  theme(axis.text.x = element_blank(),  # Removes x-axis text
        axis.ticks.x = element_blank()) # Removes x-axis ticks



#reliability_db_combined <- imap_dfr(reliability_db_list, ~ mutate(.x, counter = .y))
#reliability_db_combined %>% write.csv("data/reliability_test.csv", row.names = F)

reliability_test <- read.csv(here("data", "reliability_test.csv")) %>% 
  mutate(counter = as.factor(counter))

ggreliability_test1 <- ggplot(reliability_test, aes(y = R2, x = p_value, color = counter)) +
  geom_point(alpha = 0.8) +
  #geom_hline(aes(yintercept = 1), color = "red", linetype = "dashed", size = 1) +
  labs(title = "R2 and p_value distribution for lm(imputed ~ original)",
       x = "p_value",  # Removes x-axis label
       y = " R2", 
       color = "Imputation")


ggreliability_test2 <-ggplot(reliability_test, aes(x = R2)) +
  geom_histogram(bins = 30, fill = "red4", color = "white", alpha = 0.8) +
  scale_x_continuous(
    expand = expansion(mult = c(0.05, 0.1)),
    breaks = seq(0, max(imputed_db$sd_imputation / imputed_db$nind_m2_imputed, na.rm = TRUE), by = 0.1) # Adjust "by" as needed
  ) +
  labs(title = "R2 for lm(imputed ~ original)",
       x = "R2",
       y = "Count") 

#reliability_plot_combined <- imap_dfr(reliability_plot_list, ~ mutate(.x, counter = .y))
#reliability_plot_combined %>% write.csv("data/reliability_LM_test.csv", row.names = F)

reliability_LM_test <- read.csv(here("data", "reliability_LM_test.csv")) %>% 
  mutate(counter = as.factor(counter))


ggLM_test <- ggplot(reliability_LM_test, aes(x = nind_m2_original,
                                      y = nind_m2,
                                      color = as.factor(.imp))) + 
  facet_wrap(~ counter, nrow = 2, ncol = 3) +
  geom_point(alpha = 0.6, size = 0.3) + 
  geom_smooth(method = "lm", se = FALSE, size = 0.5) +
  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),  
           method = "pearson", 
           label.x.npc = "left", 
           label.y.npc = "top") + # Displays R² and p-value
  guides(color = "none") # Improve legend label



## Merging imputation results with biomass database : 

biomass_imp <- biomass %>% 
  select(year, date, sampling, plot, code, species_level, genus_level, family,
         abundance, height, Ah, Ab, x, biomass_i, richness, abundance_community, nind_m2) %>% 
  left_join(imputed_db)




  