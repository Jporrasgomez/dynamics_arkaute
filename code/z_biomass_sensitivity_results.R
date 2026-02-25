


rm(list = ls(all.names = TRUE))
pacman::p_load(dplyr, reshape2, tidyverse, lubridate, ggplot2, ggpubr, rpivotTable, ggrepel, here)

source("code/palettes_labels.R")



 { flora_raw <- read.csv("data/flora_db_raw.csv")
  
  flora_raw <- flora_raw %>%
    mutate(across(where(is.character), as.factor),
           plot = factor(plot),
           sampling = factor(sampling, levels = sort(unique(flora_raw$sampling))),
           treatment =  factor(flora_raw$treatment, levels =  c("c", "w", "p", "wp")),
           plot = factor(plot, levels = sort(unique(flora_raw$plot)))) %>% 
    select(-date, -category, -OBS, -ni.agrupado, -familia, -species_old_1, -species_old_2)
  
  
  # Adding dates
  sampling_dates <- read.csv("data/sampling_dates.csv") %>% 
    mutate(sampling = as.factor(sampling),
           date = ymd(date), 
           month = month(date, label = TRUE),
           day = day(date), 
           year = year(date)) %>% 
    select(sampling, date, day, month, year, one_month_window, omw_date) %>% 
    mutate(across(where(is.character), as.factor))
  
  
  flora_rare <- flora_raw %>% 
    right_join(sampling_dates, by = join_by(sampling)) %>% 
    select(sampling, one_month_window, omw_date, plot, treatment,
           code, abundance, height, Cb, Db, Cm, Dm, date, month)
  
  
  #Adding species information
  species_code <- read.csv("data/species_code.csv") %>% 
    select(species, code, family, genus_level, species_level, growing_type) %>%
    mutate(across(where(is.character), as.factor))
  
  
  # WE WORK WITH IDENTIFIED SPECIES!!
  
  flora_rare <- merge(flora_rare, species_code, by = "code") 
  
  
  # Modification of 0.1 cm in order to avoid caliber error. 
  
  flora_rare <- flora_rare %>%
    mutate(Dm = coalesce(ifelse(Dm < 0.1, 0.1, Dm), Dm))
  
  flora_rare <- flora_rare %>%
    mutate(Db = coalesce(ifelse(Db < 0.1, 0.1, Db), Db))
  
  
  ## BIOMASS AT INDIVIDUAL LEVEL ####
  
  # Ecuación biomasa####                 
  #Transforming diameters into circumferences
  flora_rare$cm <- round(ifelse(!is.na(flora_rare$Dm), flora_rare$Dm * pi, flora_rare$Cm), 2)
  flora_rare$cb <- round(ifelse(!is.na(flora_rare$Db), flora_rare$Db * pi, flora_rare$Cb), 2)
  
  #Calculating area of circle with circumference values
  flora_rare$Ah <- ((flora_rare$cm)^2)/(4*pi)
  flora_rare$Ab <- ((flora_rare$cb)^2)/(4*pi)
  
  
  taxongroups <- flora_rare %>%
    filter(code %in% c("poaceae", "asteraceae", "tosp", "orchidaceae"))%>%
    group_by(code, sampling, one_month_window, omw_date, plot, treatment, date,
             month, species, family, genus_level, species_level) %>%
    summarize(abundance = sum(unique(abundance), na.rm = T), #here we sum abundances 
              height = mean(height, na.rm = T),
              Ah = mean(Ah, na.rm = T),
              Ab = mean(Ab, na.rm = T))
  
  species <- anti_join(flora_rare, taxongroups, by = "code") %>%
    group_by(code, sampling, one_month_window, omw_date, plot, treatment, date,
             month, species, family, genus_level, species_level) %>%
    summarize(abundance = mean(abundance, na.rm = T), #here we mean abundances (fake mean, species in the same plot and sampling have the same abundance info)
              height = mean(height, na.rm = T),
              Ah = mean(Ah, na.rm = T),
              Ab = mean(Ab, na.rm = T))
  
  flora_medium <- bind_rows(taxongroups, species)
  
  
  d <- 1.96
  z <- 2/3
  flora_medium$x <- (flora_medium$height/2)*(flora_medium$Ab + flora_medium$Ah)
  
  flora_medium$biomass_i <- d*(flora_medium$x^z)

  
  
  # Senstitivity analysis for z 
  
  z_vector <- c(1/6, 1/3, 1/2, 5/6, 3/3, 7/6)
  
  for (i in seq_along(z_vector)) {
    
    # etiqueta estable para el nombre de columna
    z_lbl <- gsub("/", "_", as.character(MASS::fractions(z_vector[i])))  # "2/3" -> "2_3"
    colname <- paste0("biomass_z_", z_lbl)
    
    flora_medium[[colname]] <-  d * (((flora_medium$height/2) * (flora_medium$Ab + flora_medium$Ah))^z_vector[i])
  }
  
  
  
  flora_medium <- flora_medium  %>% 
    rename(original_biomass_i = biomass_i) %>% 
    pivot_longer(
      cols = starts_with("biomass"), 
      names_to = "z", 
      values_to = "biomass_i_z"
    ) 
  
  
  # RICHNESSS ########
  
  flora_medium <- flora_medium %>% 
    group_by(plot, sampling) %>% 
    mutate(richness = n_distinct(code, na.rm = T)) %>% 
    ungroup()
  
  # ABUNDANCE ########
  
  flora_medium <- flora_medium %>% 
    group_by(plot, sampling) %>% 
    mutate(abundance_community = sum(abundance, na.rm = T)) %>% 
    ungroup()

  
  ## BIOMASS AT SPECIES LEVEL ###########
  
  
  flora_biomass_raw <- flora_medium %>% 
    filter(!sampling %in% c("0", "1", "2", "12")) %>%  # Remove samplings for which we have no morphological measurements
    filter(!is.na(x)) %>%                              # Remove rows where x is NA since we do not have morph. meas. for those datapoints
    filter(height > 5 & height < 200)               # Remove individuals with height < 5 cm or > 200 cm, because the equation does not properly work with them 
  

## NUMBER OF INDIVIDUALS ####


plots <- read.csv("data/plots.csv") %>% 
  select(plot, treatment_code) %>% 
  rename(treatment = treatment_code)

nind1 <- read.csv("data/n_individuals.csv") %>% 
  merge(plots) %>% 
  select(sampling, treatment,  plot, code, abundance, nind_m2) %>%
  group_by(sampling, treatment, plot, code) %>%
  summarize(nind_m2 = sum(nind_m2), abundance = sum(abundance)) %>% 
  mutate(sampling = as.factor(sampling),
         treatment = as.factor(treatment), 
         plot = as.factor(plot), 
         code = as.factor(code), 
         nind_m2 = as.numeric(nind_m2), 
         abundance = as.numeric(abundance))

nind2 <- flora_raw %>%
  filter(!sampling %in% c( "12", "13", "14", "15", "16", "17", "18", "19", "20"))  %>%
  group_by(sampling, treatment, plot, code) %>%
  mutate(n_individuals = n()) %>%
  group_by(sampling, treatment, plot, code, abundance) %>%
  summarize(nind_m2 = mean(n_individuals, na.rm = T)) %>%
  filter(nind_m2 < 5) 

nind <- bind_rows(nind1, nind2)


# Integrating number of individuals data within biomass information

biomass_nind <- flora_biomass_raw %>% 
  merge(species_code) %>% 
  left_join(nind) %>% 
  mutate(
    year = year(date))


####### CALCULATION OF BIOMASS AT SPECIES LEVEL #############

## CALCULATION OF BIOMASS AT SPECIES LEVEL WITHOUT IMPUTATIONS

biomass_just <- biomass_nind %>% 
  mutate(biomass_s_original = nind_m2 * original_biomass_i,
         biomass_s_z = nind_m2 * biomass_i_z) %>% 
  select(sampling, treatment, plot, year, date,  one_month_window, omw_date, code, species_level, genus_level, family,
         abundance, richness, abundance_community, nind_m2, Ah, Ab, height, biomass_s_original, biomass_s_z, z)


# MICE IMPUTATION

#library(mice)
#
#z_unique <- unique(biomass_nind$z)
#mice_list <- list()
#
#for (i in seq_along(z_unique)){
#  
#data <- biomass_nind %>% 
#  filter(z == z_unique[i])
#  
#biomass_mice0 <- data %>% 
#  select(year, sampling, plot, treatment, code,
#         family, richness, abundance, abundance_community,
#         height, Ah, Ab, biomass_i_z, nind_m2) %>% 
#  mutate(across(where(is.character), as.factor))
#
#biomass_mice_imputed <- mice(biomass_mice0, method = "rf", m = 10, maxit = 300)
#
#
#imputed_db <- complete(biomass_mice_imputed, action = "long") %>% 
#  group_by(plot, treatment, sampling, code) %>% 
#  mutate(nind_m2_imputed = round(mean(nind_m2, 0)),
#         sd_imputation = sd(nind_m2)) %>% 
#  ungroup() %>% 
#  select(plot, treatment, sampling, code, nind_m2_imputed, sd_imputation) %>% 
#  distinct()
#
#imputed_db$label_imputation <- ifelse(is.na(data$nind_m2), 1, 0) # If the value nind is imputed, 1, if not, 0. 
#
#biomass_mice_results <- data %>% 
#  left_join(imputed_db) %>% 
#  mutate(biomass_s_imp = nind_m2_imputed * biomass_i_z) %>% 
#  select(sampling, treatment, plot, year, date,  one_month_window, omw_date, code, species_level, genus_level, family,
#         abundance, richness, abundance_community, biomass_s_imp, nind_m2, nind_m2_imputed, label_imputation)
#
#biomass_mice_results$z <- z_unique[i]
#
#mice_list[[i]] <- biomass_mice_results
#
#}
#

#biomass_mice <- do.call(rbind, mice_list)
#
#biomass_mice <- merge(biomass_mice, biomass_just) %>% select(-Ah, -Ab, -height)
#
#biomass_mice %>% write.csv("data/z_mice_biomass_sensitivity.csv", row.names = F)

}


biomass <- read.csv("data/z_mice_biomass_sensitivity.csv") %>% 
  select(-biomass_s_original) %>% 
  rename(biomass_s_z_imp = biomass_s_imp)


# removing outliers of imputation data

z_levels <- unique(biomass$z)
result_list <- list()
for(i in seq_along(z_levels)){
  
data <- biomass %>% 
  filter(z == z_levels[i])

Q1 <- quantile(log(data$biomass_s_z_imp), 0.25, na.rm = TRUE)
Q3 <- quantile(log(data$biomass_s_z_imp), 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

result_list[[i]] <- data %>%
  filter(log(biomass_s_z_imp) >= Q1 - 1.5 * IQR & log(biomass_s_z_imp) <= Q3 + 1.5 * IQR)

}

biomass <- do.call(rbind, result_list)



## BIOMASS AT COMMUNITY LEVEL ##

biomass_community <- biomass %>%
  group_by(plot, sampling, treatment, z) %>%
  mutate(biomass_z_mice = sum(biomass_s_z_imp, na.rm = TRUE),
         biomass_z_raw = sum(biomass_s_z, na.rm = TRUE)) %>%
  ungroup() %>% 
  select(year, date, sampling, one_month_window, omw_date, treatment, plot, code, 
         abundance, richness, abundance_community, z, biomass_z_raw,
         biomass_z_mice) %>% 
  mutate(sampling_date = as.factor(format(ymd(date), "%Y-%m-%d"))) %>% 
  distinct(treatment, plot, sampling, date, omw_date, one_month_window, z, biomass_z_raw,
           biomass_z_mice) %>% 
  mutate(z = as.factor(z))


# FIRST DATABASE for raw data (no imputation).  
biomass_community_raw_wide <- biomass_community %>% 
  select(-biomass_z_mice) %>%
  pivot_wider(
    names_from = z,
    values_from = biomass_z_raw
  )


biomass_community_mice <- biomass_community %>% 
  select(-biomass_z_raw) %>% 
  mutate(z = forcats::fct_relabel(z, ~ paste0(.x, "_mice")))

# SECOND DATABASE. 

biomass_community_mice_wide <- biomass_community_mice %>% 
  pivot_wider(
    names_from = z,
    values_from = biomass_z_mice
  )







## Including samplings 0, 1, 2 and 12 for each z subset


abundance_richness <- read.csv("data/abrich_db_plot.csv")


data_lm <- abundance_richness %>%
  full_join(biomass_community_mice,  by = c("sampling", "plot", "treatment", "date", "omw_date", "one_month_window")) %>%
  select(sampling, treatment, plot, z, biomass_z_mice, abundance) %>% 
  rename(biomass = biomass_z_mice) %>% 
  select(sampling, treatment, plot, z, biomass, abundance)


order_z <- c(
  "biomass_z_1_6_mice",
  "biomass_z_1_3_mice",
  "biomass_z_1_2_mice",
  "biomass_z_5_6_mice",
  "biomass_z_1_mice",
  "biomass_z_7_6_mice"
)

lm_nona <- data_lm %>% 
  filter(!is.na(biomass)) %>%
  mutate(z = factor(z, levels = order_z))

lm_nona %>% 
  ggplot(aes(x = abundance, y = biomass)) + 
  facet_wrap(~z, scales = "free") +
  geom_point(aes(color = treatment)) + 
  geom_smooth(method = "lm", se = TRUE, alpha = 0.05, aes(color = treatment, fill = treatment)) + 
  geom_smooth(method = "lm", se = TRUE, alpha = 0.1, color = "black") + 
  geom_smooth(method = "lm", se = TRUE, alpha = 0.1, color = "black") + 
  scale_color_manual(values = palette_CB, labels = labels) +
  theme_bw()

biomass_na <- lm0 %>% 
  filter(is.na(biomass)) %>% 
  select(-z)


# Paquetes (usa broom para obtener p-value del F-test cómodamente)
library(dplyr)
library(broom)

# Function to get estimates

fit_one <- function(df) {
  m <- lm(biomass ~ abundance, data = df)
  tibble(
    R2        = summary(m)$r.squared,
    `p-value` = glance(m)$p.value,                 # p-valor global (F-test del modelo)
    slope     = coef(m)[["abundance"]],
    intercept = coef(m)[["(Intercept)"]]
  )
}

treat_levels <- unique(biomass_community_mice$treatment)
z_levels <- unique(biomass_community_mice$z)

result_list_regression <- list() 
result_list_biomass_lm <- list() 


for(i in seq_along(z_levels)){
  
  data_nona <- lm_nona %>% 
    filter(z == z_levels[i])

# LM per treatment
lm_by_trt <- data_nona %>%
  filter(treatment %in% treat_levels) %>%
  group_by(treatment) %>%
  group_modify(~ fit_one(.x)) %>%
  ungroup()

# LM for all treatment together
lm_all_row <- data_nona %>%
  fit_one() %>%
  mutate(treatment = "all") %>%
  select(treatment, everything())

# Results
result <- bind_rows(
  lm_by_trt %>% mutate(treatment = factor(treatment, levels = treat_levels)) %>% arrange(treatment),
  lm_all_row
) %>%
  select(treatment, R2, `p-value`, slope, intercept) %>% 
  mutate(z = z_levels[i])

result_list_regression[[i]] <- result

result_treatments <- result %>% 
  filter(treatment != "all")


lm_fill <- biomass_na %>% 
  left_join(result_treatments) %>% 
  mutate(
    biomass_lm = abundance * slope + intercept, 
    biomass_lm_all = abundance* result$slope[5] + result$intercept[5]
    
  ) %>% 
  select(sampling, treatment, plot, biomass_lm, biomass_lm_all, abundance, z)


# we use the biomass extrapolated by using the linear model for all treatments at once 
lm_fill <- lm_fill %>% 
  select(-biomass_lm) %>% 
  rename(biomass = biomass_lm_all)

biomass_lm <- rbind(data_nona, lm_fill) %>% 
  select(sampling, treatment, plot, biomass, z) %>% 
  mutate(
    biomass = ifelse(biomass < 0, 1, biomass)  # Adding 1 unit of biomass to those estimations under 0 
  ) %>% 
  mutate(
    biomass = ifelse(sampling == "1" & treatment %in% c("p", "wp"),
                             0,
                     biomass)
  ) 

result_list_biomass_lm[[i]] <- biomass_lm

}


results_regressions <- do.call(rbind, result_list_regression)
biomass_community_mice_lm <- do.call(rbind, result_list_biomass_lm)



# Third database
biomass_community_mice_lm_wide <- biomass_community_mice_lm %>% 
  mutate(z = forcats::fct_relabel(z, ~ paste0(.x, "_lm"))) %>% 
  pivot_wider(
    names_from = z,
    values_from = biomass
  )



sensitivity_biomass <- merge(biomass_community_raw_wide, biomass_community_mice_wide) %>% 
  right_join(biomass_community_mice_lm_wide)

#adding original results of biomass (z = 2/3)

biomass_orignal_raw <- read.csv("data/biomass_no_imputation.csv") %>%  rename(biomass_original_raw = biomass)

biomass_original_mice <- read.csv("data/biomass_mice_imputation.csv") %>%  rename(biomass_original_mice = biomass)

biomass_original_mice_lm <- read.csv("data/biomass_mice_lm.csv") %>%  rename(biomass_original_mice_lm = biomass_mice_lm)

sensitivity_biomass <- sensitivity_biomass %>% 
  left_join(biomass_orignal_raw) %>% 
  left_join(biomass_original_mice) %>% 
  left_join(biomass_original_mice_lm) %>% 
  select(-X)


sensitivity_biomass %>%  write.csv("results/sensitivity_biomass.csv")



sensitivity_biomass_long <- sensitivity_biomass %>% 
  pivot_longer(
    cols = starts_with("biomass"),
    names_to = "biomass_z_level", 
    values_to = 
  )

biomass_levels <- unique(sensitivity_biomass_long$biomass_z_level)

sensitivity_biomass_no0 <- sensitivity_biomass %>% 
  filter(sampling != "0")

source("code/functions/eff_size_LRR_function.R")

list_eff <- list()
for (i in seq_along(biomass_levels)){
  
  LRR_agg(sensitivity_biomass_no0, biomass_levels[i])
  
  list_eff[[i]] <- effsize_data
  
  rm(effsize_data)
}

eff_size_agg <- do.call(rbind, list_eff) 

# Storing data for checking results

eff_size_agg <- eff_size_agg %>% 
  mutate(
    eff_value = round(eff_value, 2),
    lower_limit = round(lower_limit, 2),
    upper_limit = round(upper_limit, 2)
  ) %>% 
  select(eff_descriptor, variable, eff_value, lower_limit, upper_limit, null_effect)





{

data <- eff_size_agg %>% 
  filter(eff_descriptor == "wp_vs_p") %>% 
  mutate(
    eff_descriptor = factor(eff_descriptor),
    x_jit = (as.integer(eff_descriptor) - 2) 
  )

ggplot(data, aes(
  x = variable,                 # centrado en 0 + pequeño desplazamiento
  y = eff_value,
  color = variable
)) +
  #facet_wrap( scales = "free_y") +
  
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "black", linewidth = 0.5) +
  
  
  geom_linerange(aes(ymin = lower_limit, ymax = upper_limit),
                 linewidth = 1, alpha = 1) +
  
  geom_point(size = 2.5) +
  
  geom_text(aes(
    y = ifelse(eff_value < 0, lower_limit - 0.5, upper_limit + 0.5),
    label = ifelse(null_effect == "NO", "*", NA_character_)
  ),
  show.legend = FALSE,
  size = 8) +
  
  #scale_color_manual(values = palette, labels = labels) +
  
  # Chat gpt help
  #scale_x_continuous(limits = c(-1.8 , 1.8 ), expand = expansion(mult = 0.1)) +
  
  #scale_y_continuous( breaks = scales::pretty_breaks(n = breaks_axix_y), 
  #                    # añade margen relativo por abajo y por arriba (5% y 25% como ejemplo)
  #                    expand = expansion(mult = c(0.05, 0.1)) ) +
  #
  
  labs(x = NULL, y = NULL, color = NULL) +
  
  gg_RR_theme +
  theme(
    strip.background   = element_blank(),
    strip.placement    = "outside",
    strip.text         = element_text(face = "bold", size = 12),
    strip.text.y.left  = element_text(face = "bold"),
    axis.text.y        = element_text(angle = 90, hjust = 0.5, face = "plain", size = 12),
    axis.text.x        = element_blank(),
    axis.ticks.x       = element_blank(),
    legend.position    = "bottom",
    legend.text        = element_text(size = 14, face = "plain")
  )

}







