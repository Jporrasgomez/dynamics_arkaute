






# Revisar samplin 15, plot 5, especie shar. Le faltan datos.
# Reisar sampling 17, plot 3, especie apar. Le faltan datos. 



rm(list = ls(all.names = TRUE))
pacman::p_load(dplyr, reshape2, tidyverse, lubridate, ggplot2, ggpubr, rpivotTable, ggrepel, here)

source("code/palettes_labels.R")
{
  
  flora_raw <- read.csv("data/flora_db_raw.csv")
  
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
  
  
  #At the first samplings, we were collecting information of all possible species. With the pass of time, we
  # realized we could not identify every species at every sampling,
  
  # "poaceae" here stands for the code we use for all those species from family Poaceae that we could not track across time
  # Same for "asteraceae" and "orchidaceae"
  
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
  
  
  flora_abrich <- flora_medium  # Database for richness and abundance data
  
  
  
  
  flora_medium$code_ordered <- reorder(flora_medium$code, -flora_medium$abundance, FUN = mean)
  
  flora_nobs <- flora_medium %>% 
    group_by(code) %>% 
    summarize(n_observations= n(),
              mean_abundance = mean(abundance))
  
  flora_nobs$abnobs <- flora_nobs$mean_abundance * flora_nobs$n_observations
  
  flora_nobs$code_nobs <- factor(flora_nobs$code,
                                 levels = flora_nobs$code[order(flora_nobs$n_observations, decreasing = TRUE)])
  
  ## BIOMASS AT SPECIES LEVEL ###########
  
  
  flora_biomass_raw <- flora_medium %>% 
    filter(!sampling %in% c("0", "1", "2", "12")) %>%  # Remove samplings for which we have no morphological measurements
    filter(!is.na(x)) %>%                              # Remove rows where x is NA since we do not have morph. meas. for those datapoints
    filter(height > 5 & height < 200)               # Remove individuals with height < 5 cm or > 200 cm, because the equation does not properly work with them 
  
  f_5_200 <- flora_medium %>% 
    filter(!(height > 5 & height < 200)) %>% 
    nrow() # Only 187 datapoints are lost
  
  (f_5_200/nrow(flora_medium)) * 100
  
  length(unique(flora_biomass_raw$code))
  
  length(unique(flora_medium$code))
  
}

#There is no loss of species by taking out individuals with height > 5 cm 

## NUMBER OF INDIVIDUALS ####

# To be able to estimate abundance based on the number of individuals we need data were abundance and number of individuals per species,
# plot and sampling were available. Since we have been learning the application of this non-destructive methodology in the go,
# we have 2 different groups of data:

#1.  Since sampling 12, we have been estimating the number of individuals per species and plot based on direct field observations.

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

## Las especies de asteraceae que hemos agrupado por familia se recalculan sus individuos y abundancias aquí

#2.  Up to sampling 11, We only measured the morphological parameters of 5 individuals per specie as maximum. 
#    If there were less than 5 species, we know that number was the total amount of individuals in the plot.

nind2 <- flora_raw %>%
  filter(!sampling %in% c( "12", "13", "14", "15", "16", "17", "18", "19", "20"))  %>%
  group_by(sampling, treatment, plot, code) %>%
  mutate(n_individuals = n()) %>%
  group_by(sampling, treatment, plot, code, abundance) %>%
  summarize(nind_m2 = mean(n_individuals, na.rm = T)) %>%
  filter(nind_m2 < 5) 

# Final database for number of individuals

nind <- bind_rows(nind1, nind2)

##| Plus*
##| There are some species for which we only have measured 1 individual per plot and sampling every time we have spot it.
##|  Therefore, for these species we do not need to estimate the number of individuals.
##|  In think is useful to keep a vector

one_ind_species <- c("rucr", "amsp", "kisp")


# Integrating number of individuals data within biomass information

biomass_nind <- flora_biomass_raw %>% 
  merge(species_code) %>% 
  left_join(nind) %>% 
  mutate(
    year = year(date))

biomass_nind %>%  write.csv("data/biomass_nind_for_mice.csv")


####### CALCULATION OF BIOMASS #############



## CALCULATION OF BIOMASS AT SPECIES LEVEL WITHOUT IMPUTATIONS

biomass_just <- biomass_nind %>% 
  mutate(biomass_s = nind_m2 * biomass_i) %>% 
  select(sampling, treatment, plot, year, date,  one_month_window, omw_date, code, species_level, genus_level, family,
         abundance, richness, abundance_community, biomass_s, nind_m2)


# Removing outliers
#  Q1 <- quantile(log(biomass_just$biomass_s), 0.25, na.rm = TRUE)
#  Q3 <- quantile(log(biomass_just$biomass_s), 0.75, na.rm = TRUE)
#  IQR <- Q3 - Q1
#  biomass_just <- biomass_just %>%
#    filter(log(biomass_s) >= Q1 - 1.5 * IQR & log(biomass_s) <= Q3 + 1.5 * IQR)



## IMPUTATION OF is.na(nind_m2) with MICE (see script "1.1.MICE.R)  ####
library(mice)

biomass_mice_imputed <- readRDS("data/biomass_mice_imputed.rds")

imputed_db <- complete(biomass_mice_imputed, action = "long") %>% 
  group_by(plot, treatment, sampling, code) %>% 
  mutate(nind_m2_imputed = round(mean(nind_m2, 0)),
         sd_imputation = sd(nind_m2)) %>% 
  ungroup() %>% 
  select(plot, treatment, sampling, code, nind_m2_imputed, sd_imputation) %>% 
  distinct()

imputed_db$label_imputation <- ifelse(is.na(biomass_nind$nind_m2), 1, 0) # If the value nind is imputed, 1, if not, 0. 

biomass_mice <- biomass_nind %>% 
  left_join(imputed_db) %>% 
  mutate(biomass_s = nind_m2_imputed * biomass_i) %>% 
  select(sampling, treatment, plot, year, date,  one_month_window, omw_date, code, species_level, genus_level, family,
         abundance, richness, abundance_community, biomass_s, nind_m2, nind_m2_imputed, label_imputation)

# Removing outliers
 Q1 <- quantile(log(biomass_mice$biomass_s), 0.25, na.rm = TRUE)
 Q3 <- quantile(log(biomass_mice$biomass_s), 0.75, na.rm = TRUE)
 IQR <- Q3 - Q1
 biomass_mice <- biomass_mice %>%
   filter(log(biomass_s) >= Q1 - 1.5 * IQR & log(biomass_s) <= Q3 + 1.5 * IQR)



# Old way of filling samplings 0, 1, 2 and 12 with linear regression at species level
#source("code/1.2.lm_biomass012.R")
#
#flora_medium012 <- left_join(flora_medium, lm_data_filtered) %>%
#  filter(sampling %in% c("0", "1", "2", "12")) %>%  # I COULD PUT HERE SAMPLING 12
#  mutate(year = year(date)) %>% 
#  mutate(abundance = ifelse(abundance < 1, 1, abundance)) %>% 
#  mutate(biomass_s = exp(intercept + slope * log(abundance)) * smearing_factor)
#
#biomass_imp_clean_012 <- bind_rows(biomass_imp_clean, flora_medium012) 
#
#biomass_noimp_clean_012 <- bind_rows(biomass_noimp_clean, flora_medium012)


#Calculation of total biomasss (community biomass) by summing all biomass at species level per square meter
# I will keep the flora_biomass not cleaned in order to compare results with and without outliers





## BIOMASS AT COMMUNITY LEVEL ##

biomass_just <- biomass_just %>%
  group_by(plot, sampling, treatment) %>%
  mutate(biomass_community = sum(biomass_s, na.rm = TRUE)) %>%
  ungroup() %>% 
  select(year, date, sampling, one_month_window, omw_date, treatment, plot, code, 
         abundance, richness, biomass_s, biomass_community, abundance_community) %>% 
  mutate(sampling_date = as.factor(format(ymd(date), "%Y-%m-%d"))) %>% 
  rename(biomass = biomass_community) %>% 
  distinct(treatment, plot, sampling, date, omw_date, one_month_window, biomass)


biomass_mice <- biomass_mice %>%
  group_by(plot, sampling, treatment) %>%
  mutate(biomass_community = sum(biomass_s, na.rm = TRUE)) %>%
  ungroup() %>% 
  select(year, date, sampling, one_month_window, omw_date, treatment, plot, code, 
         abundance, richness, biomass_s, biomass_community, abundance_community) %>% 
  mutate(sampling_date = as.factor(format(ymd(date), "%Y-%m-%d"))) %>% 
  rename(biomass = biomass_community) %>% 
  distinct(treatment, plot, sampling, date, omw_date, one_month_window, biomass)



# Dummy rows for 0 values of richness and abundance 

{dummy_rows_p <- matrix(nrow = 4, ncol = 8)
  colnames(dummy_rows_p) <- c("sampling", "plot", "treatment", "richness","abundance", "abundance_community", 
                              "biomass_s", "biomass_community")
  dummy_rows_p <- as.data.frame(dummy_rows_p)
  
  dummy_rows_p[] <- 0
  dummy_rows_p[, 1] <- 1
  dummy_rows_p[, 2] <- c(3, 6, 10, 15)
  dummy_rows_p[, 3] <- "p"
  
  
  dummy_rows_wp <- matrix(nrow = 4, ncol = 8)
  colnames(dummy_rows_wp) <- c("sampling", "plot", "treatment", "richness","abundance", "abundance_community", 
                               "biomass_s", "biomass_community")
  dummy_rows_wp <- as.data.frame(dummy_rows_wp)
  
  dummy_rows_wp[] <- 0
  dummy_rows_wp[, 1] <- 1
  dummy_rows_wp[, 2] <- c(4, 5, 12, 13)
  dummy_rows_wp[, 3] <- "wp"
  
  
  dummy_rows <- bind_rows(dummy_rows_p, dummy_rows_wp) %>% 
    mutate(sampling = as.factor(sampling),
           plot = as.factor(plot), 
           treatment = as.factor(treatment))
  
  dummy_rows <- right_join(dummy_rows, sampling_dates, by = join_by(sampling)) %>% 
    filter(sampling == "1") }


### Final databases: ##########


flora_abrich <- bind_rows(flora_abrich, dummy_rows)%>% 
  select(year, date, sampling, one_month_window, omw_date,  treatment, plot, code, species_level, genus_level, family, abundance,
         richness, abundance_community) %>% 
  mutate(sampling_date = as.factor(format(ymd(date), "%Y-%m-%d"))) %>% 
  rename(abundance_s = abundance, 
         abundance = abundance_community)

flora_abrich %>%  write.csv("data/flora_abrich.csv") # Database for RAD, species composition and funct.traits

abrich_db_plot <- flora_abrich %>% 
  distinct(treatment, plot, sampling, date, omw_date, one_month_window, richness, abundance)

abrich_db_plot %>%  write.csv("data/abrich_db_plot.csv")


biomass_just %>%  write.csv("data/biomass_no_imputation.csv", row.names = F)
biomass_mice %>%  write.csv("data/biomass_mice_imputation.csv", row.names = F)








