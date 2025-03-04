


# Revisar samplin 15, plot 5, especie shar. Le faltan datos.
# Reisar sampling 17, plot 3, especie apar. Le faltan datos. 



rm(list = ls(all.names = TRUE))
pacman::p_load(dplyr, reshape2, tidyverse, lubridate, ggplot2, ggpubr, rpivotTable, ggrepel)

theme_set(theme_bw() +
            theme(
              legend.position = "right",
              panel.grid = element_blank(),
              strip.background = element_blank(),
              strip.text = element_text(face = "bold"),
              text = element_text(size = 11)))


  
  flora_raw <- read.csv("data/flora_db_raw.csv") # Opening and transforming data(opening_floradata.R) ####
  
  flora_raw <- flora_raw %>%
    mutate(across(where(is.character), as.factor))
  
  flora_raw$plot <- factor(flora_raw$plot)
  
  
  flora_raw$sampling <- factor(flora_raw$sampling, levels = sort(unique(flora_raw$sampling)))
  flora_raw$treatment <- factor(flora_raw$treatment, levels =  c("c", "w", "p", "wp") )
  flora_raw$plot <- factor(flora_raw$plot, levels = sort(unique(flora_raw$plot)))
  
  flora_raw <- select(flora_raw, -date, -category, -OBS, -ni.agrupado, -familia, -species_old_1, -species_old_2)
  
  
  # Adding dates
  sampling_dates <- read.csv("data/sampling_dates.csv")
  sampling_dates$sampling <- factor(sampling_dates$sampling)
  
  sampling_dates$date <-  ymd(sampling_dates$date)
  sampling_dates$month <- month(sampling_dates$date, label = TRUE)
  sampling_dates$day <- day(sampling_dates$date)
  sampling_dates$year <- year(sampling_dates$date)
  
  sampling_dates <- sampling_dates %>% 
    select(sampling, date, day, month, year, one_month_window, omw_date)
  
  sampling_dates <- sampling_dates %>%
    mutate(across(where(is.character), as.factor))
  
  flora_raw <- right_join(flora_raw, sampling_dates, by = join_by(sampling))
  
  
  flora_rare <- flora_raw %>%
    select(sampling, one_month_window, omw_date, plot, treatment, code, abundance, height, Cb, Db, Cm, Dm, date, month)
  
  
  
 
  #Adding species information
  species_code <- read.csv("data/species_code.csv")
  species_code <- select(species_code, species, code, family, genus_level, species_level, growing_type)
  species_code <- species_code %>%
    mutate(across(where(is.character), as.factor))
  
  
  # WE WORK WITH IDENTIFIED SPECIES!!
  
  flora_rare <- merge(flora_rare, species_code, by = "code") 
  
# Cambios de los datos morfológicos antes de agrupar la base de datos por especie, muestro y sampling 
 
  
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
  taxongroups <- flora_rare %>%
    filter(code %in% c("poaceae", "asteraceae", "tosp", "orchidaceae"))%>%
    group_by(code, sampling, one_month_window, omw_date, plot, treatment, date, month, species, family, genus_level, species_level) %>%
    summarize(abundance = sum(unique(abundance), na.rm = T), #here we sum abundances 
              height = mean(height, na.rm = T),
              Ah = mean(Ah, na.rm = T),
              Ab = mean(Ab, na.rm = T))
  
  
  species <- anti_join(flora_rare, taxongroups, by = "code") %>%
    group_by(code, sampling, one_month_window, omw_date, plot, treatment, date, month, species, family, genus_level, species_level) %>%
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
  
  
  flora_abrich <- flora_medium
  
  
  flora_medium$code_ordered <- reorder(flora_medium$code, -flora_medium$abundance, FUN = mean)
  
  flora_nobs <- flora_medium %>% 
    group_by(code) %>% 
    summarize(n_observations= n(),
              mean_abundance = mean(abundance))
  
  flora_nobs$abnobs <- flora_nobs$mean_abundance * flora_nobs$n_observations
  
  flora_nobs$code_nobs <- factor(flora_nobs$code,
                                 levels = flora_nobs$code[order(flora_nobs$n_observations, decreasing = TRUE)])
  


## BIOMASS AT SPECIES LEVEL ###########

#For biomass, we do not have information for samplings 0, 1, 2 and 12. 

flora_biomass_raw <- flora_medium %>% 
  filter(!sampling %in% c("0", "1", "2", "12")) %>%  # Remove samplings for which we have no morphological measurements
  filter(!is.na(x)) %>%                              # Remove rows where x is NA since we do not have morph. meas. for those datapoints
  filter(height > 5 & height < 200)               # Remove individuals with height < 5 cm or > 200 cm, because the equation does not properly work with them 


source("code/0.1.number_of_individuals.R")
  
biomass <- flora_biomass_raw
  
biomass <- merge(biomass, species_code)
  
biomass <- left_join(biomass, nind)
  
biomass$year <- year(biomass$date)
  

library(mice)
  
biomass_mice_imputed <- readRDS("data/biomass_mice_imputed.rds")
imputed_db <- complete(biomass_mice_imputed, action = "long")

imputed_db <- imputed_db %>% 
  group_by(plot, treatment, sampling, code) %>% 
  mutate(nind_m2_imputed = round(mean(nind_m2, 0)),
         sd_imputation = sd(nind_m2)) %>% 
  select(plot, treatment, sampling, code, nind_m2_imputed, sd_imputation) %>% 
  distinct()

imputed_db$label_imputation <- ifelse(is.na(biomass$nind_m2), 1, 0)

biomass_imp <- biomass %>% 
  select(year, date, sampling, plot, code, species_level, genus_level, family,
         abundance, height, Ah, Ab, x, biomass_i, richness, abundance_community) %>% 
  left_join(imputed_db)


# Calculation of biomass at species level for both scenarios: with imputation of data with MICE and without it

biomass_imp <- biomass_imp %>% 
  mutate(biomass_s = nind_m2_imputed * biomass_i)

Q1 <- quantile(log(biomass_imp$biomass_s), 0.25, na.rm = TRUE)
Q3 <- quantile(log(biomass_imp$biomass_s), 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

biomass_imp_clean<- biomass_imp %>%
  filter(log(biomass_s) >= Q1 - 1.5 * IQR & log(biomass_s) <= Q3 + 1.5 * IQR)


biomass_noimp <- biomass %>% 
  mutate(biomass_s = nind_m2* biomass_i, na.rm =T)

Q1 <- quantile(log(biomass_noimp$biomass_s), 0.25, na.rm = TRUE)
Q3 <- quantile(log(biomass_noimp$biomass_s), 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

biomass_noimp_clean<- biomass_noimp %>%
  filter(log(biomass_s) >= Q1 - 1.5 * IQR & log(biomass_s) <= Q3 + 1.5 * IQR)


# Can we now fill the gaps in samplings 0, 1 and 2 ? 

source("code/lm_biomass012.R")

print(results)

flora_medium012 <- left_join(flora_medium, lm_data_filtered) %>%
  filter(sampling %in% c("0", "1", "2")) %>% 
  mutate(year = year(date)) %>% 
  mutate(abundance = ifelse(abundance < 1, 1, abundance)) %>% 
  mutate(biomass_s = exp(intercept + slope * log(abundance)) * smearing_factor)

biomass_imp_clean_012 <- bind_rows(biomass_imp_clean, flora_medium012) 

biomass_noimp_clean_012 <- bind_rows(biomass_noimp_clean, flora_medium012)



#Calculation of total biomasss (community biomass) by summing all biomass at species level per square meter
# I will keep the flora_biomass not cleaned in order to compare results with and without outliers

biomass_imp <- biomass_imp_clean_012 %>% 
  group_by(plot, sampling, treatment) %>% 
  mutate(biomass_community =  sum(biomass_s, na.rm = TRUE)) %>%
  ungroup() 

biomass_noimp <- biomass_noimp_clean_012 %>% 
  group_by(plot, sampling, treatment) %>% 
  mutate(biomass_community =  sum(biomass_s, na.rm = TRUE)) %>%
  ungroup()


#Adding dummy rows for p and wp in sampling 1

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



biomass_imp <- bind_rows(biomass_imp, dummy_rows) %>% 
  select(year, date, sampling, treatment, plot, code, abundance,
         richness, biomass_s, biomass_community, abundance_community)

biomass_noimp <- bind_rows(biomass_noimp, dummy_rows) %>% 
  select(year, date, sampling, treatment, plot, code, abundance,
         richness, biomass_s, biomass_community, abundance_community)

flora_abrich <- bind_rows(flora_abrich, dummy_rows)%>% 
  select(year, date, sampling, treatment, plot, code, abundance,
         richness, abundance_community)

rm(list = setdiff(ls(), c("flora_abrich", "biomass_imp", "biomass_noimp")))

