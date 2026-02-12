


rm(list = ls(all.names = TRUE))
pacman::p_load(dplyr, tidyverse, lubridate, ggplot2)


# Mergin databases for main results

abundance_richness <- read.csv("data/abrich_db_plot.csv")

nmds_spcomp <- read.csv("data/nmds_df_plot.csv") %>% 
  select(-NMDS3)

evenness <- read.csv("data/radcoeff_db_plot.csv") %>% 
  select(-one_month_window, -omw_date, -year)

cwm_LES <-  read.csv("data/cwm_plot_db.csv") %>%  select(-X, -year)

biomass_just <- read.csv("data/biomass_no_imputation.csv") %>%  rename(biomass_raw = biomass)

biomass_mice <- read.csv("data/biomass_mice_imputation.csv") %>%  rename(biomass_mice = biomass)

temp_vwc_data <- read.csv("data/temp_vwc_data.csv")

biomass_mice_lm <- read.csv("data/biomass_mice_lm.csv")




library(dplyr)

arkaute <- abundance_richness %>%
  full_join(biomass_just,     by = c("sampling", "plot", "treatment", "date", "omw_date", "one_month_window")) %>%
  full_join(biomass_mice,  by = c("sampling", "plot", "treatment", "date", "omw_date", "one_month_window")) %>%
  full_join(nmds_spcomp,        by = c("sampling", "plot", "treatment", "date")) %>%
  full_join(evenness,    by = c("sampling", "plot", "treatment", "date")) %>% 
  full_join(cwm_LES,        by = c("sampling", "plot", "treatment", "date")) %>% 
  full_join(temp_vwc_data,       by = c("plot", "treatment", "date")) %>%
  full_join(biomass_mice_lm,     by = c("sampling", "plot", "treatment")) %>% 
  select(date, omw_date, one_month_window, sampling, plot,
         treatment, richness, abundance, biomass_raw, biomass_mice, biomass_mice_lm, Y_zipf, 
          SLA, LDMC, leafN, mean_temperature, mean_vwc) %>% 
  mutate(year = year(date))


arkaute <- arkaute %>% 
  mutate(OTC = ifelse(treatment %in% c("w", "wp"), paste0("YES"), paste0("NO"))) %>% 
  mutate(perturbation = ifelse(treatment %in% c("p", "wp"), paste0("YES"), paste0("NO"))) 


na_rows <- arkaute %>%
  filter(is.na(biomass_raw))

arkaute %>%  write.csv("data/arkaute.csv", row.names = F)







