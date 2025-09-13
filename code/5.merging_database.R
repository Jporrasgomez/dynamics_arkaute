


rm(list = ls(all.names = TRUE))
pacman::p_load(dplyr, tidyverse, lubridate, ggplot2)


# Mergin databases for main results

abundance_richness <- read.csv("data/abrich_db_plot.csv")

biomass <- read.csv("data/biomass_db_plot.csv")

biomass_lm_sp <- read.csv("data/biomass012_db_plot.csv")

nmds_spcomp <- read.csv("data/nmds_df_plot.csv") %>% 
  select(-NMDS3)

PCA_ft <- read.csv("data/pca_cwm_plot.csv") %>% 
  select(-PC3)

evenness <- read.csv("data/radcoeff_db_plot.csv") %>% 
  select(-one_month_window, -omw_date, -year)

temp_vwc_data <- read.csv("data/temp_vwc_data.csv")

cwm_LES <-  read.csv("data/cwm_plot_db.csv") %>%  select(-X, -year)

biomass_lm_plot <- read.csv("data/biomass_lm_plot.csv")


library(dplyr)

arkaute <- abundance_richness %>%
  full_join(biomass,     by = c("sampling", "plot", "treatment", "date", "omw_date", "one_month_window")) %>%
  full_join(biomass_lm_sp,  by = c("sampling", "plot", "treatment", "date", "omw_date", "one_month_window")) %>%
  full_join(nmds_spcomp,        by = c("sampling", "plot", "treatment", "date")) %>%
  full_join(PCA_ft,        by = c("sampling", "plot", "treatment", "date")) %>% 
  full_join(evenness,    by = c("sampling", "plot", "treatment", "date")) %>% 
  full_join(temp_vwc_data,       by = c("plot", "treatment", "date")) %>%
  full_join(cwm_LES,        by = c("sampling", "plot", "treatment", "date")) %>% 
  full_join(biomass_lm_plot,     by = c("sampling", "plot", "treatment")) %>% 
  select(year, date, omw_date, one_month_window, sampling, plot,
         treatment, richness, abundance, biomass, biomass012, biomass_lm_plot, Y_zipf, 
         NMDS1, NMDS2,PC1, PC2, SLA, LDMC, leafN, mean_temperature, mean_vwc)



arkaute <- arkaute %>% 
  mutate(OTC = ifelse(treatment %in% c("w", "wp"), paste0("YES"), paste0("NO"))) %>% 
  mutate(perturbation = ifelse(treatment %in% c("p", "wp"), paste0("YES"), paste0("NO"))) %>% 
  select(colnames(arkaute)[1:7], "OTC", "perturbation", colnames(arkaute[8:21]))


na_rows <- arkaute %>%
  filter(is.na(biomass))

arkaute %>%  write.csv("data/arkaute.csv", row.names = F)


arkaute %>% 
  ggplot(aes(x = sampling, y = biomass012, color = treatment)) + 
  geom_point() + 
  geom_path(group = plot)


