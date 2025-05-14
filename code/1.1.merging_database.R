

rm(list = ls(all.names = TRUE))

# Mergin databases for main results

abrich_db_plot <- read.csv("data/abrich_db_plot.csv")

biomass_db_plot <- read.csv("data/biomass_db_plot.csv")

biomass012_db_plot <- read.csv("data/biomass012_db_plot.csv")

nmds_df_plot <- read.csv("data/nmds_df_plot.csv") %>% 
  select(-NMDS3)

pca_cwm_plot <- read.csv("data/pca_cwm_plot.csv") %>% 
  select(-PC3, -PC4, -PC5, -PC6)

radcoeff_db_plot <- read.csv("data/radcoeff_db_plot.csv") %>% 
  select(-one_month_window, -omw_date, -year)


library(dplyr)

arkaute <- abrich_db_plot %>%
  full_join(biomass_db_plot,     by = c("sampling", "plot", "treatment", "date", "omw_date", "one_month_window")) %>%
  full_join(biomass012_db_plot,  by = c("sampling", "plot", "treatment", "date", "omw_date", "one_month_window")) %>%
  full_join(nmds_df_plot,        by = c("sampling", "plot", "treatment", "date")) %>%
  full_join(pca_cwm_plot,        by = c("sampling", "plot", "treatment", "date")) %>% 
  full_join(radcoeff_db_plot,    by = c("sampling", "plot", "treatment", "date")) %>% 
  select(year, date, omw_date, one_month_window, sampling, plot, treatment, 
         richness, abundance, biomass, biomass012, Y_zipf, NMDS1, NMDS2, PC1, PC2)



na_rows <- arkaute %>%
  filter(is.na(biomass))

arkaute %>%  write.csv("data/arkaute.csv", row.names = F)
