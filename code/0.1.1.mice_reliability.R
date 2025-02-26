


# Checking reliability of mice imputation 
# Let's recreate
perc_NA <- ((biomass_nolm %>% 
  filter(is.na(nind_m2)) %>% 
  nrow())) / nrow(biomass_nolm)

nind_nona <- biomass_nolm %>% 
  filter(!is.na(nind_m2))

mice_check <- nind_nona %>%
  mutate(nind_m2 = ifelse(runif(n()) < perc_NA, NA, nind_m2)) #Artificially creating the same percentage of NA as
# there are in my original database

mice_check %>% 
  filter(is.na(nind_m2)) %>% 
  nrow()



check_subset <- mice_check %>% 
  filter(is.na(nind_m2)) %>% 
  select(sampling, plot, treatment, code, abundance, abundance_community) %>%  # Keeping only necessary identifiers
  left_join(nind_nona %>% select(sampling, plot, treatment, code, abundance, abundance_community, nind_m2), 
            by = c("sampling", "plot", "treatment", "code", "abundance", "abundance_community")) %>% 
  rename(nind_m2_original = nind_m2)




