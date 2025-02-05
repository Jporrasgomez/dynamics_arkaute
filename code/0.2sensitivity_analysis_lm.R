


species_lm <- nind_lm_data %>% 
  filter(r_squared > 0.3) %>% 
  filter(p_value < 0.1)
species_lm_codes <- unique(species_lm$code)
nind_lm_species <- nind_lm_data %>% 
  filter(code %in% species_lm_codes )
excluded_species_lm <- {nind_lm_data %>% 
    filter(!code %in% species_lm_codes )}$code
perc_ex <- round(100 - (length(species_lm_codes)+ length(one_ind_species))/(length(unique(flora_raw$code))) *100, 2)
flora_nobs$excluded <- ifelse(flora_nobs$code %in% excluded_species_lm, "excluded", "included")

# Update the plot
a <- ggplot(flora_nobs, aes(x = mean_abundance, y = n_observations, label = code)) +
  geom_point(aes(color = excluded)) +  # Color based on the exclusion status
  scale_color_manual(
    values = c("included" = "black", "excluded" = "red")  # Specify colors
  ) +
  geom_text_repel(
    size = 3,                # Text size
    min.segment.length = 0.1,  # Ensures lines are always drawn
    segment.color = "gray50",  # Line color
    segment.size = 0.5         # Line thickness
  ) +
  labs(color = "LM inclusion", x = NULL, y = "Number of observations",
       title = paste0("p-value < 0.1 and R2 > 0.3: "), subtitle = paste0(perc_ex, "%", " excluded species")) +
  theme(legend.position = "none")


species_lm_1<- nind_lm_data %>% 
  filter(p_value < 0.1)
species_lm_codes_1 <- unique(species_lm_1$code)
nind_lm_species_1 <- nind_lm_data %>% 
  filter(code %in% species_lm_codes_1 )
excluded_species_lm_1 <- {nind_lm_data %>% 
    filter(!code %in% species_lm_codes_1 )}$code
perc_ex_1 <- round(100 - (length(species_lm_codes_1)+ length(one_ind_species))/(length(unique(flora_raw$code))) *100, 2)
flora_nobs$excluded_1 <- ifelse(flora_nobs$code %in% excluded_species_lm_1, "excluded", "included")


# Update the plot
b <- ggplot(flora_nobs, aes(x = mean_abundance, y = n_observations, label = code)) +
  geom_point(aes(color = excluded_1)) +  # Color based on the exclusion status
  scale_color_manual(
    values = c("included" = "black", "excluded" = "red")  # Specify colors
  ) +
  geom_text_repel(
    size = 3,                # Text size
    min.segment.length = 0.1,  # Ensures lines are always drawn
    segment.color = "gray50",  # Line color
    segment.size = 0.5         # Line thickness
  )+
  labs(color = "LM inclusion", y = NULL, x = NULL,
       title = paste0("p-value < 0.1"), subtitle = paste0(perc_ex_1, "%", " excluded species")) +
  theme(legend.position = "none")



species_lm_2<- nind_lm_data %>% 
  filter(p_value < 0.05)
species_lm_codes_2 <- unique(species_lm_2$code)
nind_lm_species_2 <- nind_lm_data %>% 
  filter(code %in% species_lm_codes_2 )
excluded_species_lm_2 <- {nind_lm_data %>% 
    filter(!code %in% species_lm_codes_2 )}$code
perc_ex_2 <- round(100 - (length(species_lm_codes_2)+ length(one_ind_species))/(length(unique(flora_raw$code))) *100, 2)
flora_nobs$excluded_2 <- ifelse(flora_nobs$code %in% excluded_species_lm_2, "excluded", "included")


c <- ggplot(flora_nobs, aes(x = mean_abundance, y = n_observations, label = code)) +
  geom_point(aes(color = excluded_2)) +  # Color based on the exclusion status
  scale_color_manual(
    values = c("included" = "black", "excluded" = "red")  # Specify colors
  ) +
  geom_text_repel(
    size = 3,                # Text size
    min.segment.length = 0.1,  # Ensures lines are always drawn
    segment.color = "gray50",  # Line color
    segment.size = 0.5         # Line thickness
  ) +
  labs(color = "Linear model inclusion", y = NULL, x = NULL,
       title = paste0("p-value < 0.05"), subtitle = paste0(perc_ex_2, "%", " excluded species")) +
  theme(legend.position = "none")



ggarrange(a,b,c,
  labels = c("A", "B", "C"),
  nrow = 1, 
  ncol = 3)
