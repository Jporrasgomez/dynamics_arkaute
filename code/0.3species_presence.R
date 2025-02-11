


species_presence_treat <- flora_raw %>%
  distinct(treatment, sampling, plot, code) %>%
  count(treatment, code, name = "n_obs_xtreat") %>%
  group_by(code) %>%
  mutate(total_obs = sum(n_obs_xtreat),
         perc_obs = round(n_obs_xtreat / total_obs, 2))

flora_nobs_presence <- merge(flora_nobs, species_presence_treat, by = "code") %>% 
  select(treatment, code, n_obs_xtreat, total_obs, perc_obs) 

# Ensure all (treatment, code) combinations exist
flora_nobs_presence <- expand_grid(
  treatment = unique(flora_nobs_presence$treatment),
  code = unique(flora_nobs_presence$code)) %>%
  left_join(flora_nobs_presence, by = c("treatment", "code"))

flora_nobs_presence <- merge(flora_nobs_presence, species_code, by = "code")

# Convert perc_obs to a factor to handle missing values separately
flora_nobs_presence <- flora_nobs_presence %>%
  mutate(perc_obs = ifelse(is.na(perc_obs), 0 , perc_obs), 
         n_obs_xtreat = ifelse(is.na(n_obs_xtreat), 0, n_obs_xtreat),
         total_obs = ifelse(is.na(total_obs), 0, total_obs))

flora_nobs_presence <- flora_nobs_presence %>%
  mutate(code = reorder(code, total_obs))



ggpresence <- 
  ggplot(flora_nobs_presence, aes(x = treatment, y = code, fill = n_obs_xtreat)) +
  geom_tile(color = "gray13") +  # Keep black grid lines
  geom_text(aes(label = n_obs_xtreat), size = 2.3) +  # Add text labels
  scale_fill_gradientn(
    colors = c("white", "#FFF5E1", "orange2"),  # Very pale gray (#F0F0F0), white, orange
    values = scales::rescale(c(0, 1, max(flora_nobs_presence$n_obs_xtreat, na.rm = TRUE))),
    name = "Observations"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove default grid lines
    axis.text.y = element_text(face = "italic"),  # Italicize species names
    legend.position = "bottom"  # Move legend to the bottom
  ) +
  labs(x = "Treatment", y = "Species")


ggpresence0 <- 
  ggplot(flora_nobs_presence, aes(x = treatment, y = code, fill = n_obs_xtreat)) +
  geom_tile(color = "gray13") +  # Líneas de cuadrícula en gris
  geom_text(aes(label = n_obs_xtreat), size = 2.3) +  # Etiquetas con el número de observaciones
  scale_fill_gradientn(
    colors = c("white", "#FFF5E1", "orange2"),  # Gradiente de blanco a naranja
    values = scales::rescale(c(0, 1, max(flora_nobs_presence$n_obs_xtreat, na.rm = TRUE))),
    name = "Observations"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Eliminar líneas de cuadrícula predeterminadas
    axis.text.y = element_text(face = "italic", 
                               color = ifelse(levels(flora_nobs_presence$code) %in% 
                                                excluded_species_lm, "red", "black")),  # Cambiar color para "Poaceae"
    legend.position = "bottom"  # Colocar la leyenda en la parte inferior
  ) +
  labs(x = "Treatment", y = "Species", title = paste0("p-value < 0.1 and R2 > 0.3: "))


ggpresence1 <- 
  ggplot(flora_nobs_presence, aes(x = treatment, y = code, fill = n_obs_xtreat)) +
  geom_tile(color = "gray13") +  # Líneas de cuadrícula en gris
  geom_text(aes(label = n_obs_xtreat), size = 2.3) +  # Etiquetas con el número de observaciones
  scale_fill_gradientn(
    colors = c("white", "#FFF5E1", "orange2"),  # Gradiente de blanco a naranja
    values = scales::rescale(c(0, 1, max(flora_nobs_presence$n_obs_xtreat, na.rm = TRUE))),
    name = "Observations"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Eliminar líneas de cuadrícula predeterminadas
    axis.text.y = element_text(face = "italic", 
                               color = ifelse(levels(flora_nobs_presence$code) %in% 
                                                excluded_species_lm_1, "red", "black")),  # Cambiar color para "Poaceae"
    legend.position = "bottom"  # Colocar la leyenda en la parte inferior
  ) +
  labs(x = "Treatment", y = NULL, title = paste0("p-value < 0.1"))


ggpresence2 <- 
  ggplot(flora_nobs_presence, aes(x = treatment, y = code, fill = n_obs_xtreat)) +
  geom_tile(color = "gray13") +  # Líneas de cuadrícula en gris
  geom_text(aes(label = n_obs_xtreat), size = 2.3) +  # Etiquetas con el número de observaciones
  scale_fill_gradientn(
    colors = c("white", "#FFF5E1", "orange2"),  # Gradiente de blanco a naranja
    values = scales::rescale(c(0, 1, max(flora_nobs_presence$n_obs_xtreat, na.rm = TRUE))),
    name = "Observations"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Eliminar líneas de cuadrícula predeterminadas
    axis.text.y = element_text(face = "italic", 
                               color = ifelse(levels(flora_nobs_presence$code) %in% 
                                                excluded_species_lm_2, "red", "black")),  # Cambiar color para "Poaceae"
    legend.position = "bottom"  # Colocar la leyenda en la parte inferior
  ) +
  labs(x = "Treatment", y = NULL, title = paste0("p-value < 0.05"))



