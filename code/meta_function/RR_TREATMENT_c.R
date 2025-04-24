


RR_treatment_c <- function(data, variable){


data <- data %>% 
  distinct(treatment, plot, sampling, date, .data[[variable]], .keep_all = TRUE) %>% 
  group_by(treatment) %>% 
  mutate(
    n = n(),
    mean := mean(.data[[variable]], na.rm = TRUE),
    sd := sd(.data[[variable]], na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  select(treatment, sampling, date, plot, n,
         !!variable, mean, sd) %>% 
  mutate( variable = variable)


effect <- data %>% 
  select(treatment, n, mean, sd) %>% 
  distinct()

effect_c <- effect %>% 
  filter(treatment == "c") %>% 
  select(treatment, n, mean, sd) %>%  # Mantener treatment
  rename(mean_c = mean,
         sd_c = sd,
         n_c = n)


ytitle_dict <- list(
  "richness"   = "Richness",
  "abundance"  = "Community cover",
  "sigma_log"  = "Sigma (Coefficient 2 in Log model for RADs)",
  "mu_log"     = "Mu (Coefficient 1 in Log model for RADs)",
  "Y_zipf"     = "Gamma (Coefficient in Zipf model for RADs)",
  "biomass"    = "Community biomass",
  "NMDS1" = "NMDS1",
  "NMDS2" = "NMDS2",
  "NMDS3" = "NMDS3",
  "total_turnover" = "Total turnover", 
  "appearance" = "Turnover: appearance", 
  "disappearance" = "Turnover: disappearance"
)

ytitle <- ytitle_dict[[variable]]

if (is.null(ytitle)) {
  stop("Variable must be one of the following: ", paste(names(ytitle_dict), collapse = ", "))
}


RR_treatment <- effect %>% 
  filter(treatment != "c") %>% 
  mutate(
    mean_c = effect_c$mean_c,
    sd_c = effect_c$sd_c,
    n_c = effect_c$n_c
  ) %>% 
  mutate(
    # Cálculo del Log Response Ratio (RR)
    RR =  log(mean / mean_c),
    
    # Cálculo de la varianza de RR
    se_RR =  sqrt((sd^2) / (n * mean^2) + 
               (sd_c^2) / (n_c * mean_c^2))
  ) %>% 
  
  mutate(
    variable = variable
  ) %>% 
  rename(RR_descriptor = treatment) %>% 
  mutate(
    RR_descriptor = fct_recode(RR_descriptor,
                               "w_vs_c" = "w",
                               "p_vs_c" = "p", 
                               "wp_vs_c" = "wp"))


RR_treatment <<- RR_treatment


z = 1.96


gg_RR<-
  ggplot(RR_treatment, aes(x = RR_descriptor, y = RR)) + 
  geom_errorbar(aes(ymin = RR - z * se_RR,
                    ymax = RR + z * se_RR,
                    color = RR_descriptor), linewidth = 0.5) +
  geom_point(aes(color = RR_descriptor)) + 
  scale_color_manual(values = palette_RR) +
  scale_x_discrete(labels = labels_RR) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(x = NULL, y = paste0("RR ", ytitle)) +
  theme(legend.position = "none")

gg_RR <<- gg_RR



}
