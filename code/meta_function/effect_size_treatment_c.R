


effect_size_treatment_c <- function(data, variable){

  

  

mean_variable <- paste0("mean_", variable)
sd_variable <- paste0("sd_", variable)

mean_variable_c <- paste0("mean_", variable, "_c")
sd_variable_c<- paste0("sd_", variable, "_c")



data <- data %>% 
  distinct(treatment, plot, sampling, date, .data[[variable]], .keep_all = TRUE) %>% 
  group_by(treatment) %>% 
  mutate(
    n = n(),
    !!mean_variable := mean(.data[[variable]], na.rm = TRUE),
    !!sd_variable := sd(.data[[variable]], na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  select(treatment, sampling, date, plot, n,
         !!variable, !!mean_variable, !!sd_variable)


effect <- data %>% 
  select(treatment, n, all_of(mean_variable), all_of(sd_variable)) %>% 
  distinct()


effect_c <- effect %>% 
  filter(treatment == "c") %>% 
  select(treatment, n, all_of(mean_variable), all_of(sd_variable)) %>%  # Mantener treatment
  rename(!!mean_variable_c := !!sym(mean_variable),
         !!sd_variable_c := !!sym(sd_variable),
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
    !!mean_variable_c := effect_c[[mean_variable_c]],
    !!sd_variable_c := effect_c[[sd_variable_c]],
    !!sym("n_c") := effect_c[["n_c"]]
  ) %>% 
  mutate(
    # Cálculo del Log Response Ratio (RR)
    RR =  log(.data[[mean_variable]] / .data[[mean_variable_c]]),
    
    # Cálculo de la varianza de RR
    se_RR =  sqrt((.data[[sd_variable]]^2) / (n * .data[[mean_variable]]^2) + 
               (.data[[sd_variable_c]]^2) / (n_c * .data[[mean_variable_c]]^2))
  ) %>% 
  
  mutate(
    variable = variable
  )


RR_treatment <<- RR_treatment


gg_RR<-
  ggplot(RR_treatment, aes(x = treatment, y = RR)) + 
  geom_errorbar(aes(ymin = RR - se_RR,
                    ymax = RR + se_RR,
                    color = treatment), linewidth = 0.5) +
  geom_point(aes(color = treatment)) + 
  scale_color_manual(values = palette) +
  scale_x_discrete(labels = labels) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(x = NULL, y = paste0("RR ", ytitle)) +
  theme(legend.position = "none")

gg_RR <<- gg_RR



}
