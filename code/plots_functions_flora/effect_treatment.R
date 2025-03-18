


effect_size <- function(data, variable){

mean_variable <- data[[paste0("mean_", variable)]]
sd_variable <- data[[paste0("sd_", variable)]]

mean_variable_name <- paste0("mean_", variable)
sd_variable_name <- paste0("sd_", variable)


effect <- data %>% 
  select(date, sampling, treatment, all_of(mean_variable_name), all_of(sd_variable_name)) %>% 
  distinct()

effect_c <- effect %>% 
  filter(treatment == "c") %>% 
  select(date, sampling, all_of(mean_variable_name), all_of(sd_variable_name)) %>% 
  rename(mean_variable_c = mean_variable, 
         sd_variable_c = sd_variable)

n = 4
lrr_treatment <- lrr %>% 
  filter(!treatment == "c") %>% 
  select(date, sampling, treatment, mean_variable, sd_variable) %>% 
  left_join(lrr_c) %>% 
  mutate(
    lrr = log(mean_variable/mean_variable_c),
    se_lrr = sqrt(
      (1/n) + 
        (1/n) +
        ((sd_variable^2)/(n*mean_variable)) +
        ((sd_variable_c^2)/(n*mean_variable_c)))
  )

ggplot(lrr_treatment, aes(x = date, y = lrr)) + 
  facet_wrap(~ treatment, labeller = labeller(treatment = labels3)) +
  geom_errorbar(aes(ymin = lrr - se_lrr,
                    ymax = lrr + se_lrr,
                    color = treatment), alpha = 0.5) +
  geom_point(aes(color = treatment)) + 
  geom_line(aes(color = treatment)) +
  scale_color_manual(values = palette) +
  geom_hline( yintercept= 0, linetype = "dashed", color = "gray40") +
  labs(x = NULL, y = "Lrr variable") +
  theme(legend.position = "none")



library(effectsize)

hedges_treatment <- lrr %>% 
  filter(treatment != "c") %>% 
  left_join(lrr_c, by = "date") %>%
  mutate(
    pooled_sd = sqrt((((n-1) * sd_variable^2) + ((n-1) * sd_variable_c^2)) / (n+n-2)), # n=4 por grupo → df = 3
    hedges_g = (mean_variable - mean_variable_c) / pooled_sd
  )

ggplot(hedges_treatment, aes(x = date, y = hedges_g)) + 
  facet_wrap(~ treatment, labeller = labeller(treatment = labels3)) +
  geom_errorbar(aes(ymin = hedges_g - pooled_sd,
                    ymax = hedges_g + pooled_sd,
                    color = treatment), alpha = 0.5) +
  geom_point(aes(color = treatment)) + 
  geom_line(aes(color = treatment)) +
  scale_color_manual(values = palette) +
  geom_hline( yintercept= 0, linetype = "dashed", color = "gray40") +
  labs(x = NULL, y = "Hedges'g effect for variable") +
  theme(legend.position = "none")



perchange_treatment <- lrr %>% 
  filter(treatment != "c") %>% 
  left_join(lrr_c, by = c("date", "sampling")) %>%  # Unir con el control
  mutate(
    percent_change = ((mean_variable - mean_variable_c) / mean_variable_c) * 100,
    se_percent_change = (100 / mean_variable_c) * sqrt((sd_variable^2 / n) + (sd_variable_c^2 / n))
  )

ggplot(perchange_treatment, aes(x = date, y = percent_change)) + 
  facet_wrap(~ treatment, labeller = labeller(treatment = labels3)) +
  geom_errorbar(aes(ymin = percent_change - se_percent_change,
                    ymax = percent_change + se_percent_change,
                    color = treatment), alpha = 0.5) +
  geom_point(aes(color = treatment)) + 
  geom_line(aes(color = treatment)) +
  scale_color_manual(values = palette) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(x = NULL, y = "Percenctual change (%)") +
  theme(legend.position = "none")

lrr_w <- lrr %>% 
  filter(treatment == "w")

lrr_p <- lrr %>% 
  filter(treatment == "p")


}
