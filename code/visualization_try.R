

theme_set(theme_bw() +
            theme(axis.title.x = element_blank(),
                  legend.position = "NULL",
                  panel.grid = element_blank(),
                  strip.background = element_blank(),
                  strip.text = element_text(face = "bold"),
                  text = element_text(size = 11)))


treatment_labs_dynamics <- c("Control", "Warming", "Perturbation", "Warming and perturbation")
names(treatment_labs_dynamics) <- c("c","w", "p", "wp")



ggplot(mean_sd_abrich_dynamics, aes(x = as.factor(sampling),
                                    y = mean_richness,
                                    group = sampling)) +
  
  facet_grid(~ treatment, labeller = labeller(treatment = treatment_labs_dynamics)) +  
  
  geom_errorbar(aes(ymin = mean_sd_abrich_dynamics$mean_richness- mean_sd_abrich_dynamics$sd_richness,
                    ymax = mean_sd_abrich_dynamics$mean_richness + mean_sd_abrich_dynamics$sd_richness,
                    color = treatment, alpha = 0.5),
                position = position_dodge(width = 0.5), width = 0.25, size = 0.75, alpha = 0.4) +
  
  geom_point(data = ab_rich_dynamics, aes(x = as.factor(sampling),
                                          y = richness,
                                          color = treatment),
             position = position_dodge(width = 0.5), size = 1.5, alpha = 0.2) +
  
  geom_path(group = 1, aes(color = treatment), linewidth = 0.7) +
  
  geom_point(aes(color = treatment), fill = "white", position = position_dodge(width = 0.5), size = 2.25, shape = 21) +
  
  #scale_colour_manual(values = met.brewer("VanGogh2", 4)) +
  
  scale_colour_manual(values = c("c" = "green4", "p" = "blue4", "w" = "red3", "wp" = "purple2")) +
  
  scale_x_discrete(breaks = levels(as.factor(mean_sd_abrich_dynamics$sampling))[seq(1, length(levels(as.factor(mean_sd_abrich_dynamics$sampling))), by = 2)])





ggplot(mean_sd_abrich_dynamics, aes(x = as.factor(sampling_date),
                                    y = mean_richness,
                                    group = sampling_date)) +
  
  facet_grid(~ treatment, labeller = labeller(treatment = treatment_labs_dynamics)) +  
  
  geom_errorbar(aes(ymin = mean_sd_abrich_dynamics$mean_richness- mean_sd_abrich_dynamics$sd_richness,
                    ymax = mean_sd_abrich_dynamics$mean_richness + mean_sd_abrich_dynamics$sd_richness,
                    color = treatment, alpha = 0.5),
                position = position_dodge(width = 0.5), width = 0.25, size = 0.75, alpha = 0.4) +
  
  geom_point(data = ab_rich_dynamics, aes(x = as.factor(sampling_date),
                                          y = richness,
                                          color = treatment),
             position = position_dodge(width = 0.5), size = 1.5, alpha = 0.2) +
  
  geom_path(group = 1, aes(color = treatment), linewidth = 0.9) +
  
  geom_point(aes(color = treatment), fill = "white", position = position_dodge(width = 0.5), size = 2.25, shape = 21) +
  
  #scale_colour_manual(values = met.brewer("VanGogh2", 4)) +
  
  scale_colour_manual(values = c("c" = "#5C8D8B", "w" = "#F25F5C", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +

  geom_vline(xintercept = 1.5, linetype = "dashed", color = "gray40") +
  
  
  scale_x_discrete(breaks = levels(as.factor(mean_sd_abrich_dynamics$sampling_date))[seq(1, length(levels(as.factor(mean_sd_abrich_dynamics$sampling_date))), by = 2)]) +
    
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
