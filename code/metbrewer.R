



library(MetBrewer)

RR_treatments_noC <- RR_treatments[RR_treatments$treatment != "c", ]

# Changes in the data to plot
 RR_treatments_noC$RR_abundance_C[which(RR_treatments_noC$RR_abundance_C == -Inf)] <- NA
RR_treatments_noC$treatment <- factor(RR_treatments_noC$treatment, levels = c("w", "p", "wp"))

# Calculate mean and standard deviation per sampling and treatment
mean_sd_data <- RR_treatments_noC %>%
  group_by(treatment, sampling) %>%
  summarize(mean_abundance = mean(RR_abundance_C),
            sd_abundance = sd(RR_abundance_C))
mean_sd_data$mean_abundance[which(mean_sd_data$mean_abundance == -Inf)] <- NA
mean_sd_data$sd_abundance[which(is.nan(mean_sd_data$sd_abundance))] <- NA

# Plot with error bars
treatment.labs <- c("Warming", "Perturbation", "Warming and perturbation")
names(treatment.labs) <- c("w", "p", "wp")

ggplot(mean_sd_data, aes(x = as.factor(sampling), y = mean_abundance, group = sampling)) +
  facet_grid(~ treatment, labeller = labeller(treatment = treatment.labs)) +
  
  geom_errorbar(aes(ymin = mean_abundance - sd_abundance, ymax = mean_abundance + sd_abundance,
                    color = treatment),
                position = position_dodge(width = 0.5), width = 0.25, size = 0.75) +
  
  geom_point(data = RR_treatments_noC, aes(x = as.factor(sampling), y = RR_abundance_C, color = treatment),
             position = position_dodge(width = 0.5), size = 2, alpha = 0.4) +
  
  geom_point(aes(color = treatment), fill = "white", position = position_dodge(width = 0.5), size = 2.25, shape = 21) +
  
  geom_line() +
  
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.7) +
  
  geom_vline(xintercept = 1.5, linetype = "dashed", color = "black", size = 0.6) +
  
  scale_colour_manual(values = met.brewer("VanGogh2", 3)) +
  
  labs(x = "Sampling time", y = "Species abundance (log response ratio)") +
  
  theme_bw() + theme(legend.position = "none",
                     panel.grid = element_blank(),
                     strip.background = element_blank(),
                     strip.text = element_text(face = "bold"),
                     text = element_text(size = 15))

#ggsave(plot = last_plot(), path = "Results/Plots", filename = "logRR_abundance.png", device = "png", width = 10, height = 5, dpi = 320)