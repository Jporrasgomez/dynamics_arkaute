



# Can we now fill the gaps in samplings 0, 1 and 2 ? 



codes_123 <- flora_medium %>% 
  filter(sampling %in% c("0", "1", "2"))


code_levels <- droplevels(codes_123$code)  # Drop unused levels first
code_levels <- levels(code_levels)       # Extract only the actual levels
length(code_levels)  # Should correctly match the number of unique codes

gglist <- list()

lm_data <- matrix(nrow = length(code_levels), ncol = 8)
colnames(lm_data) <- c("code", "intercept", "slope", "r_squared", "p_value", "n_points_lm", "shapiro_pvalue", "smearing_factor")
lm_data <- as.data.frame(lm_data)

counter <- 0

library(broom)

for (i in 1:length(code_levels)) {
  
  code_i <- subset(biomass_imp, code == code_levels[i])
  
  if (nrow(code_i) < 2) next  # Skip if not enough data
  
  lm_i <- lm(log(biomass_s) ~ log(abundance), data = code_i)
  shapiro_i <- shapiro.test(residuals(lm_i))
  
  # Extract coefficients, R^2, and p-value
  lm_i_summary <- summary(lm_i)
  lm_i_tidy <- tidy(lm_i)
  lm_i_glance <- glance(lm_i)
  
  intercept_i <- lm_i_tidy$estimate[1]
  slope_i <- lm_i_tidy$estimate[2]
  r_squared_i <- lm_i_glance$r.squared
  p_value_i <- lm_i_tidy$p.value[2]
  n_observations_i <- nrow(code_i)
  smearing_factor_i <- mean(exp(residuals(lm_i)))  # When log-transformed a variable, a retransformation has to be done with a smearing factor
  
  counter <- counter + 1
  
  # Generate plot
  gglist[[counter]] <- ggplot(code_i, aes(x = abundance, y = biomass_s)) +
    geom_point(aes(color = treatment), alpha = 0.5) +
    scale_colour_manual(values = c("c" = "green2", "w" = "red", "p" = "blue3", "wp" = "purple")) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    labs(title = paste("LM ", code_levels[i]),
         subtitle = paste("Equation: y =", round(intercept_i, 2), "+", round(slope_i, 2), "* x\n",
                          "R2:", round(r_squared_i, 2), ", p-value:", round(p_value_i, 4), "\n",
                          "n observations:", n_observations_i),
         x = "Abundance",
         y = "Biomass at species level") +
    theme_minimal()
  
  # Store results in lm_data dataframe
  lm_data$code[counter] <- code_levels[i]
  lm_data$intercept[counter] <- intercept_i
  lm_data$slope[counter] <- slope_i
  lm_data$r_squared[counter] <- r_squared_i
  lm_data$p_value[counter] <- p_value_i
  lm_data$n_points_lm[counter] <- n_observations_i
  lm_data$shapiro_pvalue[counter] <- shapiro_i$p.value
  lm_data$smearing_factor[counter] <- smearing_factor_i  # Store correctly
  
}


#
#ggarrange(gglist[[1]], gglist[[2]], gglist[[3]], 
#          gglist[[4]], gglist[[5]], gglist[[6]], 
#          ncol = 2, nrow = 3)
#
#ggarrange(gglist[[7]], gglist[[8]], gglist[[9]], 
#          gglist[[10]], gglist[[11]], gglist[[12]], 
#          ncol = 2, nrow = 3)
#
#ggarrange(gglist[[13]], gglist[[14]], gglist[[15]], 
#          gglist[[16]], gglist[[17]], gglist[[18]], 
#          ncol = 2, nrow = 3)
#
#ggarrange(gglist[[19]], gglist[[20]], gglist[[21]], 
#          gglist[[22]], gglist[[23]], gglist[[24]], 
#          ncol = 2, nrow = 3)
#
#ggarrange(gglist[[25]], gglist[[26]], gglist[[27]], 
#          gglist[[28]], gglist[[29]], gglist[[30]], 
#          ncol = 2, nrow = 3)
#

lm_data$posneg_slope <- ifelse(lm_data$slope < 0, paste("negative"), paste("positive"))
#hist(nind_lm_data$slope)

#Let's check results
results <- 
  ggplot(lm_data, aes( x = r_squared, y = p_value, 
                       label = paste(code, n_points_lm, sep = ", "),
                       color = posneg_slope))+
  geom_point()+ 
  geom_vline(xintercept = 0.3, linetype = "dashed", color = "gray40") +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "gray40") +
  
  geom_text_repel(
    size = 3,                # Text size
    min.segment.length = 0.1,  # Ensures lines are always drawn
    segment.color = "gray50",# Line color
    segment.size = 0.5, 
    max.overlaps = 15# Line thickness
  )


#print(results)

lm_data_filtered <- lm_data %>% 
  filter(p_value < 0.05)
