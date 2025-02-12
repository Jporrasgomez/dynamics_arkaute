




flora_01212 <- flora_abrich %>% 
  filter(sampling %in% c(0, 1, 2, 12)) %>% 
  filter(!code %in% c(excluded_species_lm_2, neg_slope_species))

flora_01212$biomass_s <- NA

# First, let's check if all species found within 0, 1, 2 and 12 are available in the flora_biomass database, 
# which is the one we will  use for building the Linar model


setdiff(unique(flora_01212$code), unique(flora_biomass$code))
#There are no species within 0,1, 2 and 12 that are not available in flora_biomass

biomass_code <- unique(flora_biomass$code)
treats

ab_lm <- matrix(nrow = length(biomass_code), ncol = 6)
colnames(ab_lm) <- c("code", "intercept", "slope", "r_squared", "p_value", "n_points_lm")
ab_lm <- as.data.frame(ab_lm)

ablist <- list()


library(broom)

counter <- 0 

for (i in 1: length(biomass_code)){
  
  ab_lm_i <- subset(ab_lm, code == code_biomass[i] & treatment == "c")
  lm_i <- lm(biomass_community ~ abundance, data = flora_biomass)
  
  # Extract coefficients, R^2, and p-value
  lm_i_summary <- summary(lm_i)
  lm_i_tidy <- tidy(lm_i)
  lm_i_glance <- glance(lm_i)
  
  intercept_i <- lm_i_tidy$estimate[1]
  slope_i <- lm_i_tidy$estimate[2]
  r_squared_i <- lm_i_glance$r.squared
  p_value_i <- lm_i_tidy$p.value[2]
  n_observations_i <- nrow(ab_lm_i)
  
  counter <- counter + 1
  
  gglist[[counter]] <- ggplot(ab_lm_i, aes(x = abundance, y = ab_lm_m2)) +
    geom_point(aes(color = treatment), alpha = 0.5) +
    scale_colour_manual(values = c("c" = "green2", "w" = "red", "p" = "blue3", "wp" = "purple")) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    labs(title = paste("LM ", code_levels[i]),
         subtitle = paste("Equation: y =", round(intercept_i, 2), "+", round(slope_i, 2), "* x\n",
                          "R2:", round(r_squared_i, 2), ", p-value:", round(p_value_i, 4), "\n",
                          "n observations", n_observations_i),
         x = "Abundance",
         y = "Biomass") +
    theme_minimal()
  
  ab_lm_lm_data$code[counter] <- code_levels[i]
  ab_lm_lm_data$intercept[counter] <- intercept_i
  ab_lm_lm_data$slope[counter] <- slope_i
  ab_lm_lm_data$r_squared[counter] <- r_squared_i
  ab_lm_lm_data$p_value[counter] <- p_value_i
  ab_lm_lm_data$n_points_lm[counter] <- n_observations_i
  
  
}




results_list <- vector("list", length(treats))

# Iterate through each treatment (loop over the treatments in `treats` list)
for (j in seq_along(treats)) {
  
  # Create an empty list to store results for each code level within the current treatment
  # This temporary list will hold results for each code level for the current treatment
  temp_list <- vector("list", length(code_levels))  # Temporary storage
  
  # Iterate through each code level (loop over the levels of `code_levels` list)
  for (i in seq_along(code_levels)) {
    
    # Subset the data: filter the dataset `nind` for the current `code_levels[i]` and `treats[j]`
    # This creates a subset of the data where both the `code` and `treatment` match the current loop values
    nind_i <- subset(nind, code == code_levels[i] & treatment == treats[j])
    
    # Check if the subset is empty (i.e., no rows match the condition)
    # If the subset is empty, skip to the next iteration of the loop
    if (nrow(nind_i) == 0) next 
    
    # Fit a linear model using `lm()`:
    # Predict `nind_m2` from `abundance` for the subset of data `nind_i`
    lm_i <- lm(nind_m2 ~ abundance, data = nind_i)
    
    # Use `broom::tidy()` to extract the coefficients (intercept, slope, etc.) from the linear model
    lm_i_tidy <- broom::tidy(lm_i)
    
    # Use `broom::glance()` to extract the model's summary statistics (e.g., R-squared)
    lm_i_glance <- broom::glance(lm_i)
    
    # Create a data frame to store the model results and related information
    # We store the following:
    
    temp_list[[i]] <- data.frame(
      code = code_levels[i],
      intercept = lm_i_tidy$estimate[1],
      slope = lm_i_tidy$estimate[2],
      r_squared = lm_i_glance$r.squared,
      p_value = lm_i_glance$p.value,
      n_observations = nrow(nind_i),
      treatment = treats[j]
    ) %>% 
      distinct()  # Ensures one row per species
  }
  
  # Bind rows and store in the list
  results_list[[j]] <- do.call(rbind, temp_list)
}