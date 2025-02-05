

treats <- unique(nind$treatment)

# Initialize an empty list for storing results
# This will hold the results of each treatment (one for each treatment in the `treats` list)
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


nind_lm_data_c <- results_list[[1]]
nind_lm_data_p <- results_list[[2]]
nind_lm_data_w <- results_list[[3]]
nind_lm_data_wp <- results_list[[4]]

# As we can see, for this example the R$^2$ is quite low ( R$^2$ = 0.23). The fact is that, if we take a look to how well
# the model fit for all of our species, there are many considerations.

nind_lm_data_c$posneg_slope <- ifelse(nind_lm_data_c$slope < 0, paste("negative"), paste("positive"))
nind_lm_data_w$posneg_slope <- ifelse(nind_lm_data_w$slope < 0, paste("negative"), paste("positive"))
nind_lm_data_p$posneg_slope <- ifelse(nind_lm_data_p$slope < 0, paste("negative"), paste("positive"))
nind_lm_data_wp$posneg_slope <- ifelse(nind_lm_data_wp$slope < 0, paste("negative"), paste("positive"))
#hist(nind_lm_data$slope)

nind_lm_data_treatments <- bind_rows(nind_lm_data_c, nind_lm_data_w, nind_lm_data_p, nind_lm_data_wp)

#Let's check results
treatment_labs_dynamics <- c("Control", "Warming", "Perturbation", "Warming and perturbation")
names(treatment_labs_dynamics) <- c("c","w", "p", "wp")

results_treatments <- 
  ggplot(nind_lm_data_treatments, aes( x = r_squared, y = p_value, 
                            label = paste(code, n_observations, sep = ", "), color = posneg_slope))+
  facet_wrap(~ treatment, labeller = labeller(treatment = treatment_labs_dynamics, nrow = 2, ncol = 2)) + 
  geom_point(aes(size = n_observations))+ 
  geom_hline(yintercept = 0.1, linetype = "dashed", color = "gray40") +
  
  geom_text_repel(
    size = 3,                # Text size
    min.segment.length = 0.1,  # Ensures lines are always drawn
    segment.color = "gray50",# Line color
    segment.size = 0.5, 
    max.overlaps = 15# Line thickness
  )



species_lm_c <- nind_lm_data_c %>% 
  filter(p_value < 0.1) %>% 
  select(code)
species_lm_w <- nind_lm_data_w %>% 
  filter(p_value < 0.1)%>% 
  select(code)
species_lm_p <- nind_lm_data_p %>% 
  filter(p_value < 0.1)%>% 
  select(code)
species_lm_wp <- nind_lm_data_wp %>% 
  filter(p_value < 0.1)%>% 
  select(code)

# Extract unique 'code' values from each dataset
codes_c <- unique(species_lm_c$code)
codes_w <- unique(species_lm_w$code)
codes_p <- unique(species_lm_p$code)
codes_wp <- unique(species_lm_wp$code)

# Find the shared 'code' values across all four datasets
shared_codes <- Reduce(intersect, list(codes_c, codes_w, codes_p, codes_wp))

# Print the result
shared_codes

