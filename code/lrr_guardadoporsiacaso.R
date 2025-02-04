


# Loop through each sampling period
for (i in 1:length(samps)) {
  
  
  # Function to safely assign unique values (or NA if empty)
  safe_assign <- function(data, condition, value_column) {
    result <- unique(data[condition, value_column])
    if (length(result) == 0) NA else result
  }
  
  # Mean and SD of richness
  lrr_richness$ref_c_mean[lrr_richness$sampling == samps[i]] <- 
    safe_assign(lrr_richness, lrr_richness$treatment == "c" & lrr_richness$sampling == samps[i], "mean_richness")
  lrr_richness$ref_w_mean[lrr_richness$sampling == samps[i]] <- 
    safe_assign(lrr_richness, lrr_richness$treatment == "w" & lrr_richness$sampling == samps[i], "mean_richness")
  lrr_richness$ref_p_mean[lrr_richness$sampling == samps[i]] <- 
    safe_assign(lrr_richness, lrr_richness$treatment == "p" & lrr_richness$sampling == samps[i], "mean_richness")
  
  lrr_richness$ref_c_sd[lrr_richness$sampling == samps[i]] <- 
    safe_assign(lrr_richness, lrr_richness$treatment == "c" & lrr_richness$sampling == samps[i], "sd_richness")
  lrr_richness$ref_w_sd[lrr_richness$sampling == samps[i]] <- 
    safe_assign(lrr_richness, lrr_richness$treatment == "w" & lrr_richness$sampling == samps[i], "sd_richness")
  lrr_richness$ref_p_sd[lrr_richness$sampling == samps[i]] <- 
    safe_assign(lrr_richness, lrr_richness$treatment == "p" & lrr_richness$sampling == samps[i], "sd_richness")
  
  # Mean and SD of abundance
  lrr_abundance$ref_c_mean[lrr_abundance$sampling == samps[i]] <- 
    safe_assign(lrr_abundance, lrr_abundance$treatment == "c" & lrr_abundance$sampling == samps[i], "mean_abundance")
  lrr_abundance$ref_w_mean[lrr_abundance$sampling == samps[i]] <- 
    safe_assign(lrr_abundance, lrr_abundance$treatment == "w" & lrr_abundance$sampling == samps[i], "mean_abundance")
  lrr_abundance$ref_p_mean[lrr_abundance$sampling == samps[i]] <- 
    safe_assign(lrr_abundance, lrr_abundance$treatment == "p" & lrr_abundance$sampling == samps[i], "mean_abundance")
  
  lrr_abundance$ref_c_sd[lrr_abundance$sampling == samps[i]] <- 
    safe_assign(lrr_abundance, lrr_abundance$treatment == "c" & lrr_abundance$sampling == samps[i], "sd_abundance")
  lrr_abundance$ref_w_sd[lrr_abundance$sampling == samps[i]] <- 
    safe_assign(lrr_abundance, lrr_abundance$treatment == "w" & lrr_abundance$sampling == samps[i], "sd_abundance")
  lrr_abundance$ref_p_sd[lrr_abundance$sampling == samps[i]] <- 
    safe_assign(lrr_abundance, lrr_abundance$treatment == "p" & lrr_abundance$sampling == samps[i], "sd_abundance")
  
  # Mean and SD of Y_zipf
  lrr_Y_zipf$ref_c_mean[lrr_Y_zipf$sampling == samps[i]] <- 
    safe_assign(lrr_Y_zipf, lrr_Y_zipf$treatment == "c" & lrr_Y_zipf$sampling == samps[i], "mean_Y_zipf")
  lrr_Y_zipf$ref_w_mean[lrr_Y_zipf$sampling == samps[i]] <- 
    safe_assign(lrr_Y_zipf, lrr_Y_zipf$treatment == "w" & lrr_Y_zipf$sampling == samps[i], "mean_Y_zipf")
  lrr_Y_zipf$ref_p_mean[lrr_Y_zipf$sampling == samps[i]] <- 
    safe_assign(lrr_Y_zipf, lrr_Y_zipf$treatment == "p" & lrr_Y_zipf$sampling == samps[i], "mean_Y_zipf")
  
  lrr_Y_zipf$ref_c_sd[lrr_Y_zipf$sampling == samps[i]] <- 
    safe_assign(lrr_Y_zipf, lrr_Y_zipf$treatment == "c" & lrr_Y_zipf$sampling == samps[i], "sd_Y_zipf")
  lrr_Y_zipf$ref_w_sd[lrr_Y_zipf$sampling == samps[i]] <- 
    safe_assign(lrr_Y_zipf, lrr_Y_zipf$treatment == "w" & lrr_Y_zipf$sampling == samps[i], "sd_Y_zipf")
  lrr_Y_zipf$ref_p_sd[lrr_Y_zipf$sampling == samps[i]] <- 
    safe_assign(lrr_Y_zipf, lrr_Y_zipf$treatment == "p" & lrr_Y_zipf$sampling == samps[i], "sd_Y_zipf")
  
  # Mean and SD of mu_log
  lrr_mu_log$ref_c_mean[lrr_mu_log$sampling == samps[i]] <- 
    safe_assign(lrr_mu_log, lrr_mu_log$treatment == "c" & lrr_mu_log$sampling == samps[i], "mean_mu_log")
  lrr_mu_log$ref_w_mean[lrr_mu_log$sampling == samps[i]] <- 
    safe_assign(lrr_mu_log, lrr_mu_log$treatment == "w" & lrr_mu_log$sampling == samps[i], "mean_mu_log")
  lrr_mu_log$ref_p_mean[lrr_mu_log$sampling == samps[i]] <- 
    safe_assign(lrr_mu_log, lrr_mu_log$treatment == "p" & lrr_mu_log$sampling == samps[i], "mean_mu_log")
  
  lrr_mu_log$ref_c_sd[lrr_mu_log$sampling == samps[i]] <- 
    safe_assign(lrr_mu_log, lrr_mu_log$treatment == "c" & lrr_mu_log$sampling == samps[i], "sd_mu_log")
  lrr_mu_log$ref_w_sd[lrr_mu_log$sampling == samps[i]] <- 
    safe_assign(lrr_mu_log, lrr_mu_log$treatment == "w" & lrr_mu_log$sampling == samps[i], "sd_mu_log")
  lrr_mu_log$ref_p_sd[lrr_mu_log$sampling == samps[i]] <- 
    safe_assign(lrr_mu_log, lrr_mu_log$treatment == "p" & lrr_mu_log$sampling == samps[i], "sd_mu_log")
  
  # Mean and SD of sigma_log
  lrr_sigma_log$ref_c_mean[lrr_sigma_log$sampling == samps[i]] <- 
    safe_assign(lrr_sigma_log, lrr_sigma_log$treatment == "c" & lrr_sigma_log$sampling == samps[i], "mean_sigma_log")
  lrr_sigma_log$ref_w_mean[lrr_sigma_log$sampling == samps[i]] <- 
    safe_assign(lrr_sigma_log, lrr_sigma_log$treatment == "w" & lrr_sigma_log$sampling == samps[i], "mean_sigma_log")
  lrr_sigma_log$ref_p_mean[lrr_sigma_log$sampling == samps[i]] <- 
    safe_assign(lrr_sigma_log, lrr_sigma_log$treatment == "p" & lrr_sigma_log$sampling == samps[i], "mean_sigma_log")
  
  lrr_sigma_log$ref_c_sd[lrr_sigma_log$sampling == samps[i]] <- 
    safe_assign(lrr_sigma_log, lrr_sigma_log$treatment == "c" & lrr_sigma_log$sampling == samps[i], "sd_sigma_log")
  lrr_sigma_log$ref_w_sd[lrr_sigma_log$sampling == samps[i]] <- 
    safe_assign(lrr_sigma_log, lrr_sigma_log$treatment == "w" & lrr_sigma_log$sampling == samps[i], "sd_sigma_log")
  lrr_sigma_log$ref_p_sd[lrr_sigma_log$sampling == samps[i]] <- 
    safe_assign(lrr_sigma_log, lrr_sigma_log$treatment == "p" & lrr_sigma_log$sampling == samps[i], "sd_sigma_log")
}

