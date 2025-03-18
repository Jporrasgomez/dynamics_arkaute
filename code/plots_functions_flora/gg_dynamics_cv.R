

# function to plot recovery dynamics (output is a ggplot object)
gg_dynamics_cv <- function(data, variable) {
  
  # set theme for the plot
  theme_set(theme_bw() +
              theme(axis.title.x = element_blank(),
                    legend.position = "NULL",
                    panel.grid = element_blank(),
                    strip.background = element_blank(),
                    strip.text = element_text(face = "bold"),
                    text = element_text(size = 11)))
  
  # define y axis title
  
  if (variable == "cv_abundance") {
    ytitle <- "CV of abundance"
  } else {
    if (variable == "cv_richness") {
      ytitle <- "CV of richness"
    } else {
      if (variable == "cv_sigma_log") {
        ytitle <- "CV of Sigma for Log model RADs"
      } else {
        if (variable == "cv_mu_log") {
          ytitle <- "CV of Mu for Log model RADs"
        } else {
          if (variable == "cv_Y_zipf") {
            ytitle <- "CV of Gamma for Zipf model RADs"
          } else {
            if (variable == "cv_biomass") {
              ytitle <- "CV of community biomass"
            } else{
              stop("variable must be one of the following: cv_richness, cv_abundance, cv_sigma_log, 
                   cv_mu_log, cv_Y_zipf, cv_biomass_total")
            }
          }
        }
      }
    }
  }
  
  
  
  gg_dynamics_cv <- 
  ggplot(data, aes(x = date, y = .data[[variable]])) + 
    
    facet_wrap(~ treatment ,  nrow = 1, ncol = 4, labeller = labeller(treatment = labels)) +
    
    geom_smooth(
      se = TRUE, aes(color = treatment, fill = treatment),
      method = "loess", span = 0.6, alpha = 0.2 ) +
    
    geom_point(aes(color = treatment), alpha = 0.2) +
    
    geom_line(group = "treatment", aes(color = treatment, alpha = 0.5)) +
    
    #geom_vline(xintercept = 1.5, linetype = "dashed", color = "black", alpha = 0.5) + 
    
    scale_colour_manual(values = palette) +
    
    scale_fill_manual(values = palette) +
    
    # scale_x_discrete(breaks = levels(ab_rich_dynamics$sampling_date)[c(3, 7, 11, 15, 19)]) +
    
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
    
    labs(y = ytitle, x = NULL)
  

  
  gg_dynamics_cv <<- gg_dynamics_cv

} 

