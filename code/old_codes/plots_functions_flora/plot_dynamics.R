

# load packages
library(MetBrewer)
library(tidyverse)

# function to plot recovery dynamics (output is a ggplot object)
plot_dynamics <- function(full_data, mean_data, type) {
  
  # set theme for the plot
  theme_set(theme_bw() +
              theme(axis.title.x = element_blank(),
                    legend.position = "NULL",
                    panel.grid = element_blank(),
                    strip.background = element_blank(),
                    strip.text = element_text(face = "bold"),
                    text = element_text(size = 11)))
  
  # define y axis title
  if (type %in% c("richness", "abundance")) {
    ytitle <- str_to_title(type)
  } else {
    if (type == "sigma_log") {
      ytitle <- "Sigma (Coefficient 2 in Log model for RADs)"
    } else {
      if (type == "mu_log") {
        ytitle <- "Mu (Coefficient 1 in Log model for RADs)"
      } else {
        if (type == "Y_zipf") {
          ytitle <- "Gamma (Coefficient in Zipf model for RADs)"
        } else {
          if (type == "biomass") {
            ytitle <- "Community biomass"
          } else{
          stop("type must be one of the following: richness, abundance, sigma_log, mu_log, Y_zipf, biomass_total")
          }
        }
      }
    }
  }
  
  # define treatment names
  treatment_labs_dynamics <- c("Control", "Warming", "Perturbation", "Warming and perturbation")
  names(treatment_labs_dynamics) <- c("c","w", "p", "wp")
  
  # plot
  
  if(type %in% c("richness", "abundance", "sigma_log", "mu_log", "Y_zipf")){
    
  graph <- 
    
    ggplot(mean_data, aes(x = as.factor(sampling_date),
                          y = .data[[paste0("mean_", type)]],
                          group = sampling_date)) +
    
    facet_wrap(~ treatment, labeller = labeller(treatment = treatment_labs_dynamics, nrow = 2, ncol = 2)) +  
    
    geom_errorbar(aes(ymin = mean_data[[paste0("mean_", type)]] - mean_data[[paste0("sd_", type)]],
                      ymax = mean_data[[paste0("mean_", type)]] + mean_data[[paste0("sd_", type)]],
                      color = treatment, alpha = 0.5),
                  position = position_dodge(width = 0.5), width = 0.25, size = 0.75, alpha = 0.4) +
    
    geom_point(data = full_data, aes(x = as.factor(sampling_date),
                                     y = .data[[type]],
                                     color = treatment),
               position = position_dodge(width = 0.5), size = 1.5, alpha = 0.2) +
    
    geom_path(group = 1, aes(color = treatment), linewidth = 0.8) +
    
    geom_point(aes(color = treatment), fill = "white", position = position_dodge(width = 0.5), size = 2.25, shape = 21) +
    
    #scale_colour_manual(values = met.brewer("VanGogh2", 4)) +
    
    scale_colour_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
    
    geom_vline(xintercept = 1.5, linetype = "dashed", color = "gray40") +
    
    scale_x_discrete(breaks = levels(as.factor(mean_data$sampling_date))[seq(1, length(levels(as.factor(mean_data$sampling_date))), by = 2)]) +
    
    labs(y = ytitle) +
    
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
  return(graph)
  
  } else{ #I just take out the hline in case it is biomass
    
    graph <- 
      
      ggplot(mean_data, aes(x = as.factor(sampling_date),
                            y = .data[[paste0("mean_", type)]],
                            group = sampling_date)) +
      
      facet_grid(~ treatment, labeller = labeller(treatment = treatment_labs_dynamics)) +  
      
      geom_errorbar(aes(ymin = mean_data[[paste0("mean_", type)]] - mean_data[[paste0("sd_", type)]],
                        ymax = mean_data[[paste0("mean_", type)]] + mean_data[[paste0("sd_", type)]],
                        color = treatment, alpha = 0.5),
                    position = position_dodge(width = 0.5), width = 0.25, size = 0.75, alpha = 0.4) +
      
      geom_point(data = full_data, aes(x = as.factor(sampling_date),
                                       y = .data[[type]],
                                       color = treatment),
                 position = position_dodge(width = 0.5), size = 1.5, alpha = 0.2) +
      
      geom_path(group = 1, aes(color = treatment), linewidth = 0.8) +
      
      geom_point(aes(color = treatment), fill = "white", position = position_dodge(width = 0.5), size = 2.25, shape = 21) +
      
      scale_colour_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
      
      #geom_vline(xintercept = 1.5, linetype = "dashed", color = "gray40") +
      
      scale_x_discrete(breaks = levels(as.factor(mean_data$sampling_date))[seq(1, length(levels(as.factor(mean_data$sampling_date))), by = 2)]) +
      
      labs(y = ytitle) +
      
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
    
    return(graph)
    
  }
  
}

  
  
  
  
  
  
  #end of plot_recovery

# end of the script