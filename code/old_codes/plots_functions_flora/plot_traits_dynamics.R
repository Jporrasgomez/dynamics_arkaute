


# load packages
library(MetBrewer)
library(tidyverse)

# function to plot recovery dynamics (output is a ggplot object)
plot_traits_dynamics <- function(full_data, mean_data, type) {
  
  # set theme for the plot
  theme_set(theme_bw() +
              theme(axis.title.x = element_blank(),
                    legend.position = "NULL",
                    panel.grid = element_blank(),
                    strip.background = element_blank(),
                    strip.text = element_text(face = "bold"),
                    text = element_text(size = 11)))
  
  # define y axis title ############### INDICATE UNITS
  ytitle <- switch(type,
                   "leafN" = "CWM - Leaf nitrogen ()",
                   "rootN" = "CWM - Root nitrogen ()",
                   "RTD" = "CWM - Root tissue density (RTD)", 
                   "seed.mass" = "CWM - Seed mass ()",
                   "LDMC" = "CWM - LDMC ()",
                   "vegetation.height" = "CWM - Vegetation height ()",
                   "SLA.inc" = "CWM - SLA .inc ()",
                   "LA.inc" = "CWM - LA .inc ()",
                   "SSD" = "CWM - Specific surface area (SSD)",
                   "SRL" = "CWM - Specific root length (SRL)",
                   "LA.ex" = "CWM - LA .ex ()",
                   "SLA.ex" = "CWM - SLA .ex ()",
                   "LA.un" = "CWM - LA unexpanded ()",
                   "LA" = "CWM - Leaf area ()",
  )
  
  # define treatment names
  treatment_labs_dynamics <- c("Control", "Warming", "Perturbation", "Warming and perturbation")
  names(treatment_labs_dynamics) <- c("c","w", "p", "wp")
  
  # plot
  
    
    graph <- 
      
      ggplot(mean_data, aes(x = as.factor(sampling_date),
                            y = mean_CWM,
                            group = sampling_date)) +
      
      facet_wrap(~ treatment, labeller = labeller(treatment = treatment_labs_dynamics, nrow = 2, ncol = 2)) +  
      
      geom_errorbar(aes(ymin = mean_data$mean_CWM - mean_data$sd_CWM,
                        ymax = mean_data$mean_CWM + mean_data$sd_CWM,,
                        color = treatment, alpha = 0.5),
                    position = position_dodge(width = 0.5), width = 0.25, size = 0.75, alpha = 0.4) +
      
      geom_point(data = full_data, aes(x = as.factor(sampling_date),
                                       y = CWM,
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
    
  
}







#end of plot_recovery

# end of the script