
# load packages
library(MetBrewer)
library(tidyverse)

# function to plot recovery dynamics (output is a ggplot object)
plot_logRR_cv <- function(mean_data, type) {
  
  # set theme for the plot
  theme_set(theme_bw() +
              theme(axis.title.x = element_blank(),
                    legend.position = "NULL",
                    panel.grid = element_blank(),
                    strip.background = element_blank(),
                    strip.text = element_text(face = "bold"),
                    text = element_text(size = 11)))
  
  # define y axis title
  
  if (type == "abundance") {
    ytitle <- "Abundance (LogRR CV)"
  } else {
    if (type == "richness") {
      ytitle <- "Richness (LogRR CV)"
    } else {
      if (type == "sigma_log") {
        ytitle <- "Sigma for Log model RADs (LogRR CV)"
      } else {
        if (type == "mu_log") {
          ytitle <- "Mu for "
        } else {
          if (type == "Y_zipf") {
            ytitle <- "Gamma for Zipf model RADs (LogRR CV)"
          } else {
            if (type == "biomass") {
              ytitle <- "Community biomass (LogRR CV)"
            } else{
              stop("type must be one of the following: richness, abundance, sigma_log, mu_log, Y_zipf, biomass_total")
            }
          }
        }
      }
    }
  }
  
  
  
  
  # define treatment names
  treatment_labs_RR<- c("Warming", "Perturbation", "Warming and perturbation")
  names(treatment_labs_RR) <- c("w", "p", "wp")
  
  # plot

  if(type %in% c("richness", "abundance", "sigma_log", "mu_log", "Y_zipf")){  

  graph <- 
    
    ggplot(mean_data, aes(x = as.factor(sampling_date),  
                          y = .data[[paste0("logRR_cv_", type)]],
                          group = treatment)) +
 
    facet_grid(~ treatment, labeller = labeller(treatment = treatment_labs_RR)) +  
    
    geom_path(group = 1, aes(color = treatment), linewidth = 0.8, alpha = 0.5)+
    
    geom_point(aes(color = treatment), fill = "white", position = position_dodge(width = 0.5), size = 2.25, shape = 21) +
    
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.7) +
    
    geom_smooth(se = T, aes(color = treatment, fill = treatment), alpha = 0.2) +
    
    #scale_colour_manual(values = met.brewer("VanGogh2", 4)) +
    
    scale_colour_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
    scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
    
    geom_vline(xintercept = 1.5, linetype = "dashed", color = "gray40") +
    
    scale_x_discrete(breaks = levels(as.factor(mean_data$sampling_date))[seq(1, length(levels(as.factor(mean_data$sampling_date))), by = 2)]) +
    
    labs(y = ytitle) +
    
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
  return(graph)
  
  }else{
  
    graph <- 
      
      ggplot(mean_data, aes(x = as.factor(sampling_date),  
                            y = .data[[paste0("logRR_cv_", type)]],
                            group = treatment)) +
      
      facet_grid(~ treatment, labeller = labeller(treatment = treatment_labs_RR)) +  
      
      geom_path(group = 1, aes(color = treatment), linewidth = 0.8, alpha = 0.5)+
      
      geom_point(aes(color = treatment), fill = "white", position = position_dodge(width = 0.5), size = 2.25, shape = 21) +
      
      geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.7) +
      
      geom_smooth(se = T, aes(color = treatment, fill = treatment), alpha = 0.2) +
      
      #scale_colour_manual(values = met.brewer("VanGogh2", 4)) +
      
      scale_colour_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
      scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
      
      #geom_vline(xintercept = 1.5, linetype = "dashed", color = "gray40") +
      
      scale_x_discrete(breaks = levels(as.factor(mean_data$sampling_date))[seq(1, length(levels(as.factor(mean_data$sampling_date))), by = 2)]) +
      
      labs(y = ytitle) +
      
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
    
    return(graph)  
    
  }
  
} #end of plot_recovery

# end of the script