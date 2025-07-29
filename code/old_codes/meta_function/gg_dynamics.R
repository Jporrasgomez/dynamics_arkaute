



# function to plot recovery dynamics (output is a ggplot object)
gg_dynamics <- function(data, variable) {
  
  # set theme for the plot
  theme_set(theme_bw() +
              theme(axis.title.x = element_blank(),
                    legend.position = "NULL",
                    panel.grid = element_blank(),
                    strip.background = element_blank(),
                    strip.text = element_text(face = "bold"),
                    text = element_text(size = 11)))

  
  ytitle_dict <- list(
    
    "richness" = "Richness",
    "abundance" = "Community cover",
    "sigma_log" = "Sigma (Coefficient 2 in Log model for RADs)",
    "mu_log" = "Mu (Coefficient 1 in Log model for RADs)",
    "Y_zipf" = "Gamma (Coefficient in Zipf model for RADs)",
    "biomass" = "Community biomass",
    "biomass012" = "Community biomass",
    "NMDS1" = "NMDS1",
    "NMDS2" = "NMDS2",
    "NMDS3" = "NMDS3",
    "total_turnover" = "Total turnover", 
    "appearance" = "Turnover: appearance", 
    "disappearance" = "Turnover: disappearance",
    "SLA" = "SLA (?)", 
    "LA" = "LA(?)", 
    "LDMC" = "LDMC(?)", 
    "leafN"= "Leaf nitrogen(?)",
    "seed.mass" = "Seed mass",
    "vegetation.height" = "Vegetation height",
    "PC1" = "Functional diversity(PC1)",
    "PC2" = "Functional diverisity (PC2)"
    
  )
  
  ytitle <- ytitle_dict[[variable]]
  
  if (is.null(ytitle)) {
    stop("Variable must be one of the following: ", paste(names(ytitle_dict), collapse = ", "))
  }
  

  
  # plot

    
    gg_all1n <- 
      
      ggplot(data, aes(x = date),
                            y = mean) +
      
        geom_smooth(
          se = TRUE, aes(x = date, y = mean,
                         color = treatment, fill = treatment),
          method = "loess", span = 0.6, alpha = 0.2 
        ) +
      

      geom_errorbar(aes(ymin = mean - sd,
                        ymax = mean + sd,
                        color = treatment), 
                    alpha = 0.2, position = position_dodge(width = 8)) +
      
      geom_point(data = data, aes(x = date,
                                       y = value,
                                       color = treatment),
                 alpha = 0.2, position = position_dodge(width = 8))+
      
      
      geom_point(aes(x = date, y = mean,
                     color = treatment), fill = "white", position = position_dodge(width = 0.5), size = 2.1, shape = 21) +
        
      
      scale_colour_manual(values = palette)+
      scale_fill_manual(values = palette)+
      
      scale_x_date(
        date_breaks = "4 weeks", # Specify the interval (e.g., every 2 weeks)
        date_labels = "%y-%b-%d" # Customize the date format (e.g., "23-May-04")
      ) +
      
      geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
      
      labs(y = ytitle) +
      
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
    
    gg_all1n <<- gg_all1n
    
    
    
    
    
    gg_facet <- 
      
    ggplot(data, aes(x = date),
           y = mean) +
      
      facet_wrap(~ treatment ,  nrow = 1, ncol = 4, labeller = labeller(treatment = labels)) +
      
      geom_errorbar(aes(ymin = mean - sd,
                        ymax = mean + sd,
                        color = treatment), 
                    alpha = 0.2, position = position_dodge(width = 8)) +
      
      geom_point(data = data, aes(x = date,
                                  y = value,
                                  color = treatment),
                 alpha = 0.2, position = position_dodge(width = 8))+
      
      geom_line(group = "treatment", aes(x = date, y = mean, color = treatment), linewidth = 1) +
      
      
      geom_point(aes(x = date, y = mean,
                     color = treatment), fill = "white", position = position_dodge(width = 0.5), size = 2.1, shape = 21) +
      
      scale_colour_manual(values = palette)+
      #scale_fill_manual(values = palette)+
      
      geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
      
      labs(y = ytitle) +
      
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
    
    gg_facet <<- gg_facet
 
}




#end of plot_recovery

# end of the script