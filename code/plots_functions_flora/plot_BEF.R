

plot_BEF <- function(data) {
  
  library(ggpmisc)
  library(stats)

  ab_rich_dynamics <-  data %>%
    group_by(sampling, date, month, treatment, plot) %>%
    reframe(richness = richness,  #total number of species per plot
            abundance = abundance_total) %>% # total coverage of plot
    distinct(sampling, date, month, plot, treatment, richness, abundance)
  
  
  
  mean_sd_abrich_dynamics<- ab_rich_dynamics %>%
    group_by(treatment, sampling) %>%
    summarize(mean_richness = mean(richness),
              sd_richness = sd(richness),
              mean_abundance = mean(abundance),
              sd_abundance = sd(abundance))
  
  # Database for biomass
  
  biomass_dynamics <- data %>%
    filter(!sampling %in% c("0", "1", "2", "12"))  %>%
    rename(biomass = biomass_total) %>%
    group_by(sampling, date, month, treatment, plot) %>%
    reframe(biomass = biomass) %>% # total coverage of plot
    distinct(sampling, date, month, plot, treatment, biomass)
  
  mean_sd_biomass_dynamics<- biomass_dynamics %>%
    group_by(treatment, sampling) %>%
    summarize(mean_biomass = mean(biomass),
              sd_biomass = sd(biomass))
  
  
  BEF_dynamics <- merge(ab_rich_dynamics, biomass_dynamics)
  
  
  
  # set theme for the plot
  theme_set(theme_bw() +
              theme(legend.position = "NULL",
                    panel.grid = element_blank(),
                    strip.background = element_blank(),
                    strip.text = element_text(face = "bold"),
                    text = element_text(size = 11)))
  
  treatment_labs_dynamics <- c("Control", "Warming", "Perturbation", "Warming and perturbation")
  names(treatment_labs_dynamics) <- c("c","w", "p", "wp")
  
 graph <- 
  
  ggplot(BEF_dynamics, aes(x = richness, y = biomass))+
    
    facet_grid(~ treatment, labeller = labeller(treatment = treatment_labs_dynamics))+ 
    
    geom_point(aes(color = treatment)) +
    
    geom_smooth(method = "lm", aes( color = treatment, fill = treatment), alpha = 0.5)+
    
    scale_colour_manual(values = c("c" = "green4", "p" = "blue4", "w" = "red3", "wp" = "purple2")) +
    
    scale_fill_manual(values = c("c" = "green4", "p" = "blue4", "w" = "red3", "wp" = "purple2")) +
    
    stat_poly_eq(
      aes(label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "~~~")),
      formula = y ~ x, 
      parse = TRUE, 
      label.x.npc = 'left', 
      label.y.npc = 'top'
    ) +
    labs( x = "Richness", y = "Biomass")
  
  
  return(graph)
  
  

} #end of plot_BEF 

# end of the script