






source("code/1.first_script.R")

rm(list = setdiff(ls(), c("ab_rich_dynamics", "biomass_imp", "biomass_imp012")))

source("code/palettes_labels.R")                          
palette <- palette5
labels <- labels_RR

source("code/meta_function/RR_TREATMENT_CV.R")

list <- list()

  variables <- c("richness", "abundance", "Y_zipf")
  for (i in 1:3){
    RR_treatment_cv(ab_rich_dynamics, variables[i])
    list[[i]] <- RR_cv

  }
  

RR_treatment_cv(biomass_imp, "biomass")
RR_cv_biomass <- RR_cv
  
RR_treatment_cv(biomass_imp012, "biomass")
RR_cv_biomass012 <- RR_cv %>% 
    mutate(variable = fct_recode(variable,
                                 "biomass012" = "biomass"))
    
  
RR_cv <- do.call(rbind, list) %>% 
  rbind(RR_cv_biomass) %>% 
  rbind(RR_cv_biomass012)


z = 1.96

RR_cv_c <- RR_cv %>% 
  filter(RR_descriptor %in% c("w_vs_c", "p_vs_c", "wp_vs_c"))

RR_cv_c %>% 
  ggplot(aes(x = variable, y = RR, color = RR_descriptor)) + 
  geom_errorbar(aes(ymin = RR - z * se_RR,
                    ymax = RR + z * se_RR,
                    color = RR_descriptor), 
                linewidth = 0.5,
                position = position_dodge(width = 0.2),
                width = 0.1) +  
  geom_point(aes(color = RR_descriptor), position = position_dodge(width = 0.2)) + 
  scale_color_manual(values = palette_RR, labels = labels_RR) +
  scale_x_discrete(
    limits = c("richness", "abundance", "Y_zipf", "biomass", "biomass012"),
    labels = c(
      "richness" = "Richness",
      "abundance" = "Cover",
      "Y_zipf" = "Evenness",
      "biomass" = "Biomass",
      "biomass012" = "Biomass012"
    )
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(x = NULL, y = "RR of mean CV values at sampling level", color = NULL) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(), # 🔹 Oculta etiquetas del eje X en todos los paneles
    panel.spacing = unit(0.5, "lines") # 🔹 Espaciado entre paneles
  ) +
  theme(
    axis.text.x = element_text() # 🔹 Vuelve a mostrar las etiquetas del último panel
  )



