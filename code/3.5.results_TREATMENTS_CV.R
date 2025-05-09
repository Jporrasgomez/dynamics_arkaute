


source("code/1.first_script.R")


nmds_df_plot <- read.csv("data/nmds_df_plot.csv") %>% 
  mutate(treatment = as.factor(treatment),
         plot = as.factor(plot),
         sampling = as.factor(sampling),
         date = ymd(date))

cwm_plot_db <- read.csv("data/cwm_plot_db.csv") %>% 
  mutate(treatment = as.factor(treatment),
         plot = as.factor(plot),
         sampling = as.factor(sampling),
         date = ymd(date))


rm(list = setdiff(ls(), c("ab_rich_dynamics", "biomass_imp", "biomass_imp012", "nmds_df_plot", "cwm_plot_db")))

source("code/palettes_labels.R")                          
palette <- palette5
labels <- labels_RR

source("code/meta_function/RR_TREATMENT_CV.R")

{list <- list()

  variables <- c("richness", "abundance", "Y_zipf")
  for (i in 1:3){
    RR_treatment_cv(ab_rich_dynamics, variables[i])
    list[[i]] <- RR_cv
    
    RR_cv_abricheven <- do.call(rbind, list)

  }
}


{RR_treatment_cv(biomass_imp, "biomass")
RR_cv_biomass <- RR_cv
  
RR_treatment_cv(biomass_imp012, "biomass")
RR_cv_biomass012 <- RR_cv %>% 
    mutate(variable = fct_recode(variable,
                                 "biomass012" = "biomass"))

RR_cv_biomass <- rbind(RR_cv_biomass, RR_cv_biomass012)

}




{list <- list()
  
  variables <- c("NMDS1", "NMDS2")
  for (i in 1:2){
    RR_treatment_cv(nmds_df_plot, variables[i])
    list[[i]] <- RR_cv
  }
  RR_cv_sp_comp <- do.call(rbind, list)
}


{list <- list()
  
  variables <- c("LA", "LDMC", "leafN", "seed.mass", "SLA", "vegetation.height")
  for (i in 1:6){
    RR_treatment_cv(cwm_plot_db, variables[i])
    list[[i]] <- RR_cv
  }
  RR_cv_traits <- do.call(rbind, list)
}


RR_cv <- rbind(RR_cv_abricheven, RR_cv_biomass) %>% 
  rbind(RR_cv_sp_comp) %>% 
  rbind(RR_cv_traits)
  


z = 1.96


{gg_RR_cv_treatment <- 
RR_cv %>% 
  filter(RR_descriptor %in% c("w_vs_c", "p_vs_c", "wp_vs_c")) %>% 
  filter(variable %in% c("richness", "abundance", "Y_zipf", "biomass", "biomass012")) %>% 
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
print(gg_RR_cv_treatment)
ggsave("results/Plots/protofinal/gg_RR_cv_treatment.png", plot = gg_RR_cv_treatment, dpi = 300)
}



{gg_RR_cv_treatment2 <- 
RR_cv %>%
  filter(RR_descriptor %in% c("w_vs_c", "p_vs_c", "wp_vs_c")) %>% 
  filter(variable %in% c("NMDS1", "NMDS2", "LA", "SLA", "LDMC", "vegetation.height", "seed.mass", 
                         "leafN")) %>% 
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
    limits = c("NMDS1", "NMDS2", "LA", "SLA", "LDMC", "vegetation.height", "seed.mass", 
               "leafN"),
    labels = c(
      "NMDS1" = "Sp.Comp.(NMDS1)",
      "NMDS2" = "Sp.Comp.(NMDS2)",
      "LA" = "LA",
      "SLA" = "SLA",
      "LDMC" = "LDMC",
      "vegetation.height" = "Height",
      "seed.mass" = "Seed mass", 
      "leafN" = "Leaf N"
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
print(gg_RR_cv_treatment2)
ggsave("results/Plots/protofinal/gg_RR_cv_treatment2.png", plot = gg_RR_cv_treatment2, dpi = 300)
}









{gg_RR_cv_treatment_GC <- 
    RR_cv %>% 
    filter(RR_descriptor %in% c("wp_vs_w", "wp_vs_p")) %>% 
    filter(variable %in% c("richness", "abundance", "Y_zipf", "biomass", "biomass012")) %>% 
    ggplot(aes(x = variable, y = RR, color = RR_descriptor)) + 
    geom_errorbar(aes(ymin = RR - z * se_RR,
                      ymax = RR + z * se_RR,
                      color = RR_descriptor), 
                  linewidth = 0.5,
                  position = position_dodge(width = 0.2),
                  width = 0.1) +  
    geom_point(aes(color = RR_descriptor), position = position_dodge(width = 0.2)) + 
    scale_color_manual(values = palette_RR_wp, labels = labels_RR_wp) +
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
  print(gg_RR_cv_treatment_GC)
  ggsave("results/Plots/protofinal/gg_RR_cv_treatment_GC.png", plot = gg_RR_cv_treatment_GC, dpi = 300)
}



{gg_RR_cv_treatment_GC2 <- 
    RR_cv %>%
    filter(RR_descriptor %in% c("wp_vs_w", "wp_vs_p")) %>% 
    filter(variable %in% c("NMDS1", "NMDS2", "LA", "SLA", "LDMC", "vegetation.height", "seed.mass", 
                           "leafN")) %>% 
    ggplot(aes(x = variable, y = RR, color = RR_descriptor)) + 
    geom_errorbar(aes(ymin = RR - z * se_RR,
                      ymax = RR + z * se_RR,
                      color = RR_descriptor), 
                  linewidth = 0.5,
                  position = position_dodge(width = 0.2),
                  width = 0.1) +  
    geom_point(aes(color = RR_descriptor), position = position_dodge(width = 0.2)) + 
    scale_color_manual(values = palette_RR_wp, labels = labels_RR_wp) +
    scale_x_discrete(
      limits = c("NMDS1", "NMDS2", "LA", "SLA", "LDMC", "vegetation.height", "seed.mass", 
                 "leafN"),
      labels = c(
        "NMDS1" = "Sp.Comp.(NMDS1)",
        "NMDS2" = "Sp.Comp.(NMDS2)",
        "LA" = "LA",
        "SLA" = "SLA",
        "LDMC" = "LDMC",
        "vegetation.height" = "Height",
        "seed.mass" = "Seed mass", 
        "leafN" = "Leaf N"
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
  print(gg_RR_cv_treatment_GC2)
  ggsave("results/Plots/protofinal/gg_RR_cv_treatment_GC2.png", plot = gg_RR_cv_treatment_GC2, dpi = 300)
}

