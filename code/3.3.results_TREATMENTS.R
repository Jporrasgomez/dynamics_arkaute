



source("code/1.first_script.R")

#turnover_db <- read.csv("data/turnover_db.csv")
nmds_df_plot <- read.csv("data/nmds_df_plot.csv")

rm(list = setdiff(ls(), c("ab_rich_dynamics", "biomass_imp", "biomass_imp012",
                          "turnover_db", "nmds_df_plot")))
                          
source("code/palettes_labels.R")                          
palette <- palette5
labels <- labels_RR

source("code/meta_function/RR_TREATMENT_c.R")
source("code/meta_function/RR_TREATMENT_wp.R")


{list_c <- list()
gglist_c <- list()
list_wp <- list()
gglist_wp <- list()

variables <- c("richness", "abundance", "Y_zipf")
for (i in 1:3){
  RR_treatment_c(ab_rich_dynamics, variables[i])
  list_c[[i]] <- RR_treatment %>%
    select(RR_descriptor, variable, starts_with("RR"), starts_with("se_RR"))
  gglist_c[[i]] <- gg_RR
  
  RR_treatment_wp(ab_rich_dynamics, variables[i])
  list_wp[[i]] <- RR_wp_vs_treatment %>%
    select(RR_descriptor, variable, starts_with("RR"), starts_with("se_RR"))
  gglist_wp[[i]] <- gg_RR_wp
  
}

gglist_c[[1]]
gglist_c[[2]]
gglist_c[[3]]

gglist_wp[[1]]
gglist_wp[[2]]
gglist_wp[[3]]


RR_c <- reduce(list_c, bind_rows)
RR_wp <- reduce(list_wp, bind_rows)}



{RR_treatment_c(biomass_imp012, "biomass")
  RR_biomass_c012 <- RR_treatment %>%
    select(RR_descriptor, variable, starts_with("RR"), starts_with("se_RR")) %>% 
    mutate(variable = fct_recode(variable,
                                  "biomass012" = "biomass"))
  print(gg_RR)
}

{RR_treatment_wp(biomass_imp012, "biomass")
  RR_biomass_wp012 <- RR_wp_vs_treatment %>%
    select(RR_descriptor, variable, starts_with("RR"), starts_with("se_RR")) %>% 
    mutate(variable = fct_recode(variable,
                                 "biomass012" = "biomass"))
  print(gg_RR_wp)
}


{RR_treatment_c(biomass_imp, "biomass")
  RR_biomass_c <- RR_treatment %>%
    select(RR_descriptor, variable, starts_with("RR"), starts_with("se_RR"))
  print(gg_RR)
}

{RR_treatment_wp(biomass_imp, "biomass")
  RR_biomass_wp <- RR_wp_vs_treatment %>%
    select(RR_descriptor, variable, starts_with("RR"), starts_with("se_RR"))
  print(gg_RR_wp)
}


RR_c <- bind_rows(RR_c, RR_biomass_c)
RR_c <-bind_rows(RR_c, RR_biomass_c012)
#  RR_c <- RR_c %>% 
# filter(! variable %in% c("mu_log", "sigma_log"))


RR_wp <- bind_rows(RR_wp, RR_biomass_wp) 
RR_wp <- bind_rows(RR_wp, RR_biomass_wp012) 
#  RR_wp <- RR_wp %>% 
# filter(! variable %in% c("mu_log", "sigma_log"))


{list_NMDS_c <- list()
list_NMDS_wp <- list()
gglist_NMDS_c <- list()
gglist_NMDS_wp <- list()
NMDS <- c("NMDS1", "NMDS2", "NMDS3")
for (i in 1:3){
  RR_treatment_c(nmds_df_plot, NMDS[i])
  list_NMDS_c[[i]] <- RR_treatment %>%
    select(RR_descriptor, variable, starts_with("RR"), starts_with("se_RR"))
  gglist_NMDS_c[[i]] <- gg_RR
  
  RR_treatment_wp(nmds_df_plot, NMDS[i])
  list_NMDS_wp[[i]] <- RR_wp_vs_treatment %>%
    select(RR_descriptor, variable, starts_with("RR"), starts_with("se_RR"))
  gglist_NMDS_wp[[i]] <- gg_RR_wp
  
}}

gglist_NMDS_c[[1]]
gglist_NMDS_c[[2]]
gglist_NMDS_c[[3]]

gglist_NMDS_wp[[1]]
gglist_NMDS_wp[[2]]
gglist_NMDS_wp[[3]]

RR_NMDS_c <- reduce(list_NMDS_c, bind_rows)
RR_NMDS_wp <- reduce(list_NMDS_wp, bind_rows)


RR_c <- bind_rows(RR_c, RR_NMDS_c)
RR_wp <- bind_rows(RR_wp, RR_NMDS_wp)


#{list_turnover_c <- list()
#  list_turnover_wp <- list()
#  gglist_turnover_c <- list()
#  gglist_turnover_wp <- list()
#  turnover <- c("total_turnover", "appearance", "disappearance")
#  for (i in 1:3){
#    RR_treatment_c(turnover_db, turnover[i])
#    list_turnover_c[[i]] <- RR_treatment %>%
#      select(treatment, variable, starts_with("RR"), starts_with("se_RR"))
#    gglist_turnover_c[[i]] <- gg_RR
#    
#    RR_treatment_wp(turnover_db, turnover[i])
#    list_turnover_wp[[i]] <- RR_wp_vs_treatment %>%
#      select(RR_descriptor, variable, starts_with("RR"), starts_with("se_RR"))
#    gglist_turnover_wp[[i]] <- gg_RR_wp
#  
#  }}
#
#RR_turnover <- do.call(rbind, list_turnover_c)
#RR_turnover_wp <- do.call(rbind, list_turnover_wp)
#
#gglist_turnover_c[[1]]
#gglist_turnover_c[[2]]
#gglist_turnover_c[[3]]
#gglist_turnover_wp[[1]]
#gglist_turnover_wp[[2]]
#gglist_turnover_wp[[3]]
#
#
#
#RR_c <- bind_rows(RR_c, RR_turnover)
#
#RR_wp <- bind_rows(RR_wp, RR_turnover_wp)





z = 1.96

{gg_RR_c <- 
  RR_c %>% 
  mutate(
    RR = if_else(variable == "Y_zipf", -1 * RR, RR), 
    se_RR = if_else(variable == "Y_zipf", -1 * se_RR, se_RR)) %>% 
  ## Multiplying by -1 gamma zipf in order to have positive values and being able to read the plots as
  ## evenness
  ggplot(aes(x = variable, y = RR, color = RR_descriptor)) + 
  geom_errorbar(
    aes(ymin = RR - z * se_RR,
        ymax = RR + z * se_RR),
    linewidth = 0.5,
    position = position_dodge(width = 0.2),
    width = 0.1
  ) +  
  geom_point(position = position_dodge(width = 0.2)) + 
  scale_color_manual(values = palette_RR, labels = labels_RR) +
  scale_x_discrete(
    limits = c("richness", "abundance", "Y_zipf", "biomass", "biomass012",
               "NMDS1", "NMDS2"),
    labels = c(
      "richness" = "Richness",
      "abundance" = "Cover",
      "Y_zipf" = "Evenness",
      "biomass" = "Biomass",
      "biomass012" = "Biomass012",
      "NMDS1" = "Sp.Comp.(NMDS1)",
      "NMDS2" = "Sp.Comp.(NMDS2")) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(x = NULL, y = "RR of mean values at plot level", color = NULL) +
  theme(legend.position = "bottom")

print(gg_RR_c)

ggsave("results/Plots/protofinal/treatment_effects.png", plot = gg_RR_c, dpi = 300)}



{gg_RR_wp <- 
RR_wp %>%
  mutate(
    RR = if_else(variable == "Y_zipf", -1 * RR, RR),
    se_RR = if_else(variable == "Y_zipf", -1 * se_RR, se_RR)) %>% 
  #filter(RR_descriptor == "wp_vs_p") %>% 
ggplot(aes(x = variable, y = RR, color = RR_descriptor)) + 
  #facet_wrap(~ RR_descriptor, ncol = 1, nrow = 2, labeller = labeller(RR_descriptor = labels_RR_wp2)) +
  geom_errorbar(aes(ymin = RR - z * se_RR,
                    ymax = RR + z * se_RR,
                    color = RR_descriptor), 
                linewidth = 0.5,
                position = position_dodge(width = 0.2),
                width = 0.1) +  
  geom_point(aes(color = RR_descriptor), position = position_dodge(width = 0.2)) + 
  scale_color_manual(values = palette_RR_wp, labels = labels_RR_wp2) +
  scale_x_discrete(
    limits = c("richness", "abundance", "Y_zipf", "biomass", "biomass012",
               "NMDS1", "NMDS2"),
    labels = c(
      "richness" = "Richness",
      "abundance" = "Cover",
      "Y_zipf" = "Evenness",
      "biomass" = "Biomass",
      "biomass012" = "Biomass012",
      "NMDS1" = "Sp.Comp.(NMDS1)",
      "NMDS2" = "Sp.Comp.(NMDS2")) + 
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(x = NULL, y = "RR of mean values at plot level", color = NULL) +
  theme(
    legend.position = "bottom",
   # strip.text = element_blank(),  # 🔹 Quita el título del facet_wrap
    axis.text.x = element_blank(), # 🔹 Oculta etiquetas del eje X en todos los paneles
    panel.spacing = unit(0.5, "lines") # 🔹 Espaciado entre paneles
  ) +
  theme(
    axis.text.x = element_text() # 🔹 Vuelve a mostrar las etiquetas del último panel
  )

print(gg_RR_wp)

ggsave("results/Plots/protofinal/globalchange_effects.png", plot = gg_RR_wp, dpi = 300)}



## Unir por RR_descriptor?

