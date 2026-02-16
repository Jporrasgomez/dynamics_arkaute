


rm(list = ls(all.names = TRUE))
pacman::p_load(dplyr, reshape2, tidyverse, lubridate, ggplot2, ggpubr, rpivotTable, ggrepel, here)

source("code/palettes_labels.R")


biomass_nind <-  read.csv("data/biomass_nind_for_mice.csv")

biomass <- biomass_nind


sum(is.na(biomass$nind_m2)) 

sum(is.na(biomass$nind_m2)) /length(biomass$code) * 100





#Where are the NA's?




NA_biomass <- biomass %>% 
  mutate(cell_content = case_when(is.na(nind_m2) ~ "NA",
                                  !is.na(nind_m2)  ~ "Number of individuals available"))




  



na1 <- ggplot(NA_biomass, aes(x = sampling, fill = cell_content)) +
  geom_bar() +
  labs(
    y = "Number of rows",
    x = "Plot",
    fill = NULL
  ) +
  theme1



na2 <- ggplot(NA_biomass, aes(x = plot, fill = cell_content)) +
    geom_bar() +
    labs(
      y = "Number of rows",
      x = "Plot",
      fill = NULL
    ) +
    theme1
  

na3 <- ggplot(
    NA_biomass,
    aes(x = fct_infreq(code), fill = cell_content)
  ) +
    geom_bar() +
    labs(
      y = NULL,
      x = "Species code",
      fill = NULL
    ) +
    gg_RR_theme
  
  library(patchwork)
  gg_na <-
    (na1 + na2 + na3) +
       plot_layout(ncol = 1, 
                   guides = "collect") +
    plot_annotation(theme = theme(legend.position = "bottom"))
  print(gg_na)
  
  #ggsave("results/Plots/protofinal/NA_biomass.png", plot = gg_na, dpi = 300)
  #ggsave("results/Plots/protofinal/NA_biomass.png.svg", plot = gg_na, dpi = 300)
  
  
  
  
na1_perc <- 
  NA_biomass %>%
    group_by(sampling) %>%
    summarise(
      n_NA = sum(cell_content == "NA", na.rm = TRUE),
      n_available = sum(cell_content == "Number of individuals available", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(total_n = n_NA + n_available) %>% 
    mutate(percentage = (n_NA / total_n) * 100) %>%
    ggplot(aes(x = sampling, y = percentage)) +
    geom_col(fill = "gray40") +
    labs(
      y = NULL,
      x = "Sampling"
    ) +
    theme1
  
  
na2_perc <- 
  NA_biomass %>%
    group_by(plot) %>%
    summarise(
      n_NA = sum(cell_content == "NA", na.rm = TRUE),
      n_available = sum(cell_content == "Number of individuals available", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(total_n = n_NA + n_available) %>% 
    mutate(percentage = (n_NA / total_n) * 100) %>%
    ggplot(aes(x = plot, y = percentage)) +
    geom_col(fill = "gray40") +
    labs(
      y = "Missing data (%)",
      x = "Plot"
    ) +
    theme1
  
  
na3_perc <-  
  NA_biomass %>%
    group_by(code) %>%
    summarise(
      n_NA = sum(cell_content == "NA", na.rm = TRUE),
      n_available = sum(cell_content == "Number of individuals available", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      total_n = n_NA + n_available,
      percentage = (n_NA / total_n) * 100
    ) %>%
    ggplot(aes(x = reorder(code, -percentage), y = percentage)) +
    geom_col(fill = "gray40") +
    labs(
      y = NULL,
      x = "Species code"
    ) +
  theme1 + theme( axis.text.x = element_text(
    angle = 45,
    hjust = 1,
    face  = "plain",
    size  = 12
  ))

  


  gg_na_perc <-
      (na1_perc + na2_perc + na3_perc) +
      plot_layout(ncol = 1, 
                  guides = "collect") +
      plot_annotation(theme = theme(legend.position = "none"))
    print(gg_na_perc)
    
    #ggsave("results/Plots/protofinal/NA_perc_biomass.png", plot = gg_na_perc, dpi = 300)


#NA's seem to be concentrated in the first year, as we already knew
# NA's are randomly distributed across plots
# NA's are randomly distributed across species, but we can see some that have more than others like: 
# poaceae, coar or shar



na <- biomass

na <- na %>% 
  mutate(nind_m2_na = ifelse(is.na(na$nind_m2), -1, na$nind_m2))


plots <- sort(unique(biomass$plot))
nalist <- list()
count = 0

for(i in seq_along(plots)){
  
  count = count + 1
  
  nalist[[count]]<- 
    ggplot(subset(na, plot == plots[i]), aes(x = sampling, y = code, fill = nind_m2_na)) +
    geom_tile(color = "gray13") +  # Keep black grid lines
    geom_text(aes(label = nind_m2_na), size = 2.3) +  # Add text labels
    scale_fill_gradientn(
      colors = c("red3", "#A9D8F2", "#2A5D9C"),  # Very pale gray (#F0F0F0), white, orange
      values = scales::rescale(c(-1, 1, max(na$nind_m2, na.rm = TRUE))),
      name = "Observations"
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),  # Remove default grid lines
      axis.text.y = element_text(face = "italic"),  # Italicize species names
      legend.position = "bottom"  # Move legend to the bottom
    ) +
    labs(x = "Samplings", y = "Species", title = paste0("Plot ", plots[i]))
  
}

for (i in seq_along(plots)) {
  print(nalist[[i]])
  
}


# MICE #####


library(mice)

biomass_mice <- biomass %>% 
  select(year, sampling, plot, treatment, code,
         family, richness, abundance, abundance_community,
         height, Ah, Ab, biomass_i, nind_m2) %>% 
  mutate(across(where(is.character), as.factor))

# We use mice package
##| method = "rf". Random forest models allow us to use both numerical and factorial variables in the imputation. Therefore, code, sampling and plot are used
##| m = 10 is the number of datasets that mice creates. This is, 10 values of nind_m2
##| maxit is the number of times that mice iterates through each dataset. The higher, the more stable (in complex database)


#biomass_mice_imputed <- mice(biomass_mice, method = "rf", m = 10, maxit = 300)
#saveRDS(biomass_mice_imputed, "data/biomass_mice_imputed.rds")
biomass_mice_imputed <- readRDS("data/biomass_mice_imputed.rds")

#summary(biomass_mice_imputed)
#plot(biomass_mice_imputed) # Check if lines stabilize
##stripplot(biomass_mice_imputed, pch = 20, cex = 1.2) #If imputed values (blue dots) align well with observed values, MICE is working well.
#densityplot(biomass_mice_imputed)


#i = 9
#imputed_dbi <- complete(biomass_mice_imputed, action = i)
#ggplot() +
#  geom_density(aes(x = biomass$nind_m2), color = "blue3") +
#  geom_density(aes(x = imputed_dbi$nind_m2), color = "red3") +
#  labs(title = "Density Plot of Original (Red) vs Imputed (Blue) nind_m2")


imputed_db <- complete(biomass_mice_imputed, action = "long")


imputation_names <- paste("Imputation", 1:10)

imputed_db$.imp <- factor(
  imputed_db$.imp,
  levels = sort(as.numeric(unique(imputed_db$.imp)))
)

imputed_db$imp_label <- factor(
  paste("Imputation", imputed_db$.imp),
  levels = paste("Imputation", 1:10)
)

ggdensity <- ggplot() +
  geom_density(
    data = biomass,
    aes(x = nind_m2, color = "Original values distribution"),
    linewidth = 4
  ) +
  geom_density(
    data = imputed_db,
    aes(x = nind_m2, color = imp_label)
  ) +
  scale_color_manual(
    values = c(
      "Original values distribution" = "gray70",
      setNames(scales::hue_pal()(10), imputation_names)
    )
  ) +
  labs(
    x = expression(delta~"(ind/"*m^2*")"),
    y = "Kernel density estimate",
    color = NULL
  ) +
  theme1+ theme(
    legend.position = c(0.75, 0.6),
    legend.background = element_blank()
  )

print(ggdensity)

#ggsave("results/Plots/protofinal/gg_mice_density.png", plot = ggdensity, dpi = 300)




# Calculation of average value for 10 imputations of nind_m2 (m = 10) and its sd. 

imputed_db <- imputed_db %>% 
  group_by(plot, treatment, sampling, code) %>% 
  mutate(nind_m2_imputed = round(mean(nind_m2, 0)),
         sd_imputation = sd(nind_m2)) %>% 
  select(plot, treatment, sampling, code, nind_m2_imputed, sd_imputation) %>% 
  distinct()

imputed_db$label_imputation <- ifelse(is.na(biomass$nind_m2), 1, 0)

# Here I plot the CV (Coefficient of variation) of the imputed values (n = 10) So I can check the stability of the imputed
# data. The more CV < 1, the more stable it is


imput_stability_db <- imputed_db %>% 
  filter(label_imputation == 1) %>% 
  mutate(CV = sd_imputation / nind_m2_imputed) %>% 
  as.data.frame()

mean(imput_stability_db$CV)
sd(imput_stability_db$CV)

library(ggdist)

raincloud_plot <- ggplot(imput_stability_db, aes(x = 1, y = CV)) +
  
  # Half-violin (the "cloud")
  stat_halfeye(
    adjust = 0.5,
    width = 0.6,
    justification = -0.3,
    .width = 0,
    point_colour = NA,
    fill = "gray40"
  ) +
  
  
  # Raw data (the "rain")
  geom_jitter(
    width = 0.08,
    alpha = 0.4,
    size = 2
  ) +
  
  # Boxplot (the "box")
  geom_boxplot(
    width = 0.2,
    outlier.shape = NA,
    alpha = 0.4,
    linewidth = 0.7
  ) +
  
  coord_flip() +
  scale_x_continuous(breaks = NULL) +
  labs(
    x = NULL,
    y = "CV"
  ) +
  
  theme1

raincloud_plot

#ggsave("results/Plots/protofinal/gg_raincloud_plot_mice_imputation.png", plot = raincloud_plot, dpi = 300)


gg_mice_imputations <-
  (ggdensity + 
     theme1 + theme(
       legend.position = c(0.75, 0.6),
       legend.background = element_blank()
     ) + raincloud_plot) +
  plot_layout(ncol = 1) + 
  plot_annotation(tag_levels = "A")
print(gg_mice_imputations)

#ggsave("results/Plots/protofinal/mice_imputation.png", plot = gg_mice_imputations, dpi = 300)

library(ggdist)
  
  mice_results <-
    ggplot(imputed_db, aes( x = sampling, y = nind_m2_imputed, color = as.factor(label_imputation))) +
    facet_wrap(~code, ncol = 7, nrow = 6)+
    geom_point(alpha = 0.5) +
    scale_color_manual(values = c("0" = "cyan3", "1" = "red3")) +
    scale_x_discrete(breaks = levels(imputed_db$sampling)[seq(1, length(levels(imputed_db$sampling)), by = 5)]) +  # Show every 2nd label
    labs(color = "Imputation")
  
  
# Reliability

# Checking reliability of mice imputation 
# Let's recreate
perc_NA <- ((biomass %>% 
               filter(is.na(nind_m2)) %>% 
               nrow())) / nrow(biomass)

##| I create a database where there are no NA in nind_m2.
##| Also, in this db (nind_nona) i take out the species for which we always have found 1 individuals so they do not
##| play a role in the original imputation because always nind_m2 = 1. And if we randomly create an NA in one of these species
##| the imputation stability might be negatively and unnecessarilyaffected 

nind_nona <- biomass %>% 
  filter(!is.na(nind_m2)) %>%  
  filter(!code %in% one_ind_species) 

######### VERY VERY VERY LONG TIME TO RUN LOOP : ###########################################################
#source("code/1.1.1.loop_reliability.R")


#stability_db_combined <- imap_dfr(stability_list, ~ mutate(.x, counter = .y)) 
#stability_db_combined %>% write.csv("data/stability_test.csv", row.names = F)


#reliability_db_combined <- imap_dfr(reliability_db_list, ~ mutate(.x, counter = .y))
#reliability_db_combined %>% write.csv("data/reliability_test.csv", row.names = F)

#reliability_plot_combined <- imap_dfr(reliability_plot_list, ~ mutate(.x, counter = .y))
#reliability_plot_combined %>% write.csv("data/reliability_LM_test.csv", row.names = F)

######### VERY VERY VERY LONG TIME TO RUN LOOP : ###########################################################



stability_test <- read.csv(here("data", "stability_test.csv"))



mean(stability_test$CV)
sd(stability_test$CV)


raincloud_plot_reliability <- ggplot(stability_test, aes(x = 1, y = CV)) +
  
  # Half-violin (the "cloud")
  stat_halfeye(
    adjust = 0.5,
    width = 0.6,
    justification = -0.3,
    .width = 0,
    point_colour = NA,
    fill = "gray40"
  ) +
  
  
  # Raw data (the "rain")
  geom_jitter(
    width = 0.08,
    alpha = 0.4,
    size = 2
  ) +
  
  # Boxplot (the "box")
  geom_boxplot(
    width = 0.2,
    outlier.shape = NA,
    alpha = 0.4,
    linewidth = 0.7
  ) +
  
  coord_flip() +
  scale_x_continuous(breaks = NULL) +
  labs(
    x = NULL,
    y = "CV"
  ) +
  
  theme1

print(raincloud_plot_reliability)

reliability_test <- read.csv(here("data", "reliability_test.csv")) %>% 
  mutate(counter = as.factor(counter))



library(dplyr)
library(tidyr)

reliability_LM_test <- read.csv(here("data", "reliability_LM_test.csv"))

reg_tab <- reliability_LM_test %>%                    # <-- reemplaza con tu data.frame real
  group_by(counter, .imp) %>%
  group_modify(~{
    dat <- .x %>%
      select(nind_m2, nind_m2_original) %>%
      mutate(
        nind_m2 = as.numeric(nind_m2),
        nind_m2_original = as.numeric(nind_m2_original)
      ) %>%
      drop_na()
    
    if (nrow(dat) < 2 || dplyr::n_distinct(dat$nind_m2_original) < 2) {
      return(tibble(
        intercept = NA_real_,
        slope     = NA_real_,
        r2        = NA_real_,
        p_value   = NA_real_
      ))
    }
    
    fit <- lm(nind_m2 ~ nind_m2_original, data = dat)
    s   <- summary(fit)
    
    tibble(
      intercept = unname(coef(fit)[1]),
      slope     = unname(coef(fit)[2]),
      r2        = s$r.squared,
      p_value   = suppressWarnings(coef(s)[2, 4])
    )
  }) %>%
  ungroup() %>%
  # Añadimos columna 'imp' (copia de .imp) y ordenamos
  mutate(
    counter_chr = as.character(counter),
    counter_num = suppressWarnings(as.numeric(counter_chr)),
    imp = .imp
  ) %>%
  arrange(counter_num, imp) %>%
  select(slope, intercept, r2, p_value, counter, imp) %>%
  mutate(across(c(slope, intercept, r2, p_value), ~round(., 6)))

reg_tab


reg_tab %>% 
  ggplot(aes(y = slope)) + 
  geom_boxplot()

mean(reg_tab$slope)
sd(reg_tab$slope)


reg_tab %>% 
  ggplot(aes(y = intercept)) + 
  geom_boxplot()

mean(reg_tab$intercept)
sd(reg_tab$intercept)


reg_tab %>% 
  ggplot(aes(y = r2)) + 
  geom_boxplot()

mean(reg_tab$r2)
sd(reg_tab$r2)


reg_tab %>% 
  ggplot(aes(y = p_value)) + 
  geom_boxplot()

mean(reg_tab$p_value)
sd(reg_tab$p_value)



## Merging imputation results with biomass database : 

biomass_imp <- biomass %>% 
  select(year, date, sampling, one_month_window, omw_date, plot, code, species_level, genus_level, family,
         abundance, height, Ah, Ab, x, biomass_i, richness, abundance_community, nind_m2) %>% 
  left_join(imputed_db)




  