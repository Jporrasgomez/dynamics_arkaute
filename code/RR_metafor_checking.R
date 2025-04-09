




source("code/1.first_script.R")
source("code/meta_function/effect_size_treatment_c.R")
source("code/palettes_labels.R")



palette <- palette5
labels <- labels3

library(metafor)

{data_list <- list(ab_rich_dynamics, biomass_imp, biomass_imp012)
variables <- c("richness", "Y_zipf", "Y_zipf", "biomass")

j = 2
i = 4}

{data <- data_list[[j]] %>%
  distinct(sampling, date, plot, treatment, .data[[variables[i]]]) %>%
  group_by(treatment) %>%
  mutate(
    mean_variable = mean(.data[[paste0(variables[i])]], na.rm = T),   # Usamos .data para referirnos a la columna
    sd_variable = sd(.data[[paste0(variables[i])]], na.rm = T),
    n = n()
  ) %>%
  ungroup() %>%
  select(treatment, n, mean_variable, sd_variable) %>%
  distinct()
  

rr_data <- data %>%
  filter(treatment != "c") %>%
  mutate(
    mean_c = data$mean_variable[data$treatment == "c"],
    sd_c   = data$sd_variable[data$treatment == "c"],
    n_c    = data$n[data$treatment == "c"]
  ) %>%
  rename(mean_t = mean_variable, sd_t = sd_variable, n_t = n)



rr_es <- escalc(measure = "ROM",
                m1i = mean_t, sd1i = sd_t, n1i = n_t,
                m2i = mean_c, sd2i = sd_c, n2i = n_c,
                data = rr_data)

rr_analysis <- rma(yi = yi, vi = vi, data = rr_es)
#summary(rr_analysis)

rr_es_df <- as.data.frame(rr_es)




  forest(x = rr_es_df$yi,   # Los valores de los tamaños de efecto
       sei = sqrt(rr_es_df$vi),  # Error estándar (raíz cuadrada de la varianza)
       slab = rr_es_df$treatment,   # Etiquetas de los tratamientos
       xlab = "Effect size (ROM)")   # Etiqueta del eje X


effect_size_treatment_c(data_list[[j]], variables[i])

gg_RR


}
 


