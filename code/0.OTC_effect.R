


rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)


pacman::p_load(dplyr, reshape2,tidyverse, lubridate, ggplot2, ggpubr, gridExtra, stringr, readr, nortest)



########### 1. OPENING DATA ######################
# ! it takes time ! around 1 minute

{
  plots <- read.csv("data/plots.csv")
plots$file_code <- paste0("data_", plots$sensor_code, "_2024_10_24_0.csv")

file_code_values <- plots$file_code

plots_list <- list()

for (i in seq_along(file_code_values)) {
  file_path <- file.path("data/data_sensors", file_code_values[i])
  new_name <- plots$plot_code[i]
  OTC_value <- plots$OTC[i]  # Get ttreat value for the current plot_code
  data <- read_delim(file_path, 
                     ";", escape_double = FALSE,
                     col_names = FALSE, 
                     trim_ws = TRUE)
  data$OTC <- OTC_value  # Add a new variable "ttreat" to the data with ttreat_value
  plots_list[[new_name]] <- data
  
  rm(data)
}


## NAMING AND TRANSFORMING VARIABLES

for (i in seq_along(plots_list)) {
  item <- plots_list[[i]]
  colnames(item) <- c("n", "date_time0", "time_zone", "t_ground", "t_bottom",
                      "t_top", "soil_moisture", "a", "b", "c", "OTC")
  
  plots_list[[i]] <- item %>% 
    select(date_time0, t_ground, t_bottom, t_top, soil_moisture, OTC) %>% 
    mutate(
      date_time = lubridate::parse_date_time(stringr::str_replace(date_time0, "\\.", "/"),
                                               orders = "%Y/%m/%d %H:%M")
    ) %>% 
    mutate(
      date = format(as.Date(date_time), "%Y/%m/%d"),
      time = format(as.POSIXct(date_time), "%H:%M"),
      hour = as.numeric(format(as.POSIXct(date_time), "%H"))
    ) %>% 
    mutate(
      year = year(date),
      month = month(date, label = TRUE), 
      day = day(date)
      
    ) %>% 
    select(-date_time0) %>% 
    filter(date >= "2023/01/01") %>% # Filtering data from this date
    mutate(plot = names(plots_list)[i]) %>% 
    mutate(plot_type = gsub("[0-9]", "",plot)) %>% 
    
    # transforming soil_moisture in 
    # volumetric water content(%). Kopecký et al. 2021:
    # Topographic Wetness Index calculation guidelines based on measured soil
    #  moisture and plant species composition. Suplemetary materials, Apendix A
    mutate(
      vwc = (-0.0000000134 * soil_moisture^2 + 0.000249622 * soil_moisture - 0.157889) * 100
           ) %>% 
    mutate(
      vwc = ifelse(vwc < 0, 0, vwc) # There are some data of vwc that gows below 0 and that is not possible. 
    )
    
}



## Arranging data
# ~ 30 secs

control_list <- c(plots_list["c2"],  plots_list["p3"], plots_list["p6"], plots_list["c7"]
                 , plots_list["p10"], plots_list["c11"], plots_list["c14"], plots_list["p15"])

controls <- do.call(rbind, control_list)

OTC_list <- c(plots_list["w1"],  plots_list["wp4"], plots_list["wp5"], plots_list["w8"]
            , plots_list["w9"], plots_list["wp12"], plots_list["wp13"], plots_list["w16"])

OTCs <- do.call(rbind, OTC_list)


data <- merge(controls, OTCs, all = TRUE) %>% 
  mutate(date = date(date), 
         OTC = as.factor(OTC))

data <- data %>% 
  mutate(
    OTC_label = ifelse(OTC == "YES", paste0("otc"), paste0("control"))
  ) %>% 
  mutate(
    OTC_label = as.factor( OTC_label)
  ) %>% 
  select(plot, OTC_label, date, month, time, hour, starts_with("t_"), vwc, soil_moisture )

data_long <- data %>% 
  pivot_longer(
    cols      = -c(time, plot, OTC_label, date, month, hour),
    names_to  = "variable",
    values_to = "value"
  )
  
  

}


## SENSOR DATA FOR DAYS OF SAMPLINGS - for arkaute.csv ##

sampling_dates <- read.csv("data/sampling_dates.csv") %>% 
  mutate(sampling = as.factor(sampling),
         date = ymd(date), 
         month = month(date, label = TRUE),
         day = day(date), 
         year = year(date)) %>% 
  select(sampling, date, day, month, year, one_month_window, omw_date) %>% 
  mutate(across(where(is.character), as.factor))

sampling_dates_vector <- sampling_dates$date

sampling_days_data <- data %>% 
  filter(date %in% sampling_dates_vector) %>% 
  select(t_top, vwc, date, time, plot)  

sampling_days_data$treatment <- gsub("[0-9]", "", as.character(sampling_days_data$plot))
sampling_days_data$plot <- gsub("[^0-9]", "", as.character(sampling_days_data$plot))

sampling_days_data <- sampling_days_data %>% 
  mutate(treatment = as.factor(treatment), 
         plot = as.factor(plot)) %>% 
  group_by(date, plot, treatment) %>% 
  summarize(mean_temperature = mean(t_top), 
            mean_vwc = mean(vwc))

#sampling_days_data %>%  write.csv("data/temp_vwc_data.csv",  row.names = F)

 
## VISUALIZATION OF ALL PLOTS: it takes time


#all_plots %>% 
#  ggplot(aes(x = date_time)) + 
#  facet_wrap(~plot, nrow = 4, ncol = 4, scales = "free") +
#  geom_line(aes(y = t_top), group = 1, color = "darkred") +
#  geom_line(aes(y = t_bottom), group = 1, color = "blue3") +
#  geom_line(aes(y = t_ground), group = 1, color = "green4") +
#  geom_line(aes(y = vwc * 100), group = 1, color = "black") +
#  geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
#  scale_y_continuous(name = "Temperature (°C)", sec.axis = sec_axis(~. *1, name = "Volumetric water content (%)")) +
#  theme_bw() +
#  labs(x = " ", y = "Temperature (°C)") +
#  ggtitle(as.character(item$plot)) +
#  theme(
#    plot.title = element_text(color = "black", size = 12, face = "bold.italic"),
#    axis.title.x = element_text(color = "black", size = 12, face = "bold"),
#    axis.title.y = element_text(color = "#993333", size = 12, face = "bold"),
#    axis.title.y.right = element_text(color = "black", size = 12, face = "bold")
#  )



### 2. OTC EFFECT  ############

# Charging palettes and themes for plots

source("code/palettes_labels.R")





# MEAN DIFFERENCE

daylight_hours <- c(7:19)
growth_season <- c("Apr", "May", "Jun", "Jul", "Aug", "Sep")

data <- data %>% 
  mutate(
    data = "All year - 24h"
  )

data_daylight  <- data %>% 
  filter(hour %in% daylight_hours) %>%   
  mutate(
    data = "All year - daylight"
  )

data_growth <- data %>% 
  filter(month %in% growth_season) %>% 
  mutate(
    data = "Growth season - 24h"
  )

data_growth_daylight <- data %>% 
  filter(month %in% growth_season) %>% 
  filter(hour %in% daylight_hours) %>% 
  mutate(
    data = "Growth season - daylight"
  )

{data_list <- list()
data_list[["data"]] <- data
data_list[["data_daylight"]] <- data_daylight
data_list[["data_growth"]] <- data_growth
data_list[["data_growth_daylight"]] <- data_growth_daylight


df_mean_difference <- data.frame(
  data       = rep(NA, 4),
  mean_diff  = rep(NA, 4),
  IC_95      = rep(NA, 4),
  mean_value_OTC = rep(NA, 4),
  sd_value_OTC   = rep(NA, 4),
  mean_value_control   = rep(NA, 4),
  sd_value_control   = rep(NA, 4),
  stringsAsFactors = FALSE
)

counter = 0 

for ( i in 1:4) {
  

  anova_result <- aov(t_top ~ OTC_label, data = data_list[[i]])
  summary(anova_result)
  a <- TukeyHSD(anova_result)
  
  counter = counter + 1
  
  df_mean_difference$data[counter] <- unique(data_list[[i]]$data)
  
  df_mean_difference$mean_diff[counter] <- round(a$OTC_label[1], 2)
  df_mean_difference$IC_95[counter] <- paste0(round(a$OTC_label[2], 2), "-", round(a$OTC_label[3],2))
  
  df_mean_difference$mean_value_OTC[counter] <- mean(data_list[[i]]$t_top[which(data_list[[i]]$OTC_label == "otc")])
  df_mean_difference$sd_value_OTC[counter] <- sd(data_list[[i]]$t_top[which(data_list[[i]]$OTC_label == "otc")])
  
  df_mean_difference$mean_value_control[counter] <-mean(data_list[[i]]$t_top[which(data_list[[i]]$OTC_label == "control")])
  df_mean_difference$sd_value_control[counter] <-sd(data_list[[i]]$t_top[which(data_list[[i]]$OTC_label == "control")])
  
  #kruskal.test(t_top ~ OTC_label, data = data_list[[i]])
  #dunn.test::dunn.test(data_list[[i]]$t_top, data_list[[i]]$OTC_label, method = "bonferroni")
  
}
}

#df_mean_difference %>%  write.csv("results/OTC_effect.csv")


metadata <- do.call(rbind, data_list)

gg_OTC_temperature <- 
ggboxplot(metadata,
          x       = "OTC_label", 
          y       = "t_top",            # cambiar aquí la variable
          fill    = "OTC_label",
          width   = 0.6) +
  facet_wrap(~ data, ncol = 4) +
  stat_compare_means(
    method       = "t.test",
    label        = "p.signif",
    label.y      = max(metadata$t_top) * 1.05,
    comparisons  = list(c("control", "otc"))
  ) +
  scale_x_discrete(name = NULL) +
  ylab("Temperature (ºC)") +
  
  scale_fill_manual( 
    name = NULL,
    values = palette_OTC,
    labels = labels_OTC
  ) +
  
theme2

print(gg_OTC_temperature)


#ggsave("results/Plots/protofinal/OTC_effect_temperature.png", plot = gg_OTC_temperature, dpi = 300)


gg_allvariables_sensor <- 
data_long %>% 
 # filter(variable == variables[i]) %>% 
  ggboxplot(
            x       = "OTC_label", 
            y       = "value",            
            fill    = "OTC_label",
            width   = 0.6) +
  facet_wrap(~ variable, ncol = 4, nrow = 1,
             scales = "free_y",
             labeller = labeller(variable = as_labeller(labels_variables_sensor))) +
 
  stat_compare_means(
    method       = "t.test",
    label        = "p.signif",
    #label.y      = max(data_long$value) * 1.05,
    comparisons  = list(c("control", "otc"))
  ) +
  scale_x_discrete(name = NULL) +
  
  scale_fill_manual( 
    name = NULL,
    values = palette_OTC,
    labels = labels_OTC
  ) +
  
  theme2 +
  labs (y = NULL)

print(gg_allvariables_sensor)

#ggsave("results/Plots/protofinal/OTC_effect_allvariables.png", plot = gg_allvariables_sensor, dpi = 300)





### HOW DIFFERENT VARIABLES EVOLVE? 

variables = c("t_top", "t_ground", "t_bottom", "vwc")
                # 1        # 2        # 3      # 4

{
  
  
i = 1

ylabels <- c(
  t_top     = "Temperature at 40 cm (ºC)",
  t_ground  = "Temperature at 2 cm (ºC)",
  t_bottom  = "Temperature at -6 cm (ºC)",
  vwc       = "VWC (%)"
)

ytitle      <- ylabels[variables[i]]



gg_boxplot_alldata <- 
  data_long %>% 
  filter(variable == variables[i]) %>% 

  ggboxplot(
    x       = "OTC_label", 
    y       = "value",            
    fill    = "OTC_label",
    width   = 0.6,
    ggtheme = theme2
  ) +
  stat_compare_means(
    method      = "t.test",
    label       = "p.signif",
    comparisons = list(c("control", "otc"))
    )+
  scale_x_discrete(
    name   = NULL,
    labels = labels_OTC
  ) +
  
  scale_fill_manual(
    name   = NULL,
    values = palette_OTC,
    guide  = FALSE
  ) +
  theme(legend.position = "none") +
  labs(y = ytitle, x = NULL)



  gg_boxplot_daily_average <- 
    data_long %>% 
    group_by(date, OTC_label, variable) %>% 
    summarise(
      mean_value = round(mean(value, na.rm = TRUE), 4),
      sd_value   = round(sd(value,   na.rm = TRUE), 4)
    ) %>% 
    filter(variable == variables[i]) %>% 
    
    ggboxplot(
      x       = "OTC_label", 
      y       = "mean_value",            
      fill    = "OTC_label",
      width   = 0.6,
      ggtheme = theme2
    ) +
    stat_compare_means(
      method      = "t.test",
      label       = "p.signif",
      comparisons = list(c("control", "otc"))
    ) +
    scale_x_discrete(
      name   = NULL      # <— aquí aplicas tus etiquetas “Without OTC” / “With OTC”
    ) +
    scale_fill_manual(          # Esto sigue afectando sólo a la leyenda (si la tuvieras)
      name   = NULL,
      values = palette_OTC,
      labels = labels_OTC
    ) +
    theme(legend.position = "none") +
    labs(y = ytitle, x = NULL)
  
  

# DIFFERENCE THROUGH TIME: ALL 652 DAYS MEASURED


gg_allyear <- 
  data_long %>% 
  group_by(date, OTC_label, variable) %>% 
  summarise(mean_value = round(mean(value, na.rm = T), 4),
            sd_value = round(sd(value, na.rm = T), 4)
  )%>% 
  filter(variable == variables[i]) %>% 
  
  ggplot(aes(x = date, y = mean_value, color = OTC_label, fill = OTC_label)) +
  
  # facet_wrap(~ variable, ncol = 4, nrow = 1, 
  #            scales = "free_y",
  #            labeller = labeller(variable = as_labeller(labels_variables_sensor))) +
  
  geom_line() +
  
  #geom_smooth(stat = "smooth", alpha = 0.2) +
  
  scale_color_manual( 
    name = NULL, values = palette_OTC, labels = labels_OTC) +
  
  scale_fill_manual(
    name = NULL, values = palette_OTC, labels = labels_OTC) +
  
  labs ( x = NULL, y = ytitle ) +
  
  theme3 +
  
  theme(legend.position = "none")




# DIFFERENCE THROUGH TIME: 24 H

n <- as.numeric(as.Date(max(data$date)) - as.Date(min(data$date)))
# 662 days we sampled

gg_24h_diff <- 
  data %>% 
    group_by(time, OTC_label) %>% 
    summarise(t_top_mean = round(mean(t_top, na.rm = T), 2),
              t_top_sd = round(sd(t_top, na.rm = T), 2), 
              t_bottom_mean = round(mean (t_bottom, na.rm = T), 2),
              t_bottom_sd = round(sd(t_bottom, na.rm = T), 2), 
              t_ground_mean = round(mean(t_ground, na.rm = T), 2),
              t_ground_sd = round(sd(t_ground, na.rm = T), 2),
              vwc_mean = round(mean(vwc, na.rm = T), 6),
              vwc_sd = round(sd(vwc, na.rm = T), 6)
    ) %>%
    pivot_wider(names_from = OTC_label,
                values_from = c(t_top_mean, t_top_sd, t_bottom_mean, t_bottom_sd,
                                t_ground_mean, t_ground_sd, vwc_mean, vwc_sd),
                names_prefix = "") %>%
    select(time, starts_with("t_"), starts_with("vwc_")) %>% 
    mutate(
      t_top_mean_diff = t_top_mean_otc - t_top_mean_control,
      t_top_sd_diff = sqrt((t_top_sd_otc^2 / n) + (t_top_sd_control^2 / n)),  ## equation to calculate SD differences 
      t_bottom_mean_diff = t_bottom_mean_otc - t_bottom_mean_control,
      t_bottom_sd_diff = sqrt((t_bottom_sd_otc^2 / n) + (t_bottom_sd_control^2 / n)),
      t_ground_mean_diff = t_ground_mean_otc - t_ground_mean_control,
      t_ground_sd_diff = sqrt((t_ground_sd_otc^2 / n) + (t_ground_sd_control^2 / n)),
      vwc_mean_diff =  vwc_mean_otc - vwc_mean_control,
      vwc_sd_diff =  sqrt((vwc_sd_otc^2 / n) + (vwc_sd_control^2 / n))
    ) %>% 
    mutate(
      data = names(data_list)[i]
    ) %>% 
    select(ends_with("_diff"), time) %>% 
    pivot_longer(
      cols      = -time,
      names_to  = c("variable", ".value"),
      names_pattern = "(.*)_(mean|sd)_diff"
    ) %>% 
    # renombramos las dos columnas que crea automáticamente: mean y sd
    rename(
      mean_diff_value = mean,
      sd_diff_value   = sd
    ) %>% 
filter(variable == variables[i]) %>%                                       #### Modify in this line the variable or variables we want to see
  ggplot(aes(x = time, y = mean_diff_value, color = variable)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray20") +
  geom_line(aes(group = variable), size = 0.5, color = "#EA6E13") +
  geom_line(aes(x = time,
                y = mean_diff_value + sd_diff_value,
                group = variable) , linetype = "dashed", size = 0.5, color = "#EA6E13") +
  geom_line(aes(x = time,
                y = mean_diff_value - sd_diff_value,
                group = variable) , linetype = "dashed", size = 0.5, color = "#EA6E13") + 
  scale_x_discrete(breaks = sprintf("%02d:00", c(1, 5, 9, 13, 17, 21))) +
    
  #scale_color_manual(values = palette_sensor) +
    
    
  labs( x = NULL,
        y = ytitle
        #y = "Temperature difference inside OTCs (ºC)"
        ) +
  
  theme3 +
  theme(legend.position = "none")





gg_year_diff <- 
  data %>% 
  group_by(date, OTC_label) %>% 
  summarise(
    t_top_mean     = round(mean(t_top,     na.rm = TRUE), 2),
    t_bottom_mean  = round(mean(t_bottom,  na.rm = TRUE), 2),
    t_ground_mean  = round(mean(t_ground,  na.rm = TRUE), 2),
    vwc_mean       = round(mean(vwc,       na.rm = TRUE), 6),
    .groups = "drop"
  ) %>%
  
  # Pasamos a formato ancho para tener control/otc en columnas separadas
  pivot_wider(
    names_from   = OTC_label,
    values_from  = c(t_top_mean, t_bottom_mean, t_ground_mean, vwc_mean)
  ) %>%
  
  # Calculamos sólo las diferencias de medias
  mutate(
    t_top_diff     = t_top_mean_otc    - t_top_mean_control,
    t_bottom_diff  = t_bottom_mean_otc - t_bottom_mean_control,
    t_ground_diff  = t_ground_mean_otc - t_ground_mean_control,
    vwc_diff       = vwc_mean_otc      - vwc_mean_control
  ) %>%
  
  # Seleccionamos sólo las columnas de diferencia + fecha
  select(date, ends_with("_diff")) %>%
  
  # Volvemos a formato largo para ggplot
  pivot_longer(
    cols       = -date,
    names_to   = "variable",
    values_to  = "mean_diff_value",
    names_pattern = "(.*)_diff"
  ) %>% 
  filter(variable == variables[i]) %>%   # elegimos la variable activa en el loop
  
  # Plot
  ggplot(aes(x = date, y = mean_diff_value)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray20") +
  geom_line(color = "#EA6E13", size = 0.5) +
  labs(
    x = NULL,
    y = paste0(ytitle)
  ) +
  
  scale_x_date(
    date_breaks  = "3 month",            # intervalos de 1 mes
    date_labels  = "%Y-%m-%d",              # ej. "Jan 2024"
    expand       = expansion(add = c(0, 0))  # ajusta márgenes si hace falta
  ) +
  
  
  theme3 +
  theme(
    legend.position = "none",
    #axis.text.x = element_text(angle = 45, hjust = 1)
    )

   
}


## !!! cambiar nombre de los plots que guardes

  print(gg_boxplot_alldata)
  #ggsave("results/Plots/protofinal/OTC_effect_VWC_alldata.png", plot = gg_boxplot_alldata, dpi = 300)

  
  print(gg_boxplot_daily_average)
  #ggsave("results/Plots/protofinal/OTC_effect_variable_daily_average.png", plot = gg_boxplot_daily_average, dpi = 300)
  
  print(gg_allyear)
  #ggsave("results/Plots/protofinal/OTC_effect_allyear.png", plot = gg_allyear, dpi = 300)
  
  
  print(gg_24h_diff)
  #ggsave("results/Plots/protofinal/OTC_effect_24h_ttop.png", plot = gg_24h_diff, dpi = 300)   
  
  
  
  print(gg_year_diff)
  ggsave("results/Plots/protofinal/OTC_effect_year_ttop.png", plot = gg_year_diff, dpi = 300) 
  
  
  
  
  
  
## HOW TEMPERATURE AND SOIL MOISTURE RELATES?  


vwc_data <- 
  
  data %>% 
  group_by(date, OTC_label) %>% 
  summarise(t_top_mean = round(mean(t_top, na.rm = T), 2),
            vwc_mean = round(mean(vwc, na.rm = T), 6),
            soil_moisture_mean = round(mean(soil_moisture, na.rm = T), 6))



vwc_data %>% 
  ggplot(aes(y = vwc_mean, x = soil_moisture_mean)) + 
  geom_point(size = 0.5) + 
  labs ( x = "Soil moisture TMS-4 raw signal", y = "VWC (%)") +
  geom_smooth(method = "lm", se = FALSE) +
  
  # primero la ecuación
  stat_regline_equation(
    mapping     = aes(label = after_stat(eq.label)),
    formula     = y ~ x,
    label.x.npc = 0.2,
    label.y.npc = 0.95,
    size        = 5,
    show.legend = FALSE
  ) +
  # luego el R²
  stat_regline_equation(
    mapping     = aes(label = after_stat(rr.label)),
    formula     = y ~ x,
    label.x.npc = 0.2,
    label.y.npc = 0.80,
    size        = 5,
    show.legend = FALSE
  ) +
  
  stat_cor(
    mapping     = aes(label = after_stat(p.label)),
    method      = "pearson",      # test de correlación Pearson
    label.x.npc = 0.2,           # misma X para alinear
    label.y.npc = 0.60,           # un poco más abajo
    size        = 5,
    show.legend = FALSE
  ) +
  
  theme3







gg_vwc_vs_t <- 
  vwc_data %>% 
  ggplot(aes(y = vwc_mean, x = t_top_mean, color = OTC_label, fill = OTC_label)) + 
  geom_point() + 
  scale_color_manual( 
    name = NULL,
    values = palette_OTC,
    labels = labels_OTC
  ) +
  scale_fill_manual(
    name = NULL, 
    values = palette_OTC,
    labels = labels_OTC
  ) +
  labs ( x = "Temperature at 40 cm (ºC)", y = "VWC (%)") +
  geom_smooth(method = "lm", se = FALSE) +
  
  # primero la ecuación
  stat_regline_equation(
    mapping     = aes(label = after_stat(eq.label)),
    formula     = y ~ x,
    label.x.npc = 0.8,
    label.y.npc = 0.95,
    size        = 5,
    show.legend = FALSE
  ) +
  # luego el R²
  stat_regline_equation(
    mapping     = aes(label = after_stat(rr.label)),
    formula     = y ~ x,
    label.x.npc = 0.8,
    label.y.npc = 0.80,
    size        = 5,
    show.legend = FALSE
  ) +
  
  stat_cor(
    mapping     = aes(label = after_stat(p.label)),
    method      = "pearson",      # test de correlación Pearson
    label.x.npc = 0.8,           # misma X para alinear
    label.y.npc = 0.60,           # un poco más abajo
    size        = 5,
    show.legend = FALSE
  ) +
  
  theme3

  print(gg_vwc_vs_t)
  ggsave("results/Plots/protofinal/vwc_vs_t.png", plot = gg_vwc_vs_t, dpi = 300)




# Statistics on VWC
  hist(data$vwc)
  anova_result <- aov(vwc ~ OTC_label, data)
  summary(anova_result)
  TukeyHSD(anova_result)











































