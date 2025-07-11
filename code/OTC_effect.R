


rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)


pacman::p_load(dplyr, reshape2,tidyverse, lubridate, ggplot2, ggpubr, gridExtra, stringr, readr)

theme_set(theme_bw() +
            theme(
              legend.position = "right",
              panel.grid = element_blank(),
              strip.background = element_blank(),
              strip.text = element_text(face = "bold"),
              text = element_text(size = 11)))


########### 1. OPENING DATA ######################
# ! it takes time !

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
  colnames(item) <- c("n", "date_time0", "time_zone", "T_ground", "T_bottom",
                      "T_top", "soil_moisture", "a", "b", "c", "OTC")
  
  plots_list[[i]] <- item %>% 
    select(date_time0, T_ground, T_bottom, T_top, soil_moisture, OTC) %>% 
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
    
    mutate(vwc =-0.0000000134 * soil_moisture^2 + 0.000249622 * soil_moisture - 0.157889) # transforming soil_moisture in 
  
  # volumetric water content(%). Kopecký et al. 2021:
  # Topographic Wetness Index calculation guidelines based on measured soil
  #  moisture and plant species composition. Suplemetary materials, Apendix A. 
}



## Arranging data

control_list <- c(plots_list["c2"],  plots_list["p3"], plots_list["p6"], plots_list["c7"]
                 , plots_list["p10"], plots_list["c11"], plots_list["c14"], plots_list["p15"])

controls <- do.call(rbind, control_list)

OTC_list <- c(plots_list["w1"],  plots_list["wp4"], plots_list["wp5"], plots_list["w8"]
            , plots_list["w9"], plots_list["wp12"], plots_list["wp13"], plots_list["w16"])

OTCs <- do.call(rbind, OTC_list)


all_plots <- merge(controls, OTCs, all = TRUE) %>% 
  mutate(date = date(date), 
         OTC = as.factor(OTC))

all_plots <- all_plots %>% 
  mutate(
    OTC_label = ifelse(OTC == "YES", paste0("otc"), paste0("control"))
  ) %>% 
  mutate(
    OTC_label = as.factor( OTC_label)
  )
  

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

sampling_days_data <- all_plots %>% 
  filter(date %in% sampling_dates_vector) %>% 
  select(T_top, vwc, date, time, plot)  

sampling_days_data$treatment <- gsub("[0-9]", "", as.character(sampling_days_data$plot))
sampling_days_data$plot <- gsub("[^0-9]", "", as.character(sampling_days_data$plot))

sampling_days_data <- sampling_days_data %>% 
  mutate(treatment = as.factor(treatment), 
         plot = as.factor(plot)) %>% 
  group_by(date, plot, treatment) %>% 
  summarize(mean_temperature = mean(T_top), 
            mean_vwc = mean(vwc))

#sampling_days_data %>%  write.csv("data/temp_vwc_data.csv",  row.names = F)

 
## VISUALIZATION OF ALL PLOTS: it takes time


#all_plots %>% 
#  ggplot(aes(x = date_time)) + 
#  facet_wrap(~plot, nrow = 4, ncol = 4, scales = "free") +
#  geom_line(aes(y = T_top), group = 1, color = "darkred") +
#  geom_line(aes(y = T_bottom), group = 1, color = "blue3") +
#  geom_line(aes(y = T_ground), group = 1, color = "green4") +
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


palette <- c("t_top" = "red", "t_bottom" = "purple", "t_ground" = "orange", "vwc" = "blue3")

data <- all_plots

data_daylight  <- all_plots %>% 
  filter(hour %in% c(8:20))

data_growth <- all_plots %>% 
  filter(month %in% c("Apr", "May", "Jun", "Jul", "Aug", "Sep"))

data_growth_daylight <- all_plots %>% 
  filter(month %in% c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")) %>% 
  filter(hour %in% c(8:20))

data_list <- list()
data_list[["data"]] <- data
data_list[["data_daylight"]] <- data_daylight
data_list[["data_growth"]] <- data_growth
data_list[["data_growth_daylight"]] <- data_growth_daylight


i = 2
ggboxplot(data_list[[i]],
  x     = "OTC_label", 
  y     = "T_top", 
  fill  = "OTC_label",
  palette = c("control" = "#48A597", "otc" = "#D94E47"),
  add   = FALSE,       # opcional: añade puntos dispersos
  width = 0.6             # ancho de las cajas
) +
  stat_compare_means(
    method    = "t.test",
    label     = "p.signif",    # o "p.format" para p = 0.012
    label.y   = max(data_list[[i]]$T_top) * 1.05,
    comparisons = list(c("control", "otc"))) +
  theme(
    legend.position = "none",
    labs(x = "OTC", y = "Temperature (ºC)")
    #title ="asdsad"
  )






# DIFFERENCE IN 24 H

n <- as.numeric(as.Date(max(all_plots$date)) - as.Date(min(all_plots$date)))
# 662 days we sampled


  data %>% 
    group_by(time, OTC_label) %>% 
    summarise(t_top_mean = round(mean(T_top, na.rm = T), 2),
              t_top_sd = round(sd(T_top, na.rm = T), 2), 
              t_bottom_mean = round(mean (T_bottom, na.rm = T), 2),
              t_bottom_sd = round(sd(T_bottom, na.rm = T), 2), 
              t_ground_mean = round(mean(T_ground, na.rm = T), 2),
              t_ground_sd = round(sd(T_ground, na.rm = T), 2),
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
filter(variable == "t_top") %>% 
  ggplot(aes(x = time, y = mean_diff_value, color = variable)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray20") +
  geom_line(aes(group = variable)) +
  geom_line(aes(x = time,
                y = mean_diff_value + sd_diff_value,
                group = variable) , linetype = "dashed") +
  geom_line(aes(x = time,
                y = mean_diff_value - sd_diff_value,
                group = variable) , linetype = "dashed") + 
  scale_x_discrete(breaks = sprintf("%02d:00", c(1, 5, 9, 13, 17, 21))) +
  scale_color_manual(values = palette) + 
  labs( x = NULL, y = "Temperature difference inside OTCs (ºC)") +
  theme(
    legend.position = "none"
  )


                                    


























































