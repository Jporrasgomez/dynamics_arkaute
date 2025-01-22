
##TO DO's :------------------
#T ground tiene aguos datos missing EN ggtdailtdiff
#No hay datos de plot 10
#Mirar qué pasa con los histogramas
#Mirar qué pasa con los boxplot
#ver cómo influye soil_moisture en los datos de temperatura. Hay que calibrar los datos de soil moisture con la información de textura del suelo. 
#Añadir un cálculo más para la diferencia de temperatura
#Hacer esto con R-markdown así puedo generar pdfs rápido con los gráficos

#Comments----------------
#Hay anomalías en las mediciones. Debería decir a R que me busque datos en los que haya una diferencia superior a 8 grados entre el dato y la medida anterior

#cambie
#Anomalías detectadas: c1 (2022.06.21 12:45, t:ground) de 48ºC  // c2 (2022.06.21 12:45, t_ground) de 60ºC, SON LAS DOS A LA MISMA HORA Y DIA!
#De momento, utilizo los datos a partir del día 22 para evitar esas anomalías. 

#Hay anomalias en soil_moisture los días 6 de julio y 11 de agosto (precipitación). Hay que probar a enterrar mejor el sensor. Y SIno, 
#eliminar los datos en los que haya una fierencia mayor a 500 unidades (o algo así) entre mediciones consecutivas. 

#En los datos, no sé si está bien comparar los datos de las 24h de los plots (medias de las medias) con los datos de los puntos
#Incluir en los datos, las medias para all otcs y all controls


rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)

#Packages-----------------
library(lubridate)
library(stringr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(readr)
library(ggpubr)
library(tidyverse)
library(gridExtra)
source("code/tools/basicFun.R")




# Organising sensors/plots names --------------
plots <- read.csv("data/plots.csv")
#IMPORTANT! Change the value of the DATE everytime a new set of data is opened.It depends on the day you took the data from the sensors. 
plots$file_code <- paste0("data_", plots$sensor_code, "_2024_04_30_0.csv")




# Opening data--------------
file_code_values <- plots$file_code

plots_list <- list()

for (i in seq_along(file_code_values)) {
  file_path <- file.path("data/data_sensors", file_code_values[i])
  new_name <- plots$plot_code[i]
  ttreat_value <- plots$ttreat[i]  # Get ttreat value for the current plot_code
  data <- read_delim(file_path, 
                     ";", escape_double = FALSE, col_names = FALSE, 
                     trim_ws = TRUE)
  data$ttreat <- ttreat_value  # Add a new variable "ttreat" to the data with ttreat_value
  plots_list[[new_name]] <- data
}

rm(data)

#View(plots_list[[1]])

# Naming data and changing variables--------------------
#Elegir la fecha a partir de la cual quiero filtrar los datos en "item <- subset(item, datenew >= "2023/01/01")"

for (i in seq_along(plots_list)) {
  item <- plots_list[[i]]
  colnames(item) <- c("n", "date_time", "time_zone", "T_ground", "T_bottom", "T_top", "soil_moisture", "a", "b", "c", "ttreat")
  item <- item %>% select(date_time, T_ground, T_bottom, T_top, soil_moisture, ttreat)
  
  item$datetimenew <- 
    lubridate::parse_date_time(stringr::str_replace(item$date_time, "\\.", "/"), orders = "%Y/%m/%d %H:%M")
  item$datetimenew <- item$datetimenew + 3600  #Adding 1 hour because sensors come with 1 hour difference, 2 for summer. So it will still be 1 hour difference for summer but none for winter. 
 
  item$date <- format(as.Date(item$datetimenew), "%Y/%m/%d")
  item$year <- year(item$date)
  item$month <- month(item$date, label = TRUE)
  item$day <- day(item$date)
  item$time <- format(as.POSIXct(item$datetimenew), "%H:%M")
  
  item <- subset(item, date >= "2023/01/01")
  
  item$date_time <- NULL
  
  item$plot <- names(plots_list)[i]
  item$plot_type <- gsub("[0-9]", "", item$plot)
  
  plots_list[[i]] <- item
}

rm(item)

#View(plots_list[[2]])
#View(plots_list[[4]])


# Data characteristics and histograms----------------
#It seems that T_ground do not have a normal distribution of data.

for (i in seq_along(plots_list)) {
  x <- plots_list[[i]]
  summary(x)
  str(x)
}

rm(x)

par(mfrow = c(4, 4))

for (i in seq_along(plots_list)) {
  x <- plots_list[[i]]
  hist(x$T_ground, main = names(plots_list)[i])
}

for (i in seq_along(plots_list)) {
  x <- plots_list[[i]]
  hist(x$T_bottom, main = names(plots_list)[i])
}

for (i in seq_along(plots_list)) {
  x <- plots_list[[i]]
  hist(x$T_top, main = names(plots_list)[i])
}

for (i in seq_along(plots_list)) {
  x <- plots_list[[i]]
  hist(x$soil_moisture, main = names(plots_list)[i])
}





# ALL DATA ARRANGE--------

control_list <- c(plots_list["c2"],  plots_list["p3"], plots_list["p6"], plots_list["c7"]
                  , plots_list["p10"], plots_list["c11"], plots_list["c14"], plots_list["p15"])
controls<- data.frame()

for (i in seq_along(control_list)) {
  x <- control_list[[i]]
  controls <- rbind(controls, x)
}


w_list <- c(plots_list["w1"],  plots_list["wp4"], plots_list["wp5"], plots_list["w8"]
              , plots_list["w9"], plots_list["wp12"], plots_list["wp13"], plots_list["w16"])
ws<- data.frame()

for (i in seq_along(w_list)) {
  x <- w_list[[i]]
  ws <- rbind(ws, x)
}

all_plots <- merge(controls, ws, all = TRUE)
all_plots$ttreat <- as.factor(all_plots$ttreat)


all_plots_sum <- subset(all_plots, month %in% c("Apr", "May", "Jun", "Jul", "Aug", "Sep"))

# Visualization individual plots------------


#Temperature and soil moisture

dailygraph_list <- list()
dailygraph_list <- lapply(plots_list, function(item) {
  ggplot(item, aes(x = datetimenew)) +
    geom_line(aes(y = T_top), group = 1, color = "darkred") +
    geom_line(aes(y = T_bottom), group = 1, color = "blue") +
    geom_line(aes(y = T_ground), group = 1, color = "green") +
    geom_line(aes(y = soil_moisture / 40), group = 1, color = "black") +
    scale_y_continuous(sec.axis = sec_axis(~. * 40)) +
    theme_bw() +
    labs(x = NULL, y = NULL) +
    ggtitle(as.character(item$plot)) +
    theme(
      plot.title = element_text(color = "black", size = 12, face = "bold.italic"))
})

# AVISO: tarda mucho en cargar!!
grid.arrange(grobs = dailygraph_list, ncol = 4)


#Temperature with mean values
dailygraph_temp_list <- list()

for (item in plots_list) {
  mean_T_top <- round(mean(item$T_top), 2)
  mean_T_bottom <- round(mean(item$T_bottom), 2)
  mean_T_ground <- round(mean(item$T_ground), 2)
  
  se_T_top <- round(sd(item$T_top) / sqrt(length(item$T_top)), 2)
  se_T_bottom <- round(sd(item$T_bottom) / sqrt(length(item$T_bottom)), 2)
  se_T_ground <- round(sd(item$T_ground) / sqrt(length(item$T_ground)), 2)
  
  x_title <- bquote(atop(bold("Mean temp top:") ~ .(mean_T_top) ~ ("±") ~ .(se_T_top),
                         atop(bold("Mean temp bottom:") ~ .(mean_T_bottom) ~ ("±") ~ .(se_T_bottom),
                              bold("Mean temp ground:") ~ .(mean_T_ground) ~ ("±") ~ .(se_T_ground))))
  
  
  ggitem <- ggplot(item, aes(x = datetimenew)) +
    geom_line(aes(y = T_top), group = 1, color = "darkred") +
    geom_line(aes(y = T_bottom), group = 1, color = "blue") +
    geom_line(aes(y = T_ground), group = 1, color = "green") +
    theme_bw() +
    labs(x = NULL, y = NULL) +
    ggtitle(as.character(item$plot)) +
    theme(
      plot.title = element_text(color = "black", size = 12, face = "bold.italic"))
  ggitem <- ggitem + labs(x = x_title)
  

  dailygraph_temp_list[[length(dailygraph_temp_list) + 1]] <- ggitem
}

# AVISO: tarda mucho en cargar!!
#grid.arrange(grobs = dailygraph_temp_list, nrow = 4, ncol = 4)



#Soil moisture
dailygraph_sm_list <- list()
dailygraph_sm_list <- lapply(plots_list, function(item) {
  ggplot(item, aes(x = datetimenew)) + 
    geom_line((aes (y = soil_moisture)), group = 1, color = "black")+
    theme_bw()+
    labs(x = NULL, y =  NULL)+ 
    ggtitle(as.character(item$plot))+
    theme(plot.title = element_text(color="black", size=12, face="bold.italic"))
})

# AVISO: tarda mucho en cargar!
#grid.arrange(grobs = dailygraph_sm_list, ncol = 4)



#To see one plot by one

eachplot <- function(item) {
  graph <- ggplot(item, aes(x = datetimenew)) +
    geom_line(aes(y = T_top), group = 1, color = "darkred") +
    geom_line(aes(y = T_bottom), group = 1, color = "blue") +
    geom_line(aes(y = T_ground), group = 1, color = "green") +
    geom_line(aes(y = soil_moisture / 40), group = 1, color = "black") +
    scale_y_continuous(name = "Temperature (°C)", sec.axis = sec_axis(~. * 40, name = "Soil moisture")) +
    theme_bw() +
    labs(x = " ", y = "Temperature (°C)") +
    ggtitle(as.character(item$plot)) +
    theme(
      plot.title = element_text(color = "black", size = 12, face = "bold.italic"),
      axis.title.x = element_text(color = "black", size = 12, face = "bold"),
      axis.title.y = element_text(color = "#993333", size = 12, face = "bold"),
      axis.title.y.right = element_text(color = "black", size = 12, face = "bold")
    )
  
  print(graph)
  readline("Press 'start' to continue...")
  dev.off()
}

#Para correr el código:
lapply(plots_list, eachplot)



# CONTROL vs WARMING: difference in 24 h-------------------------------

# CONTROL vs WARMING: Temperature difference in 24 h



allplots_temp_24h_diff <-  summarise(group_by(all_plots, time, ttreat),
                               t_top_mean = round(mean(T_top, na.rm = T), 2),
                               t_top_sd = round(sd(T_top, na.rm = T), 2), 
                               t_bottom_mean = round(mean (T_bottom, na.rm = T), 2),
                               t_bottom_sd = round(sd(T_bottom, na.rm = T), 2), 
                               t_ground_mean = round(mean(T_ground, na.rm = T), 2),
                               t_ground_sd = round(sd(T_ground, na.rm = T), 2))

allplots_temp_24h_diff_sum <-  summarise(group_by(all_plots_sum, time, ttreat),
                                     t_top_mean = round(mean(T_top, na.rm = T), 2),
                                     t_top_sd = round(sd(T_top, na.rm = T), 2), 
                                     t_bottom_mean = round(mean (T_bottom, na.rm = T), 2),
                                     t_bottom_sd = round(sd(T_bottom, na.rm = T), 2), 
                                     t_ground_mean = round(mean(T_ground, na.rm = T), 2),
                                     t_ground_sd = round(sd(T_ground, na.rm = T), 2))


allplots_temp_24h_diff <- allplots_temp_24h_diff %>%
  pivot_wider(names_from = ttreat,
              values_from = c(t_top_mean, t_top_sd, t_bottom_mean, t_bottom_sd, t_ground_mean, t_ground_sd),
              names_prefix = "") %>%
  select(time, starts_with("t_"))

allplots_temp_24h_diff_sum <- allplots_temp_24h_diff_sum %>%
  pivot_wider(names_from = ttreat,
              values_from = c(t_top_mean, t_top_sd, t_bottom_mean, t_bottom_sd, t_ground_mean, t_ground_sd),
              names_prefix = "") %>%
  select(time, starts_with("t_"))


n <- as.numeric(as.Date(max(all_plots$date)) - as.Date(min(all_plots$date))) #n es sample size (numero de dias medidos en nuestro caso)


allplots_temp_24h_diff <- summarise(group_by(allplots_temp_24h_diff, time),
                                t_top_mean_diff = t_top_mean_w - t_top_mean_c,
                                t_top_sd_diff = sqrt((t_top_sd_w^2 / n) + (t_top_sd_c^2 / n)),  ## formula para calcular diferencias entre sd. 
                                t_bottom_mean_diff = t_bottom_mean_w - t_bottom_mean_c,
                                t_bottom_sd_diff = sqrt((t_bottom_sd_w^2 / n) + (t_bottom_sd_c^2 / n)),
                                t_ground_mean_diff = t_ground_mean_w - t_ground_mean_c,
                                t_ground_sd_diff = sqrt((t_ground_sd_w^2 / n) + (t_ground_sd_c^2 / n)))

allplots_temp_24h_diff_sum <- summarise(group_by(allplots_temp_24h_diff_sum, time),
                                    t_top_mean_diff = t_top_mean_w - t_top_mean_c,
                                    t_top_sd_diff = sqrt((t_top_sd_w^2 / n) + (t_top_sd_c^2 / n)),  ## formula para calcular diferencias entre sd. 
                                    t_bottom_mean_diff = t_bottom_mean_w - t_bottom_mean_c,
                                    t_bottom_sd_diff = sqrt((t_bottom_sd_w^2 / n) + (t_bottom_sd_c^2 / n)),
                                    t_ground_mean_diff = t_ground_mean_w - t_ground_mean_c,
                                    t_ground_sd_diff = sqrt((t_ground_sd_w^2 / n) + (t_ground_sd_c^2 / n)))

#allplots_temp_24h_diff <- allplots_temp_24h_diff %>% slice(1:(nrow(allplots_temp_24h_diff) - 1))

mean(allplots_temp_24h_diff$t_top_mean_diff, na.rm = T)
sd(allplots_temp_24h_diff$t_top_mean_diff, na.rm = T)

mean(allplots_temp_24h_diff$t_bottom_mean_diff, na.rm = T)
sd(allplots_temp_24h_diff$t_bottom_mean_diff, na.rm = T)

mean(allplots_temp_24h_diff$t_ground_mean_diff, na.rm = T)
sd(allplots_temp_24h_diff$t_ground_mean_diff, na.rm = T)

mean(allplots_temp_24h_diff_sum$t_top_mean_diff, na.rm = T)
sd(allplots_temp_24h_diff_sum$t_top_mean_diff, na.rm = T)

mean(allplots_temp_24h_diff_sum$t_bottom_mean_diff, na.rm = T)
sd(allplots_temp_24h_diff_sum$t_bottom_mean_diff, na.rm = T)

mean(allplots_temp_24h_diff_sum$t_ground_mean_diff, na.rm = T)
sd(allplots_temp_24h_diff_sum$t_ground_mean_diff, na.rm = T)


#gt24h_diff <- 
ggplot(allplots_temp_24h_diff) +
  geom_line(aes(x = time, y = t_top_mean_diff, group = 1, color = "Top"), linetype = "solid", linewidth = 1) +
  geom_line(aes(x = time, y = t_top_mean_diff + t_top_sd_diff, group = 1, color = "Top"), linetype = "dashed") +
  geom_line(aes(x = time, y = t_top_mean_diff - t_top_sd_diff, group = 1, color = "Top"), linetype = "dashed")+
  
  geom_line(aes(x = time, y = t_bottom_mean_diff, group = 1, color = "Bottom"), linetype = "solid", linewidth = 1) +
  geom_line(aes(x = time, y = t_bottom_mean_diff + t_bottom_sd_diff, group = 1, color = "Bottom"), linetype = "dashed") +
  geom_line(aes(x = time, y = t_bottom_mean_diff - t_bottom_sd_diff, group = 1, color = "Bottom"), linetype = "dashed")+
  
  geom_line(aes(x = time, y = t_ground_mean_diff, group = 1, color = "Ground"), linetype = "solid", linewidth = 1) +
  geom_line(aes(x = time, y = t_ground_mean_diff + t_ground_sd_diff, group = 1, color = "Ground"), linetype = "dashed") +
  geom_line(aes(x = time, y = t_ground_mean_diff - t_ground_sd_diff, group = 1, color = "Ground"), linetype = "dashed")+
  
  labs(x = "24 hours (January 2023 - May 2024)", y = "Temperature difference (warming-control) ºC") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  scale_x_discrete(breaks = allplots_temp_24h_diff$time[c(1, seq(24, length(allplots_temp_24h_diff$time), length.out = 5))]) +
  scale_color_manual(values = c("Top" = "red2", "Bottom" = "blue3", "Ground" = "green3"),
                     name = "Temperature type") +
  theme_bw()+
  theme(legend.position = "NULL")

ggplot(allplots_temp_24h_diff_sum) +
  geom_line(aes(x = time, y = t_top_mean_diff, group = 1, color = "Top"), linetype = "solid", linewidth = 1) +
  geom_line(aes(x = time, y = t_top_mean_diff + t_top_sd_diff, group = 1, color = "Top"), linetype = "dashed") +
  geom_line(aes(x = time, y = t_top_mean_diff - t_top_sd_diff, group = 1, color = "Top"), linetype = "dashed")+
  
  geom_line(aes(x = time, y = t_bottom_mean_diff, group = 1, color = "Bottom"), linetype = "solid", linewidth = 1) +
  geom_line(aes(x = time, y = t_bottom_mean_diff + t_bottom_sd_diff, group = 1, color = "Bottom"), linetype = "dashed") +
  geom_line(aes(x = time, y = t_bottom_mean_diff - t_bottom_sd_diff, group = 1, color = "Bottom"), linetype = "dashed")+
  
  geom_line(aes(x = time, y = t_ground_mean_diff, group = 1, color = "Ground"), linetype = "solid", linewidth = 1) +
  geom_line(aes(x = time, y = t_ground_mean_diff + t_ground_sd_diff, group = 1, color = "Ground"), linetype = "dashed") +
  geom_line(aes(x = time, y = t_ground_mean_diff - t_ground_sd_diff, group = 1, color = "Ground"), linetype = "dashed")+
  
  labs(x = "24 hours (April-Sept 2023 & April-May 2024)", y = "Temperature difference (warming-control) ºC") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  scale_x_discrete(breaks = allplots_temp_24h_diff$time[c(1, seq(24, length(allplots_temp_24h_diff$time), length.out = 5))]) +
  scale_color_manual(values = c("Top" = "red2", "Bottom" = "blue3", "Ground" = "green3"),
                     name = "Temperature type") +
  theme_bw()+
  theme(legend.position = "NULL")



ggt24h_diff_justtop <- ggplot(allplots_temp_24h_diff) +
  geom_line(aes(x = time, y = t_top_mean_diff, group = 1, color = "red2"), linetype = "solid", linewidth = 1) +
  geom_line(aes(x = time, y = t_top_mean_diff + t_top_sd_diff, group = 1, color = "red2"), linetype = "dashed") +
  geom_line(aes(x = time, y = t_top_mean_diff - t_top_sd_diff, group = 1, color = "red2"), linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  labs(x = "24 hours (January 2023 - May 2024)", y = "Temperature difference (warming-control) ºC") +
  scale_x_discrete(breaks = allplots_temp_24h_diff$time[c(1, seq(24, length(allplots_temp_24h_diff$time), length.out = 5))]) +
  theme_bw() +
  theme(legend.position = "NULL")


ggt24h_diff_justtop_sum <- ggplot(allplots_temp_24h_diff_sum) +
  geom_line(aes(x = time, y = t_top_mean_diff, group = 1, color = "red2"), linetype = "solid", linewidth = 1) +
  geom_line(aes(x = time, y = t_top_mean_diff + t_top_sd_diff, group = 1, color = "red2"), linetype = "dashed") +
  geom_line(aes(x = time, y = t_top_mean_diff - t_top_sd_diff, group = 1, color = "red2"), linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  labs(x = "24 hours (January 2023 - May 2024)", y = "Temperature difference (warming-control) ºC") +
  scale_x_discrete(breaks = allplots_temp_24h_diff$time[c(1, seq(24, length(allplots_temp_24h_diff$time), length.out = 5))]) +
  theme_bw() +
  theme(legend.position = "NULL")




# CONTROL vs WARMING: Soil moisture difference in 24 h

allplots_sm_24h_diff <- summarise(group_by(all_plots, time, ttreat),
                                  sm_mean = round(mean(soil_moisture, na.rm = T), 2))

allplots_sm_24h_diff <- allplots_sm_24h_diff %>%
  pivot_wider(names_from = ttreat,
              values_from = sm_mean,
              names_prefix = "sm_mean_")
allplots_sm_24h_diff <- summarise(group_by(allplots_sm_24h_diff, time),
                                  sm_diff = sm_mean_w - sm_mean_c)

allplots_sm_24h_diff <- allplots_sm_24h_diff %>% slice(1:(nrow(allplots_sm_24h_diff) - 1))


ggsm24h_diff <- ggplot(allplots_sm_24h_diff) +
  geom_line(aes(x = time, y = sm_diff, group = 1), color = "black", linetype = "solid") +
  labs(x = "January - May", y = "Soil moisture difference (warming-control) raw signal data") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  scale_x_discrete(breaks = allplots_sm_24h_diff$time[c(1, seq(24, length(allplots_sm_24h_diff$time), length.out = 5))])+
  theme_bw()




#Plots 24h 

gg24hdiff <- ggarrange(ggt24h_diff, ggsm24h_diff, 
                    labels = c("A", "B"),
                    ncol = 2, nrow = 1)




# CONTROL vs WARMING: daily difference throughout the year---------------

#CONTROL vs WARMING: temperature daily difference throughout the year

allplots_temp_day_mean <-  summarise(group_by(all_plots, datetimenew, ttreat),
                                t_top_mean = round(mean(T_top, na.rm = T), 2), 
                                t_bottom_mean = round(mean (T_bottom, na.rm = T), 2), 
                                t_ground_mean = round(mean(T_ground, na.rm = T), 2))

allplots_temp_day_mean_sum <-  summarise(group_by(all_plots_sum, datetimenew, ttreat),
                                     t_top_mean = round(mean(T_top, na.rm = T), 2), 
                                     t_bottom_mean = round(mean (T_bottom, na.rm = T), 2), 
                                     t_ground_mean = round(mean(T_ground, na.rm = T), 2))


allplots_temp_day_diff <- allplots_temp_day_mean %>%
  pivot_wider(names_from = ttreat,
              values_from = c(t_top_mean, t_bottom_mean, t_ground_mean),
              names_prefix = "") %>%
  select(datetimenew, starts_with("t_"))


allplots_temp_day_diff_sum <- allplots_temp_day_mean_sum %>%
  pivot_wider(names_from = ttreat,
              values_from = c(t_top_mean, t_bottom_mean, t_ground_mean),
              names_prefix = "") %>%
  select(datetimenew, starts_with("t_"))

allplots_temp_day_diff <- summarise(group_by(allplots_temp_day_diff, datetimenew),
                               t_top_diff = t_top_mean_w - t_top_mean_c,
                               t_bottom_diff = t_bottom_mean_w - t_bottom_mean_c,
                               t_ground_diff = t_ground_mean_w - t_ground_mean_c)

allplots_temp_day_diff_sum <- summarise(group_by(allplots_temp_day_diff_sum, datetimenew),
                                    t_top_diff = t_top_mean_w - t_top_mean_c,
                                    t_bottom_diff = t_bottom_mean_w - t_bottom_mean_c,
                                    t_ground_diff = t_ground_mean_w - t_ground_mean_c)


mean(allplots_temp_day_diff$t_top_diff, na.rm = T)
sd(allplots_temp_day_diff$t_top_diff, na.rm = T)


ggtdailydiff_top <- ggplot(allplots_temp_day_diff, aes(x = datetimenew)) +
  geom_point(aes(y = t_top_diff), size = 0.5, alpha = 0.2, color = "red2")+
  geom_smooth(aes(y = t_top_diff), color = "red2", fill = "red2") +
  labs(x = NULL, y = "Temperature difference (ºC) ") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  scale_x_datetime(breaks = scales::date_breaks("10 weeks"), labels = scales::date_format("%Y-%m-%d")) +
  theme_bw()

ggtdailydiff_ground <- ggplot(allplots_temp_day_diff, aes(x = datetimenew)) +
  geom_point(aes(y = t_ground_diff), , size = 0.5, alpha = 0.2, color = "green") +
  geom_smooth(aes(y = t_ground_diff), color = "green3", fill = "green3") +
  labs(x = "January - May", y = "Tground diff" ) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  scale_x_datetime(breaks = scales::date_breaks("10 weeks"), labels = scales::date_format("%Y-%m-%d")) +
  theme_bw()

ggtdailydiff_bottom <- ggplot(allplots_temp_day_diff, aes(x = datetimenew)) +
  geom_point(aes(y = t_bottom_diff), , size = 0.5, alpha = 0.2, color = "blue2") +
  geom_smooth(aes(y = t_bottom_diff), color = "blue2", fill = "blue2") +
  labs(x = NULL, y = "TBottom diff") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  scale_x_datetime(breaks = scales::date_breaks("10 weeks"), labels = scales::date_format("%Y-%m-%d")) +
  theme_bw()



ggtdailtdiff <- ggarrange(ggtdailydiff_top, ggtdailydiff_bottom, ggtdailydiff_ground, 
                         labels = c("A", "B", "C"),
                         ncol = 1, nrow =3 )



ggtdailydiff_top_sum <- ggplot(allplots_temp_day_diff_sum, aes(x = datetimenew)) +
  geom_point(aes(y = t_top_diff), size = 0.5, alpha = 0.2, color = "red2")+
  geom_smooth(aes(y = t_top_diff), color = "red2", fill = "red2") +
  labs(x = NULL, y = "Temperature difference (ºC) ") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  scale_x_datetime(breaks = scales::date_breaks("10 weeks"), labels = scales::date_format("%Y-%m-%d")) +
  theme_bw()

ggtdailydiff_ground_sum <- ggplot(allplots_temp_day_diff_sum, aes(x = datetimenew)) +
  geom_point(aes(y = t_ground_diff), , size = 0.5, alpha = 0.2, color = "green") +
  geom_smooth(aes(y = t_ground_diff), color = "green3", fill = "green3") +
  labs(x = "January - May", y = "Tground diff" ) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  scale_x_datetime(breaks = scales::date_breaks("10 weeks"), labels = scales::date_format("%Y-%m-%d")) +
  theme_bw()

ggtdailydiff_bottom_sum <- ggplot(allplots_temp_day_diff_sum, aes(x = datetimenew)) +
  geom_point(aes(y = t_bottom_diff), , size = 0.5, alpha = 0.2, color = "blue2") +
  geom_smooth(aes(y = t_bottom_diff), color = "blue2", fill = "blue2") +
  labs(x = NULL, y = "TBottom diff") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  scale_x_datetime(breaks = scales::date_breaks("10 weeks"), labels = scales::date_format("%Y-%m-%d")) +
  theme_bw()



ggtdailtdiff <- ggarrange(ggtdailydiff_top_sum, ggtdailydiff_bottom_sum, ggtdailydiff_ground_sum, 
                          labels = c("A", "B", "C"),
                          ncol = 1, nrow =3 )




#CONTROL vs WARMING: soil_moisture daily difference throughout the year

allplots_sm_day_diff <- summarise(group_by(all_plots, datetimenew, ttreat),
                                  sm_mean = round(mean(soil_moisture, na.rm = T), 2))

allplots_sm_day_diff <- allplots_sm_day_diff %>%
  pivot_wider(names_from = ttreat,
              values_from = sm_mean,
              names_prefix = "sm_mean_")
allplots_sm_day_diff <- summarise(group_by(allplots_sm_day_diff, datetimenew),
                                  sm_diff = sm_mean_w - sm_mean_c)

allplots_sm_day_diff <- allplots_sm_day_diff %>% slice(1:(nrow(allplots_sm_day_diff) - 1))


ggsmdailydiff <- ggplot(allplots_sm_day_diff) +
  geom_line(aes(x = datetimenew, y = sm_diff, group = 1), color = "black", linetype = "solid") +
  labs(x = "January - May", y = "Soil moisture diff") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black")+
  scale_x_datetime(breaks = scales::date_breaks("3 weeks"), labels = scales::date_format("%Y-%m-%d"))+
  theme_bw()



# CONTROL vs OTC: daily dynamic throughout the whole time series ---------------------------

allplots_day <-  summarise(group_by(all_plots, date, ttreat),
                                     day_t_top_mean = round(mean(T_top, na.rm = T), 2), 
                                     day_t_bottom_mean = round(mean (T_bottom, na.rm = T), 2), 
                                     day_t_ground_mean = round(mean(T_ground, na.rm = T), 2),
                                day_sm_mean = round(mean(soil_moisture, na.rm = T), 2))


allplots_day <- allplots_day %>%
  pivot_wider(names_from = ttreat,
              values_from = c(day_t_top_mean, day_t_bottom_mean, day_t_ground_mean,day_sm_mean),
              names_prefix = "") %>%
  select(date, starts_with("day_"))
colnames(allplots_day) <- c("date", "t_top_c", "t_top_w", "t_bottom_c", "t_bottom_w", "t_ground_c", "t_ground_w",
                      "sm_c", "sm_w")
#str(allplots_day)

ggttopday <- ggplot(allplots_day, aes(x = date)) + 
  geom_line(aes(y = t_top_w), color = "darkred", group = 1) +
  geom_line(aes(y = t_top_c), color="steelblue", group = 1)+
  theme_bw()+
  labs(x = " ", y =  "Temperature (ºC)")+ 
  ggtitle("Top temperature") +
  theme(
    plot.title = element_text(color="black", size=12, face="bold.italic"),
    axis.title.y = element_text(color="black", size=12, face="bold")
  ) +
  scale_x_discrete(breaks = allplots_day$date[c(1, seq(1, length(allplots_day$date), length.out = 5))])


ggtbottomday <- ggplot(allplots_day, aes(x = date)) + 
  geom_line(aes(y = t_bottom_w), color = "darkred", group = 1) +
  geom_line(aes(y = t_bottom_c), color="steelblue", group = 1)+
  theme_bw()+
  labs(x = " ", y =  " ")+ 
  ggtitle("Bottom temperature") +
  theme(
    plot.title = element_text(color="black", size=12, face="bold.italic")) +
  scale_x_discrete(breaks = allplots_day$date[c(1, seq(1, length(allplots_day$date), length.out = 5))])


ggtgroundday <- ggplot(allplots_day, aes(x = date)) + 
  geom_line(aes(y = t_ground_w), color = "darkred", group = 1) +
  geom_line(aes(y = t_ground_c), color="steelblue", group = 1)+
  theme_bw()+
  labs(x = " ", y =  " ")+ 
  ggtitle("Ground temperature") +
  theme(
    plot.title = element_text(color="black", size=12, face="bold.italic")) +
  scale_x_discrete(breaks = allplots_day$date[c(1, seq(1, length(allplots_day$date), length.out = 5))])

ggsmday <- ggplot(allplots_day, aes(x = date)) +
  geom_line(aes(y = sm_w), color = "darkred", group = 1) +
  geom_line(aes(y = sm_c), color = "steelblue", group = 1) +
  theme_bw()+
  labs( x = NULL, y = "Soil moisture") + 
  ggtitle("Soil moisture") + 
  theme(
    plot.title = element_text(color="black", size=12, face="bold.italic"),
    axis.title.y = element_text(color="black", size=12, face="bold")
  ) +
  scale_x_discrete(breaks = allplots_day$date[c(1, seq(1, length(allplots_day$date), length.out = 5))])
  

#Plots

ggtday <- ggarrange(ggttopday, ggtbottomday, ggtgroundday,
          labels = c("A", "B", "C"),
          ncol = 3, nrow = 1)

ggday <- ggarrange(ggttopday, ggtbottomday, ggtgroundday, ggsmday,
                   labels = c( "A", "B", "C", "D"), 
                   ncol = 4, nrow = 1)

# CONTROL vs OTC: Boxplots---------------------------------

allplotsbp <- all_plots %>%
  pivot_longer(cols = starts_with("T_"), names_to = "T_sensor", values_to = "Temperature")
treat_color <- c("w" = "red", "c" = "blue")
ggtboxplot <- ggplot(allplotsbp,
                        aes(x = T_sensor,
                            y = Temperature,
                            color = ttreat)) +
  geom_boxplot() + 
  labs( x = " ", y = " Temperature (ºC) ")+
  scale_color_manual(values = treat_color)+
  theme_minimal()+
  ggtitle("All plots")

ggtboxplot

ggtboxplot_meant <- ggplot(allplotsbp,
                     aes(x = ttreat,
                         y = Temperature,
                         color = ttreat)) +
  geom_boxplot() + 
  labs( x = " ", y = " Temperature (ºC) ")+
  scale_color_manual(values = treat_color)+
  theme_minimal()+
  ggtitle("Temperature, all plots")

ggtboxplot_meant



ggboxsoilm_all <- ggplot(allplotsbp,
                         aes(x = ttreat,
                             y = soil_moisture,
                             color = ttreat)) +
  geom_boxplot() + 
  labs ( x = " ", y = "Soil moisture") +
  scale_color_manual(values = treat_color)+
  theme_minimal()+
  ggtitle("Soil moisture, all plots")

#ggboxsoilm_all


ggboxplots <- ggarrange(ggtboxplot, ggtboxplot_meant, ggboxsoilm_all,
                        labels = c("A", "B", "C"), 
                        ncol = 3, nrow = 1)



# VISUALIZACIÓN DE TODOS LOS GRÁFICOS----------------


par(mfrow = c(4, 4))

for (i in seq_along(plots_list)) {
  x <- plots_list[[i]]
  hist(x$T_ground, main = names(plots_list)[i])
}

for (i in seq_along(plots_list)) {
  x <- plots_list[[i]]
  hist(x$T_bottom, main = names(plots_list)[i])
}

for (i in seq_along(plots_list)) {
  x <- plots_list[[i]]
  hist(x$T_top, main = names(plots_list)[i])
}

for (i in seq_along(plots_list)) {
  x <- plots_list[[i]]
  hist(x$soil_moisture, main = names(plots_list)[i])
}


grid.arrange(grobs = dailygraph_list, ncol = 4)
grid.arrange(grobs = dailygraph_temp_list, nrow = 4, ncol = 4)
grid.arrange(grobs = dailygraph_sm_list, ncol = 4)
lapply(plots_list, eachplot)

ggt24h_diff
ggsm24h_diff
gg24hdiff

ggtdailtdiff
ggsmdailydiff

ggttopday
ggtbottomday
ggtgroundday
ggtday
ggsmday
ggday

ggtboxplot
ggtboxplot_meant
ggboxsoilm_all
ggboxplots


# AVERAGE DATA:  -----------


wttop <- paste(round(mean(ws$T_top),2), "±", round(sd(ws$T_top),2))
wtbottom <- paste(round(mean(ws$T_bottom),2), "±", round(sd(ws$T_bottom),2))
wtground <- paste(round(mean(ws$T_ground),2), "±", round(sd(ws$T_ground),2))

cttop <- paste(round(mean(controls$T_top),2), "±", round(sd(controls$T_top),2))
ctbottom <- paste(round(mean(controls$T_bottom),2), "±", round(sd(controls$T_bottom),2))
ctground <- paste(round(mean(controls$T_ground),2), "±", round(sd(controls$T_ground),2))

ttopdiff <- paste(round(mean(allplots_temp_day_diff$t_top_diff, na.rm = T),2), "±", round(sd(allplots_temp_day_diff$t_top_diff, na.rm = T),2))
tbottomdiff <- paste(round(mean(allplots_temp_day_diff$t_bottom_diff,na.rm = T),2), "±", round(sd(allplots_temp_day_diff$t_bottom_diff, na.rm = T),2))
tgrounddiff <- paste(round(mean(allplots_temp_day_diff$t_ground_diff, na.rm = T),2), "±", round(sd(allplots_temp_day_diff$t_ground_diff, na.rm = T),2))

wsm <- paste(round(mean(ws$soil_moisture),2), "±", round(s.err(ws$soil_moisture),2))
controlsm <- paste(round(mean(controls$soil_moisture),2), "±", round(s.err(controls$soil_moisture),2))


TTOP <- c(wttop, cttop, ttopdiff)
TBOTTOM <- c(wtbottom, ctbottom, tbottomdiff)
TGROUND <- c(wtground, ctground, tgrounddiff)
SOILMOIST <- c(wsm, controlsm, "NA")

average_values <- data.frame(TTOP, TBOTTOM, TGROUND, SOILMOIST)
rownames(average_values) <- c("Warming", "Control", "Difference (Warming-Control)")
colnames(average_values) <- c("Top temperature (ºC)", "Bottom temperature (ºC)", "Ground temperature (ºC)", "Soil moisture (?)")

average_values %>% write.csv("results/average_values.csv")

avg_values <- read.csv("results/average_values.csv")

# Statistical test #####




