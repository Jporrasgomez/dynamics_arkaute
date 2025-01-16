install.packages(lubridate)
library(lubridate)
install.packages("ggplot2")
install.packages("dplyr")
install.packages("lubridate")
install.packages("tidyr")
install.packages("gridExtra")
library(gridExtra)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
summary(flora_db)
#dividir la columna abundance entre 100
flora_db$abundance <- flora_db$abundance / 100
#eliminar las columnas que no voy a usar
flora_db <- subset(flora_db, select = -c(height, Cb, Db, Cm, Dm, category, OBS))
summary(flora_db)
# Crear y guardar archivos para cada tratamiento

flora_db$treatment <- trimws(flora_db$treatment)
treatment_c <- flora_db[flora_db$treatment == "c", ]
write.csv(treatment_c, file = "flora_c.csv", row.names = FALSE)
treatment_wp <- flora_db[flora_db$treatment == "wp", ]
write.csv(treatment_wp, file = "flora_wp.csv", row.names = FALSE)
treatment_w <- flora_db[flora_db$treatment == "w", ]
write.csv(treatment_w, file = "flora_w.csv", row.names = FALSE)
treatment_p <- flora_db[flora_db$treatment == "p", ]
write.csv(treatment_p, file = "flora_p.csv", row.names = FALSE)
#he eliminado la columna sampliing en cada archivo

treatment_c <- treatment_c[, -which(names(treatment_c) == "sampling")]
treatment_c$plot <- gsub("plot2", "plot 2", treatment_c$plot)
treatment_p <- treatment_p[, -which(names(treatment_p) == "sampling")]
treatment_wp <- treatment_wp[, -which(names(treatment_wp) == "sampling")]
treatment_w <- treatment_w[, -which(names(treatment_w) == "sampling")]

# Convertir 'abundance' a numérico para asegurarse de que es un tipo de dato numérico
treatment_c$abundance <- as.numeric(treatment_c$abundance)
treatment_p$abundance<-as.numeric(treatment_p$abundance)
treatment_w$abundance <- as.numeric(treatment_w$abundance)
treatment_wp$abundance<-as.numeric(treatment_wp$abundance)
# Calcular la suma de abundancias por especie y plot
abundance_by_species_plot <- aggregate(abundance ~ species + plot, data = treatment_c, sum)
abundance_by_species_plot_p <- aggregate(abundance ~ species + plot, data = treatment_p, sum)
abundance_by_species_plot_w <- aggregate(abundance ~ species + plot, data = treatment_w, sum)
abundance_by_species_plot_wp <- aggregate(abundance ~ species + plot, data = treatment_wp, sum)
# Calcular la abundancia total por plot
total_abundance_by_plot <- aggregate(abundance ~ plot, data = abundance_by_species_plot, sum)
total_abundance_by_plot_p <- aggregate(abundance ~ plot, data = abundance_by_species_plot_p, sum)
total_abundance_by_plot_w<- aggregate(abundance ~ plot, data = abundance_by_species_plot_w, sum)
total_abundance_by_plot_wp <- aggregate(abundance ~ plot, data = abundance_by_species_plot_wp, sum)
# Fusionar los resultados para obtener un marco de datos con abundancia total por plot
abundance_data_c <- merge(abundance_by_species_plot, total_abundance_by_plot, by = "plot", suffixes = c("", "_total"))
abundance_data_p <- merge(abundance_by_species_plot_p, total_abundance_by_plot_p, by = "plot", suffixes = c("", "_total"))
abundance_data_w <- merge(abundance_by_species_plot_w, total_abundance_by_plot_w, by = "plot", suffixes = c("", "_total"))
abundance_data_wp <- merge(abundance_by_species_plot_wp, total_abundance_by_plot_wp, by = "plot", suffixes = c("", "_total"))
# Calcular la abundancia relativa
abundance_data_c$relative_abundance <- abundance_data_c$abundance / abundance_data_c$abundance_total
abundance_data_c$relative_abundance <- abundance_data_c$abundance / abundance_data_c$abundance_total
abundance_data_p$relative_abundance <- abundance_data_p$abundance / abundance_data_p$abundance_total
abundance_data_p$relative_abundance <- abundance_data_p$abundance / abundance_data_p$abundance_total
abundance_data_w$relative_abundance <- abundance_data_w$abundance / abundance_data_w$abundance_total
abundance_data_w$relative_abundance <- abundance_data_w$abundance / abundance_data_w$abundance_total
abundance_data_wp$relative_abundance <- abundance_data_wp$abundance / abundance_data_wp$abundance_total
abundance_data_wp$relative_abundance <- abundance_data_wp$abundance / abundance_data_wp$abundance_total
# Mostrar las primeras filas de la tabla resultante
head(abundance_data_c)
# Convertir 'abundance' y 'abundance_total' a numérico
abundance_data_c$abundance <- as.numeric(abundance_data_c$abundance)
abundance_data_c$abundance_total <- as.numeric(abundance_data_c$abundance_total)
abundance_data_p$abundance <- as.numeric(abundance_data_p$abundance)
abundance_data_p$abundance_total <- as.numeric(abundance_data_p$abundance_total)
abundance_data_w$abundance <- as.numeric(abundance_data_w$abundance)
abundance_data_w$abundance_total <- as.numeric(abundance_data_w$abundance_total)
abundance_data_wp$abundance <- as.numeric(abundance_data_wp$abundance)
abundance_data_wp$abundance_total <- as.numeric(abundance_data_wp$abundance_total)
# Ordenar el data frame por plot y abundancia relativa descendente
abundance_data_c <- abundance_data_c[order(abundance_data_c$plot, -abundance_data_c$relative_abundance),]
abundance_data_p <- abundance_data_p[order(abundance_data_p$plot, -abundance_data_p$relative_abundance),]
abundance_data_w <- abundance_data_w[order(abundance_data_w$plot, -abundance_data_w$relative_abundance),]
abundance_data_wp <- abundance_data_wp[order(abundance_data_wp$plot, -abundance_data_wp$relative_abundance),]
# Crear la columna 'rango' por plot
abundance_data_c$rango <- ave(seq_along(abundance_data_c$relative_abundance), abundance_data_c$plot, FUN = seq_along)
abundance_data_p$rango <- ave(seq_along(abundance_data_p$relative_abundance), abundance_data_p$plot, FUN = seq_along)
abundance_data_w$rango <- ave(seq_along(abundance_data_w$relative_abundance), abundance_data_w$plot, FUN = seq_along)
abundance_data_wp$rango <- ave(seq_along(abundance_data_wp$relative_abundance), abundance_data_wp$plot, FUN = seq_along)
# Mostrar las primeras filas del data frame resultante
head(abundance_data_c)
head(abundance_data_p)

#hacer gráficos, quiero un gráfico con rango en el eje de las x, abundancia relativa en el eje de las y que haya un color segun plot

summary(abundance_data_c)
# Convertir 'rango' a factor para asegurar un orden correcto en el eje x
abundance_data_c$rango <- factor(abundance_data_c$rango)
abundance_data_p$rango <- factor(abundance_data_p$rango)
abundance_data_w$rango <- factor(abundance_data_w$rango)
abundance_data_wp$rango <- factor(abundance_data_wp$rango)
# Crear el gráfico de puntos con líneas
rankabundance_c<-ggplot(abundance_data_c, aes(x = rango, y = relative_abundance, group = plot, color = plot)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 0.9)) + 
  labs(title = "Rank abundance-c",
       x = "Rank",
       y = "Relative Abundance") +
  theme_minimal()

print(rankabundance_c)
rankabundance_p<-ggplot(abundance_data_p, aes(x = rango, y = relative_abundance, group = plot, color = plot)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 0.9)) + 
  labs(title = "Rank abundance-p",
       x = "Rank",
       y = "Relative abundance") +
  theme_minimal()

print(rankabundance_p)
rankabundance_w<-ggplot(abundance_data_w, aes(x = rango, y = relative_abundance, group = plot, color = plot)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 0.9)) + 
  labs(title = "Rank abundance-w",
       x = "Rank",
       y = "Relative abundance") +
  theme_minimal()

print(rankabundance_w)
rankabundance_wp<-ggplot(abundance_data_wp, aes(x = rango, y = relative_abundance, group = plot, color = plot)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 0.9)) + 
  labs(title = "Rank abundance-wp",
       x = "Rank",
       y = "Relative abundance") +
  theme_minimal()

print(rankabundance_wp)

#ahora diagramas con una linea y con desviacion

# Configurar los datos para el gráfico
media_por_rangocmedia <- aggregate(relative_abundance ~ rango, data = abundance_data_c, mean)
media_por_rangopmedia <- aggregate(relative_abundance ~ rango, data = abundance_data_p, mean)
media_por_rangowmedia <- aggregate(relative_abundance ~ rango, data = abundance_data_w, mean)
media_por_rangowpmedia <- aggregate(relative_abundance ~ rango, data = abundance_data_wp, mean)

desviacion_por_rangocmedia <- aggregate(relative_abundance ~ rango, data = abundance_data_c, sd)
desviacion_por_rangopmedia <- aggregate(relative_abundance ~ rango, data = abundance_data_p, sd)
desviacion_por_rangowmedia <- aggregate(relative_abundance ~ rango, data = abundance_data_w, sd)
desviacion_por_rangowpmedia <- aggregate(relative_abundance ~ rango, data = abundance_data_wp, sd)

resultadoscmedia <- cbind(media_por_rangocmedia, Desviacion = desviacion_por_rangocmedia$relative_abundance)
resultadospmedia <- cbind(media_por_rangopmedia, Desviacion = desviacion_por_rangopmedia$relative_abundance)
resultadoswmedia <- cbind(media_por_rangowmedia, Desviacion = desviacion_por_rangowmedia$relative_abundance)
resultadoswpmedia <- cbind(media_por_rangowpmedia, Desviacion = desviacion_por_rangowpmedia$relative_abundance)
limite_max_y <- 0.9

# Graficar puntos y líneas con desviación estándar
rankabundance_cmedia <- ggplot(resultadoscmedia, aes(x = rango, y = relative_abundance, group = 1)) +
  geom_line(color = "purple") +  # Color de la línea
  geom_point(color = "purple") +  # Color de los puntos
  geom_errorbar(aes(ymin = relative_abundance - Desviacion, ymax = relative_abundance + Desviacion), 
                width = 0.2, color = "blue") +  # Color de las barras de error
  labs(title = "Rank abundance-c",
       x = "Rank", y = "Relative abundance") +
  ylim(0, limite_max_y)  # Establecer límite máximo en el eje y

# Mostrar el gráfico
print(rankabundance_cmedia)

rankabundance_cmedia <- ggplot(resultadoscmedia, aes(x = rango, y = relative_abundance, group = 1)) +
  geom_line(color = "purple") +  # Color de la línea
  geom_point(color = "purple") +  # Color de los puntos
  geom_errorbar(aes(ymin = relative_abundance - Desviacion, ymax = relative_abundance + Desviacion), 
                width = 0.2, color = "blue") +  # Color de las barras de error
  labs(title = "Rank abundance-c",
       x = "Rank", y = "Relative abundance") +
  ylim(0, limite_max_y)  # Establecer límite máximo en el eje y

# Mostrar el gráfico
print(rankabundance_cmedia)

rankabundance_pmedia <- ggplot(resultadospmedia, aes(x = rango, y = relative_abundance, group = 1)) +
  geom_line(color = "purple") +  # Color de la línea
  geom_point(color = "purple") +  # Color de los puntos
  geom_errorbar(aes(ymin = relative_abundance - Desviacion, ymax = relative_abundance + Desviacion), 
                width = 0.2, color = "blue") +  # Color de las barras de error
  labs(title = "Rank abundance-p",
       x = "Rank", y = "Relative abundance") +
  ylim(0, limite_max_y)  # Establecer límite máximo en el eje y

# Mostrar el gráfico
print(rankabundance_pmedia)

rankabundance_wmedia <- ggplot(resultadoswmedia, aes(x = rango, y = relative_abundance, group = 1)) +
  geom_line(color = "purple") +  # Color de la línea
  geom_point(color = "purple") +  # Color de los puntos
  geom_errorbar(aes(ymin = relative_abundance - Desviacion, ymax = relative_abundance + Desviacion), 
                width = 0.2, color = "blue") +  # Color de las barras de error
  labs(title = "Rank abundance-w",
       x = "Rank", y = "Relative abundance") +
  ylim(0, limite_max_y)  # Establecer límite máximo en el eje y

# Mostrar el gráfico
print(rankabundance_wmedia)

rankabundance_wpmedia <- ggplot(resultadoswpmedia, aes(x = rango, y = relative_abundance, group = 1)) +
  geom_line(color = "purple") +  # Color de la línea
  geom_point(color = "purple") +  # Color de los puntos
  geom_errorbar(aes(ymin = relative_abundance - Desviacion, ymax = relative_abundance + Desviacion), 
                width = 0.2, color = "blue") +  # Color de las barras de error
  labs(title = "Rank abundance-wp",
       x = "Rank", y = "Relative abundance") +
  ylim(0, limite_max_y)  # Establecer límite máximo en el eje y

# Mostrar el gráfico
print(rankabundance_wpmedia)

