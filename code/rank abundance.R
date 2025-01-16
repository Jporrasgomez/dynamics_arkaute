####intento de aplicar el script de dani con las mariposas (no he hecho eso, he seguido lo que me había dicho Javi)
##### Carga de packages y almohadillas
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

####carga de la base de datos
summary(flora_db)
#dividir la columna abundance entre 100 para tener la abundancia sobre 1
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
treatment_c$plot <- gsub("plot2", "plot 2", treatment_c$plot) ####esto es porque hay un plot2 y un plot 2 y si no aparecen como 2 plots diferentes
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

# Calcular la abundancia relativa que es la abundancia total de la especie entre la abundancia total del plot
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

##ahora intentando aplicar lo de dani, añadimos una nueva columna asignando colores
abundance_data_c <- abundance_data_c %>%
  mutate(ab_group = cut(relative_abundance,
                        breaks = c(-Inf, 0.09, 0.3, Inf), ####estos son los parametros que he puesto para rare medium y high pero se pueden cambiar
                        labels = c("rare", "medium", "high"),
                        include.lowest = TRUE))
ab_groups_control <- select(abundance_data_c, species, ab_group)
summary(ab_groups_control)
abundance_data_p$species <- as.character(abundance_data_p$species)
ab_groups_control$species <- as.character(ab_groups_control$species)
abundance_data_w$species<-as.character(abundance_data_w$species)
abundance_data_wp$species<-as.character(abundance_data_wp$species)
# Realiza el merge para fusionar los archivos de cada uno de los tratameintos con la lista de especie en control
merged_data <- merge(abundance_data_p, ab_groups_control, by = "species", all.x = TRUE)
rankperturbance<-merged_data
rankw<-merge(abundance_data_w, ab_groups_control, by="species", all.x=TRUE)
rankwp<-merge(abundance_data_wp, ab_groups_control, by="species", all.x = TRUE)
##voy a graficar los plots, estos son los que tienen 4 líneas

merged_data<-ggplot(abundance_data_c, aes(x = rango, y = relative_abundance, group = plot, color = ab_group)) +
  geom_point() +
  geom_line() +
  scale_y_log10()+
  scale_color_manual(values = c("high" = "red", "medium" = "blue", "rare" = "black")) +
  labs(title = "RAD-c", x = "Rank", y = "Relative abundance")
print(merged_data)

#a los NA en otros plot les voy a llamar "Not found in control" y les vpoy a asignar el color verde
summary(rankperturbance)
# Asigna ""Not found in control"" a los valores NA en ab_group
rankperturbance$species <- as.character(rankperturbance$species)
if ("Not found in control" %in% levels(rankperturbance$ab_group)) {
  rankperturbance$ab_group <- factor(rankperturbance$ab_group, levels = c(levels(rankperturbance$ab_group), "Not found in control"))
} else {
  rankperturbance$ab_group <- factor(rankperturbance$ab_group, levels = c("high", "medium", "rare", "Not found in control"))
}
rankw$species <- as.character(rankw$species)
if ("Not found in control" %in% levels(rankw$ab_group)) {
  rankw$ab_group <- factor(rankw$ab_group, levels = c(levels(rankw$ab_group), "Not found in control"))
} else {
  rankw$ab_group <- factor(rankw$ab_group, levels = c("high", "medium", "rare", "Not found in control"))
}

rankwp$species <- as.character(rankwp$species)
if ("Not found in control" %in% levels(rankwp$ab_group)) {
  rankwp$ab_group <- factor(rankwp$ab_group, levels = c(levels(rankwp$ab_group), "Not found in control"))
} else {
  rankwp$ab_group <- factor(rankwp$ab_group, levels = c("high", "medium", "rare", "Not found in control"))
}

# Asigna ""Not found in control"" a los valores NA en ab_group
rankperturbance$ab_group[is.na(rankperturbance$ab_group)] <- "Not found in control"
rankw$ab_group[is.na(rankw$ab_group)] <- "Not found in control"
rankwp$ab_group[is.na(rankwp$ab_group)]<-"Not found in control"
# Crea el gráfico
prad<-ggplot(rankperturbance, aes(x = rango, y = relative_abundance, group = plot, color = ab_group)) +
  geom_point() +
  geom_line() +
  scale_y_log10()+
  scale_color_manual(values = c("high" = "red", "medium" = "blue", "rare" = "black", "Not found in control" = "green")) +
  labs(title = "RAD-p", x = "Rank", y = "Relative abundance")
print(prad)
wrad<-ggplot(rankw, aes(x = rango, y = relative_abundance, group = plot, color = ab_group)) +
  geom_point() +
  geom_line() +
  scale_y_log10()+
  scale_color_manual(values = c("high" = "red", "medium" = "blue", "rare" = "black", "Not found in control" = "green")) +
  labs(title = "RAD-w", x = "Rank", y = "Relative abundance")
print(wrad)
wprad<-ggplot(rankwp, aes(x = rango, y = relative_abundance, group = plot, color = ab_group)) +
  geom_point() +
  geom_line() +
  scale_y_log10()+
  scale_color_manual(values = c("high" = "red", "medium" = "blue", "rare" = "black", "Not found in control" = "green")) +
  labs(title = "RAD-wp", x = "Rank", y = "Relative abundance")
print(wprad)

###quiero ahora hacer estos graficos pero con una linea por tratamiento
#he eliminado la columna sampliing en cada archivo, tengo estos archivos:
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

# Calcular la suma de abundancias por especie
abundance_by_speciesc <- aggregate(abundance ~ species, data = treatment_c, sum)
abundance_by_speciesp <- aggregate(abundance ~ species, data = treatment_p, sum)
abundance_by_speciesw <- aggregate(abundance ~ species, data = treatment_w, sum)
abundance_by_specieswp <- aggregate(abundance ~ species, data = treatment_wp, sum)
##calculo de la abundancia total y luego de las abundancias relativas
total_abundancec <- sum(abundance_by_speciesc$abundance)
total_abundancep <- sum(abundance_by_speciesp$abundance)
total_abundancew <- sum(abundance_by_speciesw$abundance)
total_abundancewp <- sum(abundance_by_specieswp$abundance)

# Paso 2: Calcular la abundancia relativa
abundance_by_speciesc$abundance_relative <- abundance_by_speciesc$abundance / total_abundancec
abundance_by_speciesp$abundance_relative <- abundance_by_speciesp$abundance / total_abundancep
abundance_by_speciesw$abundance_relative <- abundance_by_speciesw$abundance / total_abundancew
abundance_by_specieswp$abundance_relative <- abundance_by_specieswp$abundance / total_abundancewp
##ahora establecer un rango ordenando las especies
# Convertir 'abundance' y 'abundance_relative' a numérico
abundance_by_speciesc$abundance <- as.numeric(abundance_by_speciesc$abundance)
abundance_by_speciesc$abundance_relative <- as.numeric(abundance_by_speciesc$abundance_relative)
abundance_by_speciesp$abundance <- as.numeric(abundance_by_speciesp$abundance)
abundance_by_speciesp$abundance_relative <- as.numeric(abundance_by_speciesp$abundance_relative)
abundance_by_speciesw$abundance <- as.numeric(abundance_by_speciesw$abundance)
abundance_by_speciesw$abundance_relative <- as.numeric(abundance_by_speciesw$abundance_relative)
abundance_by_specieswp$abundance <- as.numeric(abundance_by_specieswp$abundance)
abundance_by_specieswp$abundance_relative <- as.numeric(abundance_by_specieswp$abundance_relative)
# Ordenar el data frame por  abundancia relativa descendente
summary(abundance_by_speciesc)
abundance_by_speciesc <- abundance_by_speciesc %>%
  arrange(desc(abundance_relative)) %>%  # Ordenar por abundancia relativa de mayor a menor
  mutate(rank = row_number())
abundance_by_speciesp <- abundance_by_speciesp %>%
  arrange(desc(abundance_relative)) %>%  # Ordenar por abundancia relativa de mayor a menor
  mutate(rank = row_number())
abundance_by_speciesw <- abundance_by_speciesw %>%
  arrange(desc(abundance_relative)) %>%  # Ordenar por abundancia relativa de mayor a menor
  mutate(rank = row_number())
abundance_by_specieswp <- abundance_by_specieswp %>%
  arrange(desc(abundance_relative)) %>%  # Ordenar por abundancia relativa de mayor a menor
  mutate(rank = row_number())

##ahora establecemos categorías segun abundancia en el control, son diferentes a los otros plts
abundance_by_speciesc <- abundance_by_speciesc %>%
  mutate(ab_group = cut(abundance_relative,
                        breaks = c(-Inf, 0.05, 0.1, Inf),
                        labels = c("rare", "medium", "high"),
                        include.lowest = TRUE))
ab_groups_c <- select(abundance_by_speciesc, species, ab_group)
summary(ab_groups_c)
abundance_by_speciesp$species <- as.character(abundance_by_speciesp$species)
ab_groups_c$species <- as.character(ab_groups_c$species)
abundance_by_speciesw$species<-as.character(abundance_by_speciesw$species)
abundance_by_specieswp$species<-as.character(abundance_by_specieswp$species)
# Realiza el merge de ab_grops_ con las bases de datos de otros tratamientos
rankp3<- merge(abundance_by_speciesp, ab_groups_c, by = "species", all.x = TRUE)
rankw3<-merge(abundance_by_speciesw, ab_groups_c, by="species", all.x=TRUE)
rankwp3<-merge(abundance_by_specieswp, ab_groups_c, by="species", all.x = TRUE)
##ahora graficamos en control a ver que sale

colorc<-ggplot(abundance_by_speciesc, aes(x = rank, y = abundance_relative, color = ab_group)) +
  geom_point() +
  geom_line(color="black") +
  scale_y_log10()+
  scale_color_manual(values = c("high" = "red", "medium" = "blue", "rare" = "black")) +
  labs(title = "RAD-c", x = "Rank", y = "Relative abundance")
print(colorc)

##ahora a los NA que aparecen en las otras bases de datos les voy a llamar Not found in control
if ("Not found in control" %in% levels(rankp3$ab_group)) {
  rankp3$ab_group <- factor(rankp3$ab_group, levels = c(levels(rankp3$ab_group)[!levels(rankp3$ab_group) %in% "Not found in control"], "Not found in control"))
} else {
  rankp3$ab_group <- factor(rankp3$ab_group, levels = c("high", "medium", "rare", "Not found in control"))
}
rankp3$ab_group[is.na(rankp3$ab_group)] <- "Not found in control"

if ("Not found in control" %in% levels(rankw3$ab_group)) {
  rankw3$ab_group <- factor(rankw3$ab_group, levels = c(levels(rankw3$ab_group)[!levels(rankw3$ab_group) %in% "Not found in control"], "Not found in control"))
} else {
  rankw3$ab_group <- factor(rankw3$ab_group, levels = c("high", "medium", "rare", "Not found in control"))
}
rankw3$ab_group[is.na(rankw3$ab_group)] <- "Not found in control"

if ("Not found in control" %in% levels(rankp3$ab_group)) {
  rankp3$ab_group <- factor(rankp3$ab_group, levels = c(levels(rankp3$ab_group)[!levels(rankp3$ab_group) %in% "Not found in control"], "Not found in control"))
} else {
  rankp3$ab_group <- factor(rankp3$ab_group, levels = c("high", "medium", "rare", "Not found in control"))
}
rankp3$ab_group[is.na(rankp3$ab_group)] <- "Not found in control"

if ("Not found in control" %in% levels(rankwp3$ab_group)) {
  rankwp3$ab_group <- factor(rankwp3$ab_group, levels = c(levels(rankwp3$ab_group)[!levels(rankwp3$ab_group) %in% "Not found in control"], "Not found in control"))
} else {
  rankwp3$ab_group <- factor(rankwp3$ab_group, levels = c("high", "medium", "rare", "Not found in control"))
}
rankwp3$ab_group[is.na(rankwp3$ab_group)] <- "Not found in control"
##ahora graficamos esto
colorp<-ggplot(rankp3, aes(x = rank, y = abundance_relative, color = ab_group)) +
  geom_point() +
  geom_line(color="black") +
  scale_y_log10()+
  scale_color_manual(values = c("high" = "red", "medium" = "blue", "rare" = "black", "Not found in control"="green")) +
  labs(title = "RAD-p", x = "Rank", y = "Relative abundance")
print(colorp)
colorw<-ggplot(rankw3, aes(x = rank, y = abundance_relative, color = ab_group)) +
  geom_point() +
  geom_line(color="black") +
  scale_y_log10()+
  scale_color_manual(values = c("high" = "red", "medium" = "blue", "rare" = "black", "Not found in control"="green")) +
  labs(title = "RAD-w", x = "Rank", y = "Relative abundance")
print(colorw)
colorwp<-ggplot(rankwp3, aes(x = rank, y = abundance_relative, color = ab_group)) +
  geom_point() +
  geom_line(color="black") +
  scale_y_log10()+
  scale_color_manual(values = c("high" = "red", "medium" = "blue", "rare" = "black", "Not found in control"="green")) +
  labs(title = "RAD-wp", x = "Rank", y = "Relative abundance")
print(colorwp)