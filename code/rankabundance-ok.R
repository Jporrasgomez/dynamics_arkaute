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
##calculo de la abundancia total en cada tratamiento y luego de las abundancias relativas
total_abundancec <- sum(abundance_by_speciesc$abundance)
total_abundancep <- sum(abundance_by_speciesp$abundance)
total_abundancew <- sum(abundance_by_speciesw$abundance)
total_abundancewp <- sum(abundance_by_specieswp$abundance)

# Paso 2: Calcular la abundancia relativa (abundancia de cada especie/abundancia total del tratamiento)
abundance_by_speciesc$abundance_relative <- abundance_by_speciesc$abundance / total_abundancec
abundance_by_speciesp$abundance_relative <- abundance_by_speciesp$abundance / total_abundancep
abundance_by_speciesw$abundance_relative <- abundance_by_speciesw$abundance / total_abundancew
abundance_by_specieswp$abundance_relative <- abundance_by_specieswp$abundance / total_abundancewp
##ahora establecer un rango ordenando las especies
# Convertir 'abundance' y 'abundance_relative' a numérico porsi
abundance_by_speciesc$abundance <- as.numeric(abundance_by_speciesc$abundance)
abundance_by_speciesc$abundance_relative <- as.numeric(abundance_by_speciesc$abundance_relative)
abundance_by_speciesp$abundance <- as.numeric(abundance_by_speciesp$abundance)
abundance_by_speciesp$abundance_relative <- as.numeric(abundance_by_speciesp$abundance_relative)
abundance_by_speciesw$abundance <- as.numeric(abundance_by_speciesw$abundance)
abundance_by_speciesw$abundance_relative <- as.numeric(abundance_by_speciesw$abundance_relative)
abundance_by_specieswp$abundance <- as.numeric(abundance_by_specieswp$abundance)
abundance_by_specieswp$abundance_relative <- as.numeric(abundance_by_specieswp$abundance_relative)
# Ordenar el data frame por  abundancia relativa descendente en cada tratemiento
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

##ahora establecemos categorías segun abundancia en el control, he puesto como rare las especies
###con una abundancia menor a 0.05 las medium las entre 0.05 y 0.1 y las high como las que tienen una abundancia mayor a 0.1
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

##ahora a los NA que aparecen en las otras bases de datos les voy a llamar "Not found in control" y los voy a poner como puntos verdes
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