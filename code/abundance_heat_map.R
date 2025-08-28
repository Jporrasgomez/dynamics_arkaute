



species_data <- read.csv("data/species_code.csv") %>% 
  select(code, species, family, life_cycle)


sp_wide_sampling2 <- species_ab_sampling %>%
  select(-id) %>%
  mutate(abundance = round(abundance, 2)) %>% 
  pivot_wider(id_cols = c(code, treatment,),
              names_from = sampling,
              values_from = abundance,
              values_fill = list(abundance = 0)) %>% 
  ungroup() %>% 
  as_tibble() %>% 
  left_join(species_data) %>% 
  select(code, species, family, life_cycle, treatment, "0", "1", "2", "3", "4",
         "5", "6", "7", "8", "9", "10", "11", "12" ,
         "13", "14", "15", "16", "17", "18", "19", "20") 
#%>% 
 # write.csv("results/abundance_df.csv")



library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

pal_treat <- c(
  c  = "#12D08C",
  w  = "#E05050",
  p  = "#00CAFF",
  wp = "#903996"
)
trt_order <- c("c","w","p","wp")  # orden deseado de tratamientos

# Datos a largo para pintar, SIN agrupar especies (cada especie-tratamiento se mantiene)
heat_df <- sp_wide_sampling2 %>%
  pivot_longer(cols = `0`:`20`, names_to = "sampling", values_to = "value") %>%
  mutate(
    sampling = factor(sampling, levels = as.character(0:20)),
    treatment = factor(treatment, levels = trt_order),
    # etiqueta de fila (especie × tratamiento) para el eje Y
    sp_trt = paste(species, treatment, sep = "_"),
    # intensidad según cercanía a 0 (global)
    alpha_val = rescale(abs(value), to = c(0.1, 1), from = range(abs(value), na.rm = TRUE))
  )

# 1) Definir el ORDEN de las filas: por especie (alfabético) y dentro por tratamiento
levels_tbl <- heat_df %>%
  distinct(species, treatment) %>%
  arrange(species, treatment) %>%                     # especie A..Z, tratamiento en trt_order
  mutate(sp_trt = paste(species, treatment, sep = "_")) %>%
  group_by(species) %>%
  mutate(y_label = if_else(row_number() == 1, species, "")) %>%  # solo la 1ª fila lleva el nombre
  ungroup()

# 2) Aplicar el orden al factor y construir el vector de etiquetas (nombre solo en la primera fila)
heat_df <- heat_df %>%
  mutate(sp_trt = factor(sp_trt, levels = levels_tbl$sp_trt))

label_map <- setNames(levels_tbl$y_label, levels_tbl$sp_trt)

# 3) Gráfico
ggplot(heat_df, aes(x = sampling, y = sp_trt, fill = treatment, alpha = alpha_val)) +
  geom_tile() +
  scale_fill_manual(values = pal_treat) +
  scale_alpha(range = c(0.1, 1), guide = "none") +
  scale_y_discrete(labels = label_map) +   # ← especie solo en la primera fila de cada bloque
  labs(x = "Sampling", y = "Species", title = "Heatmap (species mostrada 1 vez por bloque)") +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 6)
  ) +
  coord_equal()  # celdas cuadradas



species_ab_sampling %>% 
  filter(treatment == "w") %>% 
ggplot(aes( x = sampling, y = abundance, color = code)) + 
  geom_line(aes(group = code)) + 
  geom_point()
