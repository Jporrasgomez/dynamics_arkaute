rm(list = ls(all.names = TRUE)) 
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) 
pacman::p_load(tidyverse,here, ggpubr, dplyr,ggplot2,ggsci,Polychrome, RColorBrewer,ggpubr)

all_df<-read.csv(here("data","useful", "all_data.csv"))

plantas_visitas <- all_df %>%
  group_by(Planta, Site) %>%
  summarise(visitas = n(), .groups = "drop") %>%
  arrange(desc(visitas))

especies_seleccionadas <- c("Lotus corniculatus", "Helleborus viridis subsp. occidentalis", "Helianthemum nummularium",
                            "Vicia pyrenaica", "Lathyrus linifolius", "Pedicularis sylvatica subsp. sylvatica",
                            "Scilla verna",  "Cardamine pratensis", "Hepatica nobilis", "Erysimum gorbeanum", 
                            "Halimiun calycinum","Cistus salviifolius", "Cistus ladanifer", "Cistus crispus","Halimiun halimifolium",
                            "Cistus libanotis", "Bellis perennis","Rosmarinus officinalis","Taraxacum sp.",
                            "Potentilla erecta", "Ranunculus sp.", "Lavandula stoechas", "Leontodon hispidus",
                            "Ranunculus repens", "Halimium halimifolium", "Leontodon longirostris", "Echium plantagineum",
                            "Halimium calycinum", "Medicago lupulina", "Trifolium repens", "Erica cinerea",
                            "Thymus praecox subsp. polytrichus", "Veronica officinalis", "Andryala arenaria","Scorzonera humilis",
                            "Andryala integrifolia", "Trifolium pratense")


datos_rangos <- all_df %>%
  filter(Planta %in% especies_seleccionadas) %>%
  group_by(Planta, Site) %>%
  summarise(inicio = min(Periodo), fin = max(Periodo))



length(unique(datos_rangos$Planta))
colors<-get_palette("jama",55)


presence_plot <- ggplot(datos_rangos %>% filter(Planta %in% especies_seleccionadas),
                aes(y = Planta)) +
                geom_linerange(aes(xmin = inicio, xmax = fin, color = Planta), size = 4) +
                scale_color_manual(values = colors) +
                labs(x = "Sampling period", y = "Species") +
                facet_wrap(~ Site, scales = "free_y")+
                theme_minimal() +
                theme(
                   axis.text.y = element_text(size = 10),
                   axis.title.x = element_text(size = 12),
                   legend.position = "none"
                 )+
                 scale_x_continuous(breaks = 1:9) 


all_df <- all_df %>%
  mutate(Pollinator_id = recode(Pollinator_id, "Platycheirus  sp" = "Platycheirus sp"))

all_df$Periodo <- as.numeric(all_df$Periodo)

datos_ordenados <- all_df %>%
  group_by(Pollinator_id, Site, Periodo) %>%
  summarise(
    abundancia = n(),  # Número de observaciones (abundancia) por especie y periodo
    .groups = "drop"  # Evitar advertencias de agrupamiento
  ) %>%
  arrange(Site, Pollinator_id, Periodo)  # Ordenar por sitio, especie y periodo

datos_ordenados_top20 <- datos_ordenados %>%
  group_by(Site, Pollinator_id) %>%
  summarise(
    presencia_total = n_distinct(Periodo),  # Cuántos períodos tiene presencia la especie
    .groups = "drop"
  ) %>%
  group_by(Site) %>%
  top_n(20, presencia_total) %>%  # Seleccionar las 20 especies con más presencia
  ungroup()

# Filtrar los datos para incluir solo las 20 especies seleccionadas por sitio
datos_filtrados <- datos_ordenados %>%
  semi_join(datos_ordenados_top20, by = c("Site", "Pollinator_id"))

datos_rep <- datos_filtrados %>%
  uncount(weights = abundancia)

colors<-get_palette("jama",45)


violin_pol <- ggplot(datos_rep, aes(x = Periodo, y = Pollinator_id, fill = Pollinator_id)) +
  geom_violin(color = NA, bw = 0.35) +  # Gráfico de violín con borde negro, la anchura se ajusta automáticamente
  scale_fill_manual(values = colors) +  # Usar la paleta de colores especificada (jama de ggsci)
  labs(x = "Sampling period", y = "Species", fill = "Polinizador") +  # Etiquetas
  facet_wrap(~ Site, scales = "free_y") +  # Un gráfico para cada Site
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    legend.text = element_blank(),
    legend.position = "none"
  )+
  scale_x_continuous(breaks = 1:9) 

save(presence_plot, violin_pol, file="scripts/presence_plot.RData")
