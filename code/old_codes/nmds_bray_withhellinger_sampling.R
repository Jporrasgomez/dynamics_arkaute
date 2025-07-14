



library(vegan)
library(ggplot2)
library(ggrepel)
library(dplyr)

# 1. (Opcional) Transformación Hellinger de abundancias
abundance_hell <- decostand(abundance_data_sampling, method = "hellinger")

list_abundance <- list(abundance_hell, abundance_data_sampling)
                              # 1                # 2
i = 1

# 2. Distancia y NMDS sobre datos transformados
dist_bc_hel <- vegdist(list_abundance[[i]], method = "bray")
nmds_bc_sampling <- metaMDS(dist_bc_hel, k = 2, trymax = 250, maxit = 999)

# 3. Envfit para extraer vectores de especies
ef <- envfit(nmds_bc_sampling ~ ., data = abundance_hell, perm = 999)
# Tomamos sólo los vectores con R2 > 0.2, por ejemplo
vecs <- scores(ef, display = "vectors")
sel   <- which(ef$vectors$r > 0.2)
vecs  <- vecs[sel, , drop = FALSE]
# 4. Preparar data frame para ggplot (renombramos MDS1/MDS2 a NMDS1/NMDS2 para mayor claridad)
nmds_df_sampling <- as_tibble(nmds_bc_sampling$points) %>%
  rename(NMDS1 = MDS1, 
         NMDS2 = MDS2) %>%
  mutate(
    treatment = sp_wide_sampling$treatment,
    sampling  = sp_wide_sampling$sampling,
    date      = sp_wide_sampling$date
  )

# 5. Envfit: extraer sólo las especies con R² > 0.2
ef   <- envfit(nmds_bc_sampling ~ ., data = abundance_hell, perm = 999)
vecs <- scores(ef, display = "vectors")           # matriz con coords
r2   <- ef$vectors$r                              # vector de R2

# Filtramos las filas con R2 > 0.2 y convertimos a data.frame
sel  <- which(r2 > 0.2)
vecs <- as.data.frame(vecs[sel, , drop = FALSE]) %>%
  rownames_to_column("species")
# Comprueba los nombres de columna aquí:
# colnames(vecs)  # deberían ser "NMDS1" y "NMDS2"


# 6. Plot: ahora vecs ya tiene NMDS1/NMDS2, igual que nmds_df_sampling
ggnmds_alltreatments <- 
  ggplot(nmds_df_sampling, aes(x = NMDS1, y = NMDS2, color = treatment)) +
  
  stat_ellipse(geom = "polygon", aes(fill = treatment),
               alpha = 0.2, level = 0.95, show.legend = FALSE) +
  geom_point(size = 2, aes(shape = treatment)) +
  geom_path(aes(group = treatment), linetype = "dotted", alpha = 0.6) +
  
  # Flechas desde el origen hasta los puntos NMDS1/NMDS2 de vecs
  geom_segment(
    data = vecs,
    aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
    arrow = arrow(length = unit(0.2, "cm")), color = "gray30"
  ) +
  geom_text_repel(
    data = vecs,
    aes(x = NMDS1, y = NMDS2, label = species),
    size = 3, color = "gray30"
  ) +
  
  geom_hline(yintercept = 0, color = "gray70", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "gray70", linetype = "dashed") +
  
  scale_color_manual(values = palette_CB, labels = labels) +
  scale_fill_manual(values = palette_CB, guide = FALSE) +
  scale_shape_manual(values = point_shapes, guide = FALSE) +
  
  annotate(
    "text", x = Inf, y = -Inf,
    label = paste0("Stress = ", round(nmds_bc_sampling$stress, 3)),
    hjust = 1.1, vjust = -0.5, size = 4, color = "gray20"
  ) +
  
  labs(
    title    = "NMDS Bray–Curtis (Hellinger) de composición de especies",
    subtitle = "Elipses al 95 % y vectores de especies (R² > 0.2)",
    x        = "NMDS1", y = "NMDS2",
    color    = "Tratamiento"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid      = element_blank(),
    plot.subtitle   = element_text(face = "italic")
  )

print(ggnmds_alltreatments)




test_adonis <- adonis2(dist_bc_hel ~ treatment,
        data        = sp_wide_sampling,
        permutations = 999,
        method       = "bray")


print(test_adonis)
# Hay un efecto significativo del tratamiento sobre la composición de especies (p = 0.001). 
# El tratamiento explica aproximadamente el 33.7% de la variación en la composición.

bd <- betadisper(dist_bc_hel, sp_wide_sampling$treatment)
anova(bd)
# El resultado de ANOVA para las dispersiónes dentro de grupos (tratamientos) es significativo (p = 0.0004).
# Esto significa que la variabilidad o dispersión dentro de al menos un grupo es diferente respecto a otros grupos.
permutest(bd) 
plot(bd)
boxplot(bd)

permutest(bd, pairwise = TRUE)
TukeyHSD(bd)


## install.packages("remotes")
## remotes::install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")

library(pairwiseAdonis)


# Ejecutamos las comparaciones por pares
pw_adonis <- pairwise.adonis(
  x           = dist_bc_hel,                 # tu matriz de distancias Hellinger–Bray
  factors     = sp_wide_sampling$treatment,  # factor con los cuatro tratamientos
  perm        = 999,                         # número de permutaciones
  p.adjust.m  = "BH"                         # corrección de p por Benjamini–Hochberg
)

print(pw_adonis)
