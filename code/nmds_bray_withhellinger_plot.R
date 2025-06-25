



library(vegan)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(pairwiseAdonis)

# 1. Hellinger
abundance_hel_plot <- decostand(abundance_data_plot, method = "hellinger")

# 2. Distancias y NMDS k = 3
dist_bc_hel_plot <- vegdist(abundance_hel_plot, method = "bray")
nmds_plot <- metaMDS(dist_bc_hel_plot, k = 3, trymax = 250, maxit = 999)

# 3. Envfit (vectores con R² > 0.2)

ef_plot <- envfit(
  nmds_plot ~ ., 
  data        = abundance_hel_plot, 
  perm        = 999,
  choices     = 1:3         # <— aquí pedimos que envfit ajuste los tres ejes
)

# 3.b) Filtrar las especies con R² > 0.2
sel_plot <- which(ef_plot$vectors$r > 0.2)

# 3.c) Extraer YA los 3 ejes en vecs_plot
vecs_plot <- scores(
  ef_plot, 
  display = "vectors"     # ya incluye ejes 1,2 y 3
)[sel_plot, , drop = FALSE] %>%
  as.data.frame() %>%
  rownames_to_column("species")

# 4. Preparar data.frame de coordinates
nmds_df_plot <- as_tibble(nmds_plot$points) %>%
  rename(NMDS1 = MDS1, NMDS2 = MDS2, NMDS3 = MDS3) %>%
  mutate(
    treatment = sp_wide_plot$treatment,
    sampling  = sp_wide_plot$sampling,
    plot      = sp_wide_plot$plot
  )

# 5. Estadísticos multivariados
perm_adonis_plot <- adonis2(dist_bc_hel_plot ~ treatment, data = sp_wide_plot, permutations = 999)
bd_plot         <- betadisper(dist_bc_hel_plot, sp_wide_plot$treatment)
bd_anova_plot   <- anova(bd_plot)
pw_disp_plot    <- permutest(bd_plot, pairwise = TRUE)
pw_adonis_plot  <- pairwise.adonis(dist_bc_hel_plot, sp_wide_plot$treatment, perm = 999, p.adjust.m = "BH")

print(perm_adonis_plot)
print(bd_anova_plot)
print(pw_disp_plot)
print(pw_adonis_plot)

# 6. Función para plotear ejes (1–2 y 1–3)

plot_nmds_pair <- function(df, xvar, yvar, stress, title_suffix){
  # Convierte xvar/yvar en símbolos
  x_sym <- sym(xvar)
  y_sym <- sym(yvar)
  
  ggplot(df, aes(x = !!x_sym, y = !!y_sym, color = treatment)) +
    stat_ellipse(
      geom = "polygon", aes(fill = treatment),
      alpha = 0.15, level = 0.95, show.legend = FALSE
    ) +
    geom_point(size = 2, aes(shape = treatment)) +
    geom_segment(
      data = vecs_plot,
      aes(x = 0, y = 0, xend = !!x_sym, yend = !!y_sym),
      arrow = arrow(length = unit(0.2, "cm")), color = "gray30"
    ) +
    geom_text_repel(
      data = vecs_plot,
      aes(x = !!x_sym, y = !!y_sym, label = species),
      size = 3, color = "gray30"
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
    annotate(
      "text", x = Inf, y = -Inf,
      label = paste0("Stress = ", round(stress, 3)),
      hjust = 1.1, vjust = -0.5, size = 4, color = "gray20"
    ) +
    scale_color_manual(values = palette_CB) +
    scale_fill_manual(values = palette_CB) +
    scale_shape_manual(values = point_shapes) +
    labs(
      title = paste("NMDS Bray–Curtis (Hellinger)", title_suffix),
      x     = xvar,
      y     = yvar,
      color = "Treatment"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      panel.grid      = element_blank()
    )
}

# Vuelves a plotear:
p12 <- plot_nmds_pair(nmds_df_plot, "NMDS1", "NMDS2", nmds_plot$stress, "NMDS1 and NMDS2")
print(p12)
# Plot ejes 1 vs 3 (comprobación de ordenación tridimensional)
p13 <- plot_nmds_pair(nmds_df_plot, "NMDS1", "NMDS3", nmds_plot$stress, "NMDS1 and NMDS3")
print(p13)
