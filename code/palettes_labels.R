



# THEMES




gg_RR_theme <- 
  theme(
    panel.grid      = element_blank(),
    strip.background = element_blank(),
    strip.text      = element_text(face = "bold"),
    text            = element_text(size = 13
    ),
    legend.position = "none",
    axis.text.y     = element_text(face = "bold",
                                   angle = 90,
                                   hjust = 0.5))

theme1 <- 
theme_set(theme_bw() +
            theme(
              legend.position = "right",
              panel.grid = element_blank(),
              strip.background = element_blank(),
              strip.text = element_text(face = "bold"),
              text = element_text(size = 11)))

theme2 <- 
theme(
  legend.position = "bottom",
  axis.text.x     = element_blank(),
  axis.ticks.x    = element_blank(),
  panel.grid = element_blank(),
  strip.background = element_blank(),
  strip.text = element_text(face = "bold"),
  text = element_text(size = 15)
)

theme3 <- 
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    text = element_text(size = 15)
  )


## Variable names

limits_variables <- c("richness",
                      "abundance",
                      "Y_zipf",
                      #"biomass",
                      "biomass012",
                      "NMDS1",
                      "NMDS2",
                      "PC1",
                      "PC2")

labels_variables <- c("richness" = "Richness",
                      "abundance" = "Cover",
                      "Y_zipf" = "Evenness",
                      #"biomass" = "Biomass",
                      "biomass012" = "Biomass",
                      "NMDS1" = "SC1",
                      "NMDS2" = "SC2",
                      "PC1" = "FT1", 
                      "PC2" = "FT2")




# SENSORS 

palette_sensor <- c("t_top" = "red", "t_bottom" = "purple", "t_ground" = "orange", "vwc" = "blue3")

palette_OTC = c("control" = "#1FBDC7", "otc"     = "#EA6E13")

labels_OTC = c("control" = "Without OTC", "otc" = "With OTC")

labels_variables_sensor <- c("t_top" = "Temp. 40 cm", "t_bottom"= "Temp. 2 cm", "t_ground" = "Temp. -6 cm", "vwc" = "VWC")





# TREATMENTS

c_CB = "#12D08C" # "CB" color blind
w_CB = "#E05050"
p_CB = "#00CAFF"
wp_CB = "#903996"



labels1 <- c("c" = "Control", "w" = "Warming", "p" = "Perturbation", "wp" = "Combined")
labels2 <- c("c" = "C", "w" = "W", "p" = "P", "wp" = "W+P")
labels3 <- c("c" = "Control", "w" = "Warming", "p" = "Perturbation", "wp" = "Global change")


palette_CB <- c("c" = c_CB, "w" = w_CB, "p" = p_CB, "wp" = wp_CB)

## Response ratio
labels_RR <- c("w_vs_c" = "Warming vs Control", "p_vs_c" = "Perturbation vs Control", "wp_vs_c" = "Global change vs Control")
labels_RR2 <- c("w_vs_c" = "Warming vs Control", "p_vs_c" = "Perturbation vs Control", "wp_vs_c" = "Combined vs Control")

palette_RR_CB <- c("w_vs_c" = w_CB, "p_vs_c" = p_CB, "wp_vs_c" = wp_CB)


palette_RR_wp <- c(wp_vs_p = "#903996", wp_vs_w = "#E05050")

labels_RR_wp <- c("wp_vs_w" = "Global Change vs Warming", "wp_vs_p" = "Global Change vs Perturbation")
labels_RR_wp2 <- c("wp_vs_w" = "Perturbation effect", "wp_vs_p" = "Warming effect")
labels_RR_wp3 <- c("wp_vs_w" = "Global Change vs Warming", "wp_vs_p" = "Combined vs Perturbation")




palette3 <- c("c" = "#1F8A8C", "w" = "#D93232", "p" = "#F4A300", "wp" = "#3A3A3A")


palette8 <- c("c" = "#00C4A7", "w" = "#A238A2", "p" = "#F4A300", "wp" = "#3A3A3A")




point_shapes <- c("c" = 16, "w" = 17, "p" = 15, "wp" = 18)




