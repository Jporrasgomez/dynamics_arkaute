




theme_set(theme_bw() +
            theme(
              legend.position = "right",
              panel.grid = element_blank(),
              strip.background = element_blank(),
              strip.text = element_text(face = "bold"),
              text = element_text(size = 11)))


#favs:
palette5 <- c("c" = "#00C4A7", "w" = "#E0352F", "p" = "#0077FF", "wp" = "#A238A2")

palette3 <- c("c" = "#1F8A8C", "w" = "#D93232", "p" = "#F4A300", "wp" = "#3A3A3A")
palette8 <- c("c" = "#00C4A7", "w" = "#A238A2", "p" = "#F4A300", "wp" = "#3A3A3A")




palette_recodyn <- c(
  "c" = "#0F85A0", 
  "p" = "#E6B800",  # Amarillo más oscuro y con mejor visibilidad en blanco  
  "wp" = "#ED8B00", 
  "w" = "#DD4124"
)


palette_recodyn2 <- c(
  "c" = "#0F85A0", 
  "p" = "#D4A300",  # Amarillo dorado, más oscuro pero aún vibrante  
  "wp" = "#ED8B00", 
  "w" = "#DD4124"
)


palette_recodyn3 <- c(
  "c" = "#00496F", 
  "p" = "#EDD746", 
  "wp" = "#ED8B00", 
  "w" = "#DD4124"
)

palette_wp <- c("w" = "#D08A00", "p" = "#3A3A3A")
palette_wp_vs_treatment <- c("wp_vs_w" = "#D08A00", "wp_vs_p" = "#3A3A3A")



palette1 <- c("c" = "#5AC9A5", "w" = "#E56B57", "p" = "#4A92CF", "wp" = "#87528C")
palette2 <- c("c" = "#48A597", "w" = "#D94E47", "p" = "#4A92CF", "wp" = "#87528C")

palette4 <- c("c" = "#1F8A8C", "w" = "#D93232", "p" = "#4A92CF", "wp" = "#3A3A3A")

palette6 <- c("c" = "#00A83C", "w" = "#E0352F", "p" = "#0077FF", "wp" = "#A238A2")
palette7 <- c("c" = "#008F5F", "w" = "#E0352F", "p" = "#0077FF", "wp" = "#A238A2")




labels1 <- c("c" = "Control", "w" = "Warming", "p" = "Perturbation", "wp" = "Combined")
labels2 <- c("c" = "C", "w" = "W", "p" = "P", "wp" = "W+P")
labels3 <- c("c" = "Control", "w" = "Warming", "p" = "Perturbation", "wp" = "Global change")

labels_RR <- c("w" = "Warming vs Control", "p" = "Perturbation vs Control", "wp" = "Global change vs Control")

labels_variable <- c("richness" = "Richness", "abundance" = "Abundance", "Y_zipf" = expression("RAD coefficient ("*gamma*")"),
                     "biomass" = "Biomass")

labels_RR_wp <- c("wp_vs_w" = "Global Change vs Warming", "wp_vs_p" = "Global Change vs Perturbation")
labels_RR_wp2 <- c("wp_vs_w" = "Perturbation effect", "wp_vs_p" = "Warming effect")



point_shapes <- c("c" = 16, "w" = 17, "p" = 15, "wp" = 18)
