



theme_set(theme_bw() +
            theme(
              legend.position = "right",
              panel.grid = element_blank(),
              strip.background = element_blank(),
              strip.text = element_text(face = "bold"),
              text = element_text(size = 11)))



labels1 <- c("c" = "Control", "w" = "Warming", "p" = "Perturbation", "wp" = "Combined")
labels2 <- c("c" = "C", "w" = "W", "p" = "P", "wp" = "W+P")
labels3 <- c("c" = "Control", "w" = "Warming", "p" = "Perturbation", "wp" = "Global change")

#favs:
palette5 <- c("c" = "#00C4A7", "w" = "#E0352F", "p" = "#0077FF", "wp" = "#A238A2")
palette_CB <- c("c" = "#12D08C", "w" = "#E05050", "p" = "#00CAFF", "wp" = "#903996")


labels_RR <- c("w_vs_c" = "Warming vs Control", "p_vs_c" = "Perturbation vs Control", "wp_vs_c" = "Global change vs Control")
labels_RR2 <- c("w_vs_c" = "Warming vs Control", "p_vs_c" = "Perturbation vs Control", "wp_vs_c" = "Combined vs Control")
palette_RR <- c("w_vs_c" = "#E0352F", "p_vs_c" = "#0077FF", "wp_vs_c" = "#A238A2")
palette_RR_CB <- c("w_vs_c" = "#E05050", "p_vs_c" = "#00CAFF", "wp_vs_c" = "#903996")


palette_RR_wp <- c("wp_vs_w" = "#E05050", "wp_vs_p" = "#903996")
labels_RR_wp <- c("wp_vs_w" = "Global Change vs Warming", "wp_vs_p" = "Global Change vs Perturbation")
labels_RR_wp2 <- c("wp_vs_w" = "Perturbation effect", "wp_vs_p" = "Warming effect")
labels_RR_wp3 <- c("wp_vs_w" = "Global Change vs Warming", "wp_vs_p" = "Combined vs Perturbation")




palette3 <- c("c" = "#1F8A8C", "w" = "#D93232", "p" = "#F4A300", "wp" = "#3A3A3A")
palette8 <- c("c" = "#00C4A7", "w" = "#A238A2", "p" = "#F4A300", "wp" = "#3A3A3A")




##
##
##
##
point_shapes <- c("c" = 16, "w" = 17, "p" = 15, "wp" = 18)




