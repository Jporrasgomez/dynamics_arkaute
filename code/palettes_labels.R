




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


palette1 <- c("c" = "#5AC9A5", "w" = "#E56B57", "p" = "#4A92CF", "wp" = "#87528C")
palette2 <- c("c" = "#48A597", "w" = "#D94E47", "p" = "#4A92CF", "wp" = "#87528C")

palette4 <- c("c" = "#1F8A8C", "w" = "#D93232", "p" = "#4A92CF", "wp" = "#3A3A3A")

palette6 <- c("c" = "#00A83C", "w" = "#E0352F", "p" = "#0077FF", "wp" = "#A238A2")
palette7 <- c("c" = "#008F5F", "w" = "#E0352F", "p" = "#0077FF", "wp" = "#A238A2")



labels1 <- c("c" = "Control", "w" = "Warming", "p" = "Perturbation", "wp" = "Combined")
labels2 <- c("c" = "C", "w" = "W", "p" = "P", "wp" = "W+P")
labels3 <- c("c" = "Control", "w" = "Warming", "p" = "Perturbation", "wp" = "Global change")

point_shapes <- c("c" = 16, "w" = 17, "p" = 15, "wp" = 18)