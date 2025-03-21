

stats_wp <- function(data, response_variable, explanatory_variable) {

dunn_results <- 
  rstatix::dunn_test(
    data, formula = as.formula(paste(response_variable, "~", explanatory_variable)), p.adjust.method = "bonferroni")


# Compute the max value once
max_y <- max(data[[response_variable]], na.rm = TRUE)

# Define y.position based on the sign of max_y
if (response_variable %in% c("richness", "abundance", "mu_log", "sigma_log")) {
  dunn_results <- dunn_results %>%
    mutate(y.position = max_y * 1.5 +
             seq(0, by = max_y * 0.1, length.out = n()))
} else {
  dunn_results <- dunn_results %>%
    mutate(y.position = max_y + 
             abs(max_y) * 0.001 + 
             seq(0, by = (abs(max_y) + 1) * 0.3, length.out = n()))
}



# Crear el gráfico

gg_dunn <- 
  ggboxplot(data, x = explanatory_variable, y = response_variable,
            fill = explanatory_variable, alpha = 0.5) +
  stat_pvalue_manual(dunn_results, label = "p.adj.signif", tip.length = 0.01) +  # Add p-value labels from Dunn's Test
  scale_fill_manual(values = palette_wp) +
  scale_x_discrete(labels = labels_RR_wp) +
  labs(x = NULL, y = tools::toTitleCase(response_variable), fill = explanatory_variable,
       title = "Dunn test results") +
  theme(legend.position = "none", text = element_text(size = 10))

gg_dunn <<- gg_dunn 

gg_ttest <- 
  ggboxplot(data,  x = explanatory_variable, y = response_variable,
            fill = explanatory_variable, alpha = 0.5) +
  stat_compare_means(comparisons = list(c("c", "w"), c("c", "p"), c("c", "wp"),
                                        c("w", "p"), c("w", "wp"), c("p", "wp")),
                     method = "t.test",
                     label = "p.signif",
                     tip.length = 0.01) +  # Show significance stars (*, **, ***)
  scale_fill_manual(values = palette_wp) +
  scale_x_discrete(labels = labels_RR_wp) +
  labs( x = NULL, y = tools::toTitleCase(response_variable), fill = explanatory_variable, 
        title = "t.test results") +
  theme(legend.position = "none", text = element_text(size = 10))

gg_ttest <<- gg_ttest

}
