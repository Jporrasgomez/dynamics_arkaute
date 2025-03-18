
stats <- function(data, response_variable, explanatory_variable) {
  
  # Histograma
  print("Histogram:")
  hist(data[[response_variable]], breaks = 20)
  
  # Q-Q plot
  print("Q-Q Plot:")
  qqnorm(data[[response_variable]])  # Creates the Q-Q plot
  qqline(data[[response_variable]], col = "red", lwd = 2)  # Adds a reference line
  
  # Normality tests
  print("Normality tests: if p-value < 0.05 the response variable does not follow normal distribution")
  print("Shapiro-Wilk test for normality:")
  print(shapiro.test(data[[response_variable]]))
  
  print("Kolmogorov-Smirnov test:")
  print(ks.test(data[[response_variable]], "pnorm", mean = mean(data[[response_variable]]), sd = sd(data[[response_variable]])))
  
  print("Anderson-Darling test:")
  library(nortest)
  print(ad.test(data[[response_variable]]))
  
  # To check homoscedasticity (homogeneity of variance)
  print("Levene's Test for homoscedasticity:")
  print(car::leveneTest(data[[response_variable]] ~ data[[explanatory_variable]], data = data))
  
  print("If there is no normality: ")
  # Kruskal-Wallis test for differences between groups
  print("Kruskal-Wallis Test:")
  print(kruskal.test(data[[response_variable]] ~ data[[explanatory_variable]], data = data))
  
  # If Kruskal-Wallis is significant, we use Dunn's test to compare groups
  print("Dunn's Test for Pairwise Comparisons (Bonferroni):")
  dunn_rich <- dunn.test::dunn.test(data[[response_variable]], data[[explanatory_variable]], method = "bonferroni")
  print(dunn_rich)
  
  
  
  print("If there is normality: ")
  # ANOVA
  print("ANOVA Results:")
  aov_result <- aov(data[[response_variable]] ~ data[[explanatory_variable]], data = data)
  print(summary(aov_result))
  
  # If ANOVA is significant, perform post-hoc tests (t-tests) for pairwise comparisons
  print("Post-hoc t-tests (Pairwise Comparisons with Bonferroni adjustment):")
  posthoc_result <- pairwise.t.test(data[[response_variable]], data[[explanatory_variable]], p.adjust.method = "bonferroni")
  print(posthoc_result)
  
  
  library(rstatix)
  
  # Realizar la prueba de Dunn y agregar la columna y.position
  
  dunn_results <- 
    rstatix::dunn_test(
      data, formula = as.formula(paste(response_variable, "~", explanatory_variable)), p.adjust.method = "bonferroni")
  dunn_results <- dunn_results %>%
    mutate(y.position = max(data[[response_variable]]) + seq(1, by = 2, length.out = nrow(dunn_results)))
  
  # Crear el gráfico
  
  gg_dunn <- 
    ggboxplot(data, x = explanatory_variable, y = response_variable,
              fill = explanatory_variable, alpha = 0.5) +
    stat_pvalue_manual(dunn_results, label = "p.adj.signif", tip.length = 0.01) +  # Add p-value labels from Dunn's Test
    scale_fill_manual(values = palette) +
    scale_x_discrete(labels = labels) +
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
    scale_fill_manual(values = palette) +
    scale_x_discrete(labels = labels) +
    labs( x = NULL, y = tools::toTitleCase(response_variable), fill = explanatory_variable, 
          title = "t.test results") +
    theme(legend.position = "none", text = element_text(size = 10))
  
  gg_ttest <<- gg_ttest
  
  
}
