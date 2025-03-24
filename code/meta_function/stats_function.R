
stats <- function(data, response_variable, explanatory_variable) {
  

  gg_stats <- 
  ggarrange(
    ggplot(data, aes(x = .data[[response_variable]])) +
      geom_histogram(bins = 20, fill = "skyblue", color = "black") +
      labs(x = response_variable, y = "Frequency",
           title = paste0("Histogram ", response_variable) ) +
      theme(
        axis.text.x = element_text(color = "black", size = 10), 
        axis.title.x = element_text(color = "black", size = 12)
      ),
    
    ggplot(data, aes(sample = .data[[response_variable]])) +
      stat_qq() +
      stat_qq_line(color = "red") +
      labs(x = "Theoretical Quantiles", y = "Sample Quantiles",
           title = paste0("Q-Q plot of ", response_variable)) +
      theme(
        axis.text.x = element_text(color = "black", size = 10), 
        axis.title.x = element_text(color = "black", size = 12)
      ),
      
    nrow = 1, ncol = 2)
  
  gg_stats <<- gg_stats
  
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
              fill = explanatory_variable) +
    stat_pvalue_manual(dunn_results, label = "p.adj.signif", tip.length = 0.01) +  # Add p-value labels from Dunn's Test
    scale_fill_manual(values = palette) +
    scale_x_discrete(labels = labels) +
    labs(x = NULL, y = tools::toTitleCase(response_variable), fill = explanatory_variable,
         title = "Dunn test results") +
    theme(legend.position = "none", text = element_text(size = 10))
  
  gg_dunn <<- gg_dunn 
  
  gg_ttest <- 
  ggboxplot(data,  x = explanatory_variable, y = response_variable,
            fill = explanatory_variable) +
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
  
  
 # # Realizar el ANOVA
 # anova_model <- aov(response_variable ~ explanatory_variable, data = data)
 # 
 # # Resumen del ANOVA
 # summary(anova_model)
 # 
 # # Realizar la prueba post-hoc Tukey HSD
 # tukey_results <- TukeyHSD(anova_model)
 # 
 # # Graficar el boxplot con los resultados de ANOVA y Tukey
 # gg_ttest <- 
 #   ggboxplot(data, x = explanatory_variable, y = response_variable,
 #             fill = explanatory_variable, alpha = 0.5) +
 #   stat_compare_means(method = "anova", label = "p.signif", tip.length = 0.01) +  # ANOVA test
 #   scale_fill_manual(values = palette) +
 #   scale_x_discrete(labels = labels) +
 #   labs(x = NULL, y = tools::toTitleCase(response_variable), fill = explanatory_variable, 
 #        title = "ANOVA with Tukey HSD Post-Hoc") +
 #   theme(legend.position = "none", text = element_text(size = 10))
  
  par(mfrow = c(1, 1))
}
