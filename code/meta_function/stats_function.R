
stats <- function(data, response_variable, explanatory_variable) {
  
  
  variable_name <- unique(data$variable)

  
  gg_stats <- 
  ggarrange(
    ggplot(data, aes(x = .data[[response_variable]])) +
      geom_histogram(bins = 20, fill = "skyblue", color = "black") +
      labs(x = variable_name, y = "Frequency",
           title = paste0("Histogram ", variable_name) ) +
      theme(
        axis.text.x = element_text(color = "black", size = 10), 
        axis.title.x = element_text(color = "black", size = 12)
      ),
    
    ggplot(data, aes(sample = .data[[response_variable]])) +
      stat_qq() +
      stat_qq_line(color = "red") +
      labs(x = "Theoretical Quantiles", y = "Sample Quantiles",
           title = paste0("Q-Q plot of ", variable_name)) +
      theme(
        axis.text.x = element_text(color = "black", size = 10), 
        axis.title.x = element_text(color = "black", size = 12)
      ),
      
    nrow = 1, ncol = 2)
  
  gg_stats <<- gg_stats
  
  # Normality tests
  print(unique(data$variable))
  print("Normality tests: if p-value < 0.05 the response variable does not follow normal distribution")
  
  #Shapiro test
  print("Shapiro-Wilk test for normality:")
  shapiro_result <- shapiro.test(data[[response_variable]])
  print(shapiro_result)
  shapiro_pvalue <- shapiro_result$p.value
  
  #print("Kolmogorov-Smirnov test:")
  #print(ks.test(data[[response_variable]], "pnorm", mean = data$mean, sd = data$sd))
  
  print("Anderson-Darling test:")
  library(nortest)
  ad_result <- ad.test(data[[response_variable]])
  print(ad_result)
  anderson_darling_pvalue <- ad_result$p.value
  
  
  # To check homoscedasticity (homogeneity of variance)
  print("Levene's Test for homoscedasticity:")
  levene_result <- car::leveneTest(data[[response_variable]] ~ data[[explanatory_variable]], data = data)
  print(levene_result)
  # El p-valor está en la primera fila y columna "Pr(>F)"
  levene_pvalue <- levene_result$`Pr(>F)`[1]
  
  
  normality_df <- matrix(ncol = 2, nrow = 3)
  colnames(normality_df) <- c("normality_test", "p_value")
  normality_df[1,1] <- "Shapiro-Wilk"
  normality_df[2,1] <- "Anderson-Darling"
  normality_df[3,1] <- "Levene's(homoscedasticity)"
  
  normality_df[1,2] <- round(shapiro_pvalue, 6)
  normality_df[2,2] <- round(anderson_darling_pvalue, 6)
  normality_df[3,2] <- round(levene_pvalue , 6)
  
  normality_df <- as.data.frame(normality_df) %>% 
    mutate(normality_test = as.factor(normality_test),
           p_value = as.numeric(p_value))
  
  gg_normality_tests <- 
    ggplot(normality_df, aes(x = normality_test, y = p_value)) + 
    geom_point() + 
    geom_hline(yintercept = 0.05, linetype = "dashed", color = "blue") + 
    labs(title = variable_name)
  
  gg_normality_tests <<- gg_normality_tests
  
  print("If there is no normality: ")
  
  
  # Kruskal-Wallis test for differences between groups
  print("Kruskal-Wallis Test:")
  print(kruskal.test(data[[response_variable]] ~ data[[explanatory_variable]], data = data))
  
  # If Kruskal-Wallis is significant, we use Dunn's test to compare groups
  print("Dunn's Test for Pairwise Comparisons (Bonferroni):")
  dunn_rich <- dunn.test::dunn.test(data[[response_variable]], data[[explanatory_variable]], method = "bonferroni")
  print(dunn_rich)
  
  print(unique(data$variable))
  
  
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
   if (variable_name %in% c("richness", "abundance", "mu_log", "sigma_log")) {
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
    labs(x = NULL, y = tools::toTitleCase(data$variable), fill = explanatory_variable,
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
    labs( x = NULL, y = tools::toTitleCase(data$variable), fill = explanatory_variable, 
          title = "t.test results") +
    theme(legend.position = "none", text = element_text(size = 10))
  
  gg_ttest <<- gg_ttest
  
  
 
  
  par(mfrow = c(1, 1))
}
