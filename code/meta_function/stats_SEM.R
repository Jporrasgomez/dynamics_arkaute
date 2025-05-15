


stats_SEM <- function(data, response_variable, explanatory_variable) {
  
  
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
  print(shapiro_result$p.value)

}