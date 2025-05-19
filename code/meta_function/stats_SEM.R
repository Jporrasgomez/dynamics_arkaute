


stats_SEM <- function(data, response_variable, explanatory_variable) {
  

    
    
    variable_name <- unique(data$variable)
    treatment_name <- unique(data$treatment)
    
    
    gg_stats <- 
      ggarrange(
        ggplot(data, aes(x = .data[[response_variable]])) +
          geom_histogram(bins = 20, fill = "skyblue", color = "black") +
          labs(x = variable_name, y = "Frequency",
               title = paste0("Histogram ", variable_name, treatment_name) ) +
          theme(
            axis.text.x = element_text(color = "black", size = 10), 
            axis.title.x = element_text(color = "black", size = 12)
          ),
        
        ggplot(data, aes(sample = .data[[response_variable]])) +
          stat_qq() +
          stat_qq_line(color = "red") +
          labs(x = "Theoretical Quantiles", y = "Sample Quantiles",
               title = paste0("Q-Q plot of ", variable_name, treatment_name)) +
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
    
    
    normality_df <- matrix(ncol = 2, nrow = 2)
    colnames(normality_df) <- c("normality_test", "p_value")
    normality_df[1,1] <- "Shapiro-Wilk"
    normality_df[2,1] <- "Anderson-Darling"
  
    
    normality_df[1,2] <- round(shapiro_pvalue, 6)
    normality_df[2,2] <- round(anderson_darling_pvalue, 6)

    
    normality_df <- as.data.frame(normality_df) %>% 
      mutate(normality_test = as.factor(normality_test),
             p_value = as.numeric(p_value))
    
    gg_normality_tests <- 
      ggplot(normality_df, aes(x = normality_test, y = p_value)) + 
      geom_point() + 
      geom_hline(yintercept = 0.05, linetype = "dashed", color = "blue") + 
      labs(title = variable_name)
    
    gg_normality_tests <<- gg_normality_tests

}