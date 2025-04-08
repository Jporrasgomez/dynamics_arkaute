


sampling_0 <- function(data, response_variable, explanatory_variable){
  
  
  mean_response_variable <- paste0("mean_", response_variable)
  sd_response_variable <- paste0("sd_", response_variable)
  cv_response_variable <- paste0("cv_", response_variable)
  
  data <- data %>% 
    distinct(treatment, plot, sampling, .data[[response_variable]], .keep_all = TRUE) %>% 
    group_by(treatment, sampling, ) %>% 
    mutate(
      n = n(),
      !!mean_response_variable := mean(.data[[response_variable]], na.rm = TRUE),
      !!sd_response_variable := sd(.data[[response_variable]], na.rm = TRUE)
    ) %>%
    mutate(
      !!cv_response_variable := .data[[sd_response_variable]] / .data[[mean_response_variable]]
    ) %>% 
    ungroup() %>% 
    select(treatment, sampling, plot, n,
           !!response_variable, !!mean_response_variable, !!sd_response_variable, !!cv_response_variable)
  
  data <- data %>% 
    filter(sampling == "0")
  
  
  
  dunn_results <- 
    rstatix::dunn_test(
      data, formula = as.formula(paste(response_variable, "~", explanatory_variable)), p.adjust.method = "bonferroni")
  
  
  # Compute the max value once
  max_y <- max(data[[response_variable]], na.rm = TRUE)
  
  # Define y.position based on the sign of max_y
  if (response_variable %in% c("richness", "abundance")) {
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
  
  gg_dunn_0 <- 
    ggboxplot(data, x = explanatory_variable, y = response_variable,
              fill = explanatory_variable, alpha = 0.5) +
    stat_pvalue_manual(dunn_results, label = "p.adj.signif", tip.length = 0.01) +  # Add p-value labels from Dunn's Test
    scale_fill_manual(values = palette) +
    scale_x_discrete(labels = labels) +
    labs(x = NULL, y = tools::toTitleCase(response_variable), fill = explanatory_variable,
         title = "Sampling 0 Dunn test results") +
    theme(legend.position = "none", text = element_text(size = 10))
  
  gg_dunn_0  <<- gg_dunn_0
  
  
  mean_0 <- data %>% 
    filter(sampling == "0") %>% 
    select(treatment,
           response_variable, 
           !!sym(paste0("mean_", response_variable)), 
           !!sym(paste0("sd_", response_variable))) %>% 
    distinct() %>% 
    mutate(
      max_value = !!sym(paste0("mean_", response_variable)) + !!sym(paste0("sd_", response_variable)),
      min_value = !!sym(paste0("mean_", response_variable)) - !!sym(paste0("sd_", response_variable))
    ) %>% 
    as.data.frame() %>% 
    ggplot(aes(x = treatment, y = !!sym(paste0("mean_", response_variable)), color = treatment)) + 
    geom_point(size = 4, fill = "white" )+
    geom_point(aes(x = treatment, y = !!sym(response_variable))) + 
    geom_errorbar(aes(ymin = max_value,
                      ymax = min_value,
                      color = treatment), width = 0.5, linewidth = 0.3) + 
    scale_color_manual(values = palette) + 
    scale_x_discrete(labels = labels) + 
    labs(y = paste0("Mean ", response_variable), title = "Sampling 0")
  
  mean_0 <<- mean_0

  
}
