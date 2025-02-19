




# Let's going to try glms with poaceae

nind_lm_data_poaceae <- matrix(nrow = length(code_levels), ncol = 7)
colnames(nind_lm_data) <- c("code", "intercept", "slope", "r_squared", "p_value", "n_points_lm", "shapiro_pvalue")
nind_lm_data <- as.data.frame(nind_lm_data)

hist(nind$nind_m2)

nind_i <- subset(nind, code == "poaceae")
lm_i <- lm(nind_m2 ~ abundance, data = nind_i)
hist(nind_i$nind_m2)


mean(nind_i$nind_m2)
var(nind_i$nind_m2)
print(c(mean_nind, var_nind))

glm_i <- glm(nind_m2 ~ abundance, data = nind_i, family = Gamma (link = "log"))

glm_poisson <- glm(nind_m2 ~ abundance, data = nind_i, family = poisson (link = "log"))
glm_gamma <- glm(nind_m2 ~ abundance, data = nind_i, family = Gamma (link = "log"))
glm_gaussian <- glm(nind_m2 ~ abundance, data = nind_i, family = gaussian(link = "log"))

AIC(glm_gamma)
AIC(glm_poisson)
AIC(glm_gaussian)

cooks_distance <- cooks.distance(glm_gamma)
plot(cooks_distance, type = "h", main = "Cook's Distance")

plot(glm_i)

residuals_glm <- residuals(glm_i, type = "pearson")  # Pearson residuals
plot(nind_i$abundance, residuals_glm, main = "Residuals vs Abundance")
abline(h = 0, col = "red")

library(pscl)
pR2(glm_i)

AIC(glm_i)
BIC(glm_i)

glm_i_summary <- summary(glm_gamma)
glm_i_summary$coefficients[, 4]
coef(glm_i)[1]


shapiro_i <- shapiro.test(residuals(lm_i))

# Extract coefficients, R^2, and p-value
lm_i_summary <- summary(lm_i)
lm_i_tidy <- tidy(lm_i)
lm_i_glance <- glance(lm_i)

intercept_i <- lm_i_tidy$estimate[1]
slope_i <- lm_i_tidy$estimate[2]
r_squared_i <- lm_i_glance$r.squared
p_value_i <- lm_i_tidy$p.value[2]
n_observations_i <- nrow(nind_i)

ggplot(nind_i, aes(x = abundance, y = nind_m2)) +
  geom_point(aes(color = treatment), alpha = 0.5) +
  scale_colour_manual(values = c("c" = "green2", "w" = "red", "p" = "blue3", "wp" = "purple")) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = paste("LM poaceae"),
       subtitle = paste("Equation: y =", round(intercept_i, 2), "+", round(slope_i, 2), "* x\n",
                        "R2:", round(r_squared_i, 2), ", p-value:", round(p_value_i, 4), "\n",
                        "n observations", n_observations_i),
       x = "Abundance",
       y = "Numbers of individual per m2")+
  theme_minimal()

nind_lm_data$code[counter] <- "poaceae"
nind_lm_data$intercept[counter] <- intercept_i
nind_lm_data$slope[counter] <- slope_i
nind_lm_data$r_squared[counter] <- r_squared_i
nind_lm_data$p_value[counter] <- p_value_i
nind_lm_data$n_points_lm[counter] <- n_observations_i
nind_lm_data$shapiro_pvalue[counter] <- shapiro_i$p.value




code_levels <- unique(nind$code)
gglist <- list()

nind_glm_data <- matrix(nrow = length(code_levels), ncol = 7)
colnames(nind_lm_data) <- c("code", "intercept", "slope", "r_squared", "p_value", "n_points_lm", "shapiro_pvalue")
nind_lm_data <- as.data.frame(nind_lm_data)

counter <- 0

library(broom)

for (i in 1: length(code_levels)) {
  
  nind_i <- subset(nind, code == code_levels[i])
  lm_i <- lm(nind_m2 ~ sqrt(abundance), data = nind_i)
  
  shapiro_i <- shapiro.test(residuals(lm_i))
  
  # Extract coefficients, R^2, and p-value
  lm_i_summary <- summary(lm_i)
  lm_i_tidy <- tidy(lm_i)
  lm_i_glance <- glance(lm_i)
  
  intercept_i <- lm_i_tidy$estimate[1]
  slope_i <- lm_i_tidy$estimate[2]
  r_squared_i <- lm_i_glance$r.squared
  p_value_i <- lm_i_tidy$p.value[2]
  n_observations_i <- nrow(nind_i)
  
  counter <- counter + 1
  
  gglist[[counter]] <- ggplot(nind_i, aes(x = abundance, y = nind_m2)) +
    geom_point(aes(color = treatment), alpha = 0.5) +
    scale_colour_manual(values = c("c" = "green2", "w" = "red", "p" = "blue3", "wp" = "purple")) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    labs(title = paste("LM ", code_levels[i]),
         subtitle = paste("Equation: y =", round(intercept_i, 2), "+", round(slope_i, 2), "* x\n",
                          "R2:", round(r_squared_i, 2), ", p-value:", round(p_value_i, 4), "\n",
                          "n observations", n_observations_i),
         x = "Abundance",
         y = "Numbers of individual per m2") +
    theme_minimal()
  
  nind_lm_data$code[counter] <- code_levels[i]
  nind_lm_data$intercept[counter] <- intercept_i
  nind_lm_data$slope[counter] <- slope_i
  nind_lm_data$r_squared[counter] <- r_squared_i
  nind_lm_data$p_value[counter] <- p_value_i
  nind_lm_data$n_points_lm[counter] <- n_observations_i
  nind_lm_data$shapiro_pvalue[counter] <- shapiro_i$p.value
  
  
}