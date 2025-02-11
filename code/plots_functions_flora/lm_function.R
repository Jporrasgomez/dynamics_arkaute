


lm_function <- function(data, type) {

nind_c <- subset(nind_2, code == type & treatment == "c")

lm_c <- lm(nind_m2 ~ abundance, data = nind_c)

# Extract coefficients, R^2, and p-value
lm_c_summary <- summary(lm_c)
lm_c_tidy <- tidy(lm_c)
lm_c_glance <- glance(lm_c)

intercept_c <- lm_c_tidy$estimate[1]
slope_c <- lm_c_tidy$estimate[2]
r_squared_c <- lm_c_glance$r.squared
p_value_c <- lm_c_tidy$p.value[2]
n_observations_c <- nrow(nind_c)


c <- 
  ggplot(nind_c, aes(x = abundance, y = nind_m2)) +
  geom_point(aes(color = treatment), alpha = 0.5) +
  scale_colour_manual(values = c("c" = "green2", "w" = "red", "p" = "blue3", "wp" = "purple")) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = paste(type),
       subtitle = paste("y =", round(intercept_c, 2), "+", round(slope_c, 2), "* x\n",
                        "R2:", round(r_squared_c, 2), ", p-value:", round(p_value_c, 4), "\n",
                        "n observations", n_observations_c),
       x = "Abundance",
       y = "Numbers of individual per m2") +
  theme_minimal()+
  theme(legend.position = "null")



nind_w <- subset(nind_2, code == type & treatment == "w")

lm_w <- lm(nind_m2 ~ abundance, data = nind_w)

# Extract coefficients, R^2, and p-value
lm_w_summary <- summary(lm_w)
lm_w_tidy <- tidy(lm_w)
lm_w_glance <- glance(lm_w)

intercept_w <- lm_w_tidy$estimate[1]
slope_w <- lm_w_tidy$estimate[2]
r_squared_w <- lm_w_glance$r.squared
p_value_w <- lm_w_tidy$p.value[2]
n_observations_w <- nrow(nind_w)

w <- 
ggplot(nind_w, aes(x = abundance, y = nind_m2)) +
  geom_point(aes(color = treatment), alpha = 0.5) +
  scale_colour_manual(values = c("c" = "green2", "w" = "red", "p" = "blue3", "wp" = "purple")) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = paste(type),
       subtitle = paste("y =", round(intercept_w, 2), "+", round(slope_w, 2), "* x\n",
                        "R2:", round(r_squared_w, 2), ", p-value:", round(p_value_w, 4), "\n",
                        "n observations", n_observations_w),
       x = "Abundance",
       y = "Numbers of individual per m2") +
  theme_minimal()+
  theme(legend.position = "null")


nind_p <- subset(nind_2, code == type & treatment == "p")


lm_p <- lm(nind_m2 ~ abundance, data = nind_p)

# Extract coefficients, R^2, and p-value
lm_p_summary <- summary(lm_p)
lm_p_tidy <- tidy(lm_p)
lm_p_glance <- glance(lm_p)

intercept_p <- lm_p_tidy$estimate[1]
slope_p <- lm_p_tidy$estimate[2]
r_squared_p <- lm_p_glance$r.squared
p_value_p <- lm_p_tidy$p.value[2]
n_observations_p <- nrow(nind_p)

p <- 
ggplot(nind_p, aes(x = abundance, y = nind_m2)) +
  geom_point(aes(color = treatment), alpha = 0.5) +
  scale_colour_manual(values = c("c" = "green2", "w" = "red", "p" = "blue3", "wp" = "purple")) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = paste(type),
       subtitle = paste("y =", round(intercept_p, 2), "+", round(slope_p, 2), "* x\n",
                        "R2:", round(r_squared_p, 2), ", p-value:", round(p_value_p, 4), "\n",
                        "n observations", n_observations_p),
       x = "Abundance",
       y = "Numbers of individual per m2") +
  theme_minimal()+
  theme(legend.position = "null")



nind_wp <- subset(nind_2, code == type & treatment == "wp")

lm_wp <- lm(nind_m2 ~ abundance, data = nind_wp)

# Extract coefficients, R^2, and p-value
lm_wp_summary <- summary(lm_wp)
lm_wp_tidy <- tidy(lm_wp)
lm_wp_glance <- glance(lm_wp)

intercept_wp <- lm_wp_tidy$estimate[1]
slope_wp <- lm_wp_tidy$estimate[2]
r_squared_wp <- lm_wp_glance$r.squared
p_value_wp <- lm_wp_tidy$p.value[2]
n_observations_wp <- nrow(nind_wp)

wp <- 
ggplot(nind_wp, aes(x = abundance, y = nind_m2)) +
  geom_point(aes(color = treatment), alpha = 0.5) +
  scale_colour_manual(values = c("c" = "green2", "w" = "red", "p" = "blue3", "wp" = "purple")) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = paste(type),
       subtitle = paste("y =", round(intercept_wp, 2), "+", round(slope_wp, 2), "* x\n",
                        "R2:", round(r_squared_wp, 2), ", p-value:", round(p_value_wp, 4), "\n",
                        "n observations", n_observations_wp),
       x = "Abundance",
       y = "Numbers of individual per m2") +
  theme_minimal()+
  theme(legend.position = "null")


print(ggarrange(
  c, w, p, wp,
  labels = c ("C", "W", "P", "WP"), 
  nrow = 2, 
  ncol = 2
))

}

