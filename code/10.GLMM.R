


# =================================================================================
# 0. Clear environment
# =================================================================================
rm(list = ls(all.names = TRUE))

# =================================================================================
# 1. Load packages
# =================================================================================
pacman::p_load(
  dplyr, reshape2, tidyverse, lubridate,
  ggplot2, ggpubr, gridExtra,
  car, ggsignif, dunn.test, rstatix,
  lme4, nlme, glmmTMB, performance,
  emmeans, DHARMa
)

# Custom palettes and labels
source("code/palettes_labels.R")

# Global ggplot2 theme
theme_set(
  theme_bw() +
    theme(
      legend.position    = "right",
      panel.grid         = element_blank(),
      strip.background   = element_blank(),
      strip.text         = element_text(face = "bold"),
      text               = element_text(size = 11)
    )
)

# =================================================================================
# 2. Data import & preprocessing
# =================================================================================
arkaute <- read.csv("data/arkaute.csv") %>%
  mutate(
    year             = factor(year),
    date             = ymd(date),
    omw_date         = factor(omw_date),
    one_month_window = factor(one_month_window),
    sampling         = factor(sampling),
    plot             = factor(plot),
    treatment        = factor(treatment)
  ) %>%
  # drop pre-sampling and noisy first sampling for p & wp
  filter(sampling != "0") %>%
  filter(!(sampling == "1" & treatment %in% c("p", "wp")))

arkaute_norm <- read.csv("data/arkaute_norm_all.csv") %>%
  mutate(
    year             = factor(year),
    date             = ymd(date),
    omw_date         = factor(omw_date),
    one_month_window = factor(one_month_window),
    sampling         = factor(sampling),
    plot             = factor(plot),
    treatment        = factor(treatment)
  )

# =================================================================================
# 3. Exploratory check: collinearity of mean_vwc and treatment
# =================================================================================
# Boxplots
ggplot(arkaute, aes(treatment, mean_vwc, fill = treatment)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.3) +
  labs(title = "Soil moisture by treatment",
       x = "Treatment", y = "mean_vwc")

ggplot(arkaute, aes(plot, mean_vwc)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Soil moisture by plot",
       x = "Plot", y = "mean_vwc")

# Linear model R²
colmod <- lm(mean_vwc ~ treatment, data = arkaute)
summary(colmod)  # R² ~0.08

# =================================================================================
# 4. Diagnostic helper functions
# =================================================================================

# 4.1 Overdispersion test for nlme::lme
test_overdispersion_lme <- function(model) {
  resid_p <- residuals(model, type = "pearson")
  rdf     <- length(resid_p) - length(fixef(model))
  chi2    <- sum(resid_p^2)
  ratio   <- chi2 / rdf
  p_value <- pchisq(chi2, df = rdf, lower.tail = FALSE)
  cat("Overdispersion test:\n")
  cat("  Chi2 =", round(chi2, 2),
      "on", rdf, "df → dispersion =", round(ratio, 2), "\n")
  cat("  P =", signif(p_value, 3), "\n")
}

# 4.2 GLMM diagnostics via DHARMa
diagnose_glmm <- function(model) {
  print(summary(model))
  sim <- simulateResiduals(model)
  plot(sim)
  print(testDispersion(sim))
  invisible(sim)
}

# 4.3 LME diagnostics: Q–Q, resid vs fit, Levene, manual dispersion
diagnose_lme <- function(model, data, group_var = "treatment") {
  # Q–Q plot
  qqnorm(resid(model, type = "normalized"),
         main = "Q–Q Plot of Normalized Residuals")
  qqline(resid(model, type = "normalized"),
         col = "red", lwd = 2)
  # Residuals vs fitted
  plot(fitted(model), resid(model, type = "normalized"),
       main = "Residuals vs Fitted",
       xlab = "Fitted values", ylab = "Normalized residuals")
  abline(h = 0, lty = 2)
  # Levene’s test for homogeneity
  rv <- resid(model, type = "normalized")
  grp <- data[[group_var]]
  print(car::leveneTest(rv ~ grp))
  # Manual overdispersion
  test_overdispersion_lme(model)
  invisible(NULL)
}

# =================================================================================
# 5. Modeling workflow
#    For each response: GLMM → diagnose_glmm() → emmeans
#                      LME  → diagnose_lme()  → emmeans
# =================================================================================

# 5.1 Richness
glmm_richness <- glmmTMB(
  richness ~ treatment + scale(mean_vwc) + (1 | plot),
  data   = arkaute,
  family = nbinom2(link = "log")
)
diagnose_glmm(glmm_richness)
emmeans(glmm_richness, pairwise ~ treatment)

lme_richness <- lme(
  richness ~ treatment + scale(mean_vwc),
  random = ~ 1 | plot,
  data   = arkaute_norm
)
diagnose_lme(lme_richness, arkaute_norm)
emmeans(lme_richness, pairwise ~ treatment)

# 5.2 Abundance
# GLMM Gamma
glmm_abundance <- glmmTMB(
  abundance ~ treatment * scale(mean_vwc) + (1 | plot),
  data   = arkaute,
  family = Gamma(link = "log")
)
diagnose_glmm(glmm_abundance)

# GLMM Tweedie
glmm_abundance_tw <- glmmTMB(
  abundance ~ treatment + scale(mean_vwc) + (1 | plot),
  data   = arkaute,
  family = tweedie(link = "log")
)
diagnose_glmm(glmm_abundance_tw)

# GLMM log(abundance+1)
arkaute <- arkaute %>% mutate(log_abundance = log(abundance + 1))
glmm_abundance_log <- glmmTMB(
  log_abundance ~ treatment + scale(mean_vwc) + (1 | plot),
  data   = arkaute,
  family = gaussian()
)
diagnose_glmm(glmm_abundance_log)
emmeans(glmm_abundance_log, pairwise ~ treatment, type = "response")

# LME
lme_abundance <- lme(
  abundance ~ treatment + scale(mean_vwc),
  random = ~ 1 | plot,
  data   = arkaute_norm
)
diagnose_lme(lme_abundance, arkaute_norm)
emmeans(lme_abundance, pairwise ~ treatment)

# 5.3 Evenness (Y_zipf)
arkaute_Yzipf      <- arkaute %>% filter(!is.na(Y_zipf))
arkaute_norm_Yzipf <- arkaute_norm %>% filter(!is.na(Y_zipf))

glmm_evenness <- glmmTMB(
  Y_zipf ~ treatment + scale(mean_vwc) + (1 | plot),
  data   = arkaute_Yzipf,
  family = gaussian()
)
diagnose_glmm(glmm_evenness)
emmeans(glmm_evenness, pairwise ~ treatment)

lme_evenness <- lme(
  Y_zipf ~ treatment + scale(mean_vwc),
  random = ~ 1 | plot,
  data   = arkaute_norm_Yzipf
)
diagnose_lme(lme_evenness, arkaute_norm_Yzipf)
emmeans(lme_evenness, pairwise ~ treatment)

# 5.4 Biomass
arkaute_biomass      <- arkaute %>% filter(!is.na(biomass))
arkaute_norm_biomass <- arkaute_norm %>% filter(!is.na(biomass))

glmm_biomass <- glmmTMB(
  biomass ~ treatment + scale(mean_vwc) + (1 | plot),
  data   = arkaute_biomass,
  family = Gamma(link = "log")
)
diagnose_glmm(glmm_biomass)
emmeans(glmm_biomass, pairwise ~ treatment, type = "response")

lme_biomass <- lme(
  biomass ~ treatment + scale(mean_vwc),
  random = ~ 1 | plot,
  data   = arkaute_norm_biomass
)
diagnose_lme(lme_biomass, arkaute_norm_biomass)
emmeans(lme_biomass, pairwise ~ treatment)

# 5.5 Biomass012
glmm_biomass012 <- glmmTMB(
  biomass012 ~ treatment + scale(mean_vwc) + (1 | plot),
  data   = arkaute,
  family = Gamma(link = "log")
)
diagnose_glmm(glmm_biomass012)
emmeans(glmm_biomass012, pairwise ~ treatment, type = "response")

lme_biomass012 <- lme(
  biomass012 ~ treatment + scale(mean_vwc),
  random = ~ 1 | plot,
  data   = arkaute_norm
)
diagnose_lme(lme_biomass012, arkaute_norm)
emmeans(lme_biomass012, pairwise ~ treatment)

# 5.6 NMDS1
arkaute_NMDS1      <- arkaute %>% filter(!is.na(NMDS1))
arkaute_norm_NMDS1 <- arkaute_norm %>% filter(!is.na(NMDS1))

glmm_NMDS1 <- glmmTMB(
  NMDS1 ~ treatment + scale(mean_vwc) + (1 | plot),
  data   = arkaute_NMDS1,
  family = Gamma(link = "log")
)
diagnose_glmm(glmm_NMDS1)
emmeans(glmm_NMDS1, pairwise ~ treatment, type = "response")

lme_NMDS1 <- lme(
  NMDS1 ~ treatment + scale(mean_vwc),
  random = ~ 1 | plot,
  data   = arkaute_norm_NMDS1
)
diagnose_lme(lme_NMDS1, arkaute_norm_NMDS1)
emmeans(lme_NMDS1, pairwise ~ treatment)

# 5.7 NMDS2
arkaute_NMDS2      <- arkaute %>% filter(!is.na(NMDS2))
arkaute_norm_NMDS2 <- arkaute_norm %>% filter(!is.na(NMDS2))

glmm_NMDS2 <- glmmTMB(
  NMDS2 ~ treatment + scale(mean_vwc) + (1 | plot),
  data   = arkaute_NMDS2,
  family = Gamma(link = "log")
)
diagnose_glmm(glmm_NMDS2)
emmeans(glmm_NMDS2, pairwise ~ treatment, type = "response")

lme_NMDS2 <- lme(
  NMDS2 ~ treatment + scale(mean_vwc),
  random = ~ 1 | plot,
  data   = arkaute_norm_NMDS2
)
diagnose_lme(lme_NMDS2, arkaute_norm_NMDS2)
emmeans(lme_NMDS2, pairwise ~ treatment)

# 5.8 PC1
arkaute_PC1      <- arkaute %>% filter(!is.na(PC1))
arkaute_norm_PC1 <- arkaute_norm %>% filter(!is.na(PC1))

model_PC1 <- glmmTMB(
  PC1 ~ treatment + scale(mean_vwc) + (1 | plot),
  data   = arkaute_PC1,
  family = tweedie(link = "log")
)
diagnose_glmm(model_PC1)
emmeans(model_PC1, pairwise ~ treatment, type = "response")

lme_PC1 <- lme(
  PC1 ~ treatment + scale(mean_vwc),
  random = ~ 1 | plot,
  data   = arkaute_norm_PC1
)
diagnose_lme(lme_PC1, arkaute_norm_PC1)
emmeans(lme_PC1, pairwise ~ treatment)

# 5.9 PC2
arkaute_PC2      <- arkaute %>% filter(!is.na(PC2))
arkaute_norm_PC2 <- arkaute_norm %>% filter(!is.na(PC2))

model_PC2 <- glmmTMB(
  PC2 ~ treatment + scale(mean_vwc) + (1 | plot),
  data   = arkaute_PC2,
  family = tweedie(link = "log")
)
diagnose_glmm(model_PC2)
emmeans(model_PC2, pairwise ~ treatment, type = "response")

lme_PC2 <- lme(
  PC2 ~ treatment + scale(mean_vwc),
  random = ~ 1 | plot,
  data   = arkaute_norm_PC2
)
diagnose_lme(lme_PC2, arkaute_norm_PC2)
emmeans(lme_PC2, pairwise ~ treatment)

# =================================================================================
# End of script
# =================================================================================

