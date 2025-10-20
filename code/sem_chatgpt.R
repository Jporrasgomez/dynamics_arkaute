




# =========================================================
# GLMM piecewise SEM per treatment (c, w, p, wp)
# Families chosen from data properties:
#   - biomass_lm_plot: Tweedie(log) (right-skewed with some zeros)
#   - Y_zipf: Gaussian(identity) (continuous, can be negative)
# Includes DHARMa diagnostics
# Requires: glmmTMB, piecewiseSEM (>= 2.3; uses coefs()), DHARMa, dplyr, lubridate
# =========================================================

# ---- packages ----
pkgs <- c("dplyr", "lubridate", "glmmTMB", "DHARMa", "piecewiseSEM")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)

library(dplyr)
library(lubridate)
library(glmmTMB)
library(DHARMa)
library(piecewiseSEM)

# ---- data ----
dat <- read.csv("data/arkaute.csv") %>%
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
  filter(!(sampling == "1" & treatment %in% c("p", "wp"))) %>%
  # keep only variables we need; drop rows with missing predictors/responses
  select(treatment, plot, richness, Y_zipf, LDMC, SLA, biomass_lm_plot) %>%
  na.omit()

# quick sanity checks (optional, informative only)
message("N by treatment:")
print(table(dat$treatment))
message("Zeros in biomass_lm_plot by treatment:")
print(tapply(dat$biomass_lm_plot, dat$treatment, function(x) sum(x == 0)))

# ---- helper: fit models + SEM + diagnostics for one treatment ----
fit_sem_for_treatment <- function(d_sub, treat_label = "Unknown") {
  message("\n==============================")
  message("Treatment: ", treat_label)
  message("==============================")
  
  # GLMM 1: biomass response (Tweedie with log link)
  # Handles right-skewed positive responses with some exact zeros
  mod1 <- glmmTMB(
    biomass_lm_plot ~ richness + Y_zipf + LDMC + SLA + (1 | plot),
    data   = d_sub,
    family = tweedie(link = "log")
  )
  
  # GLMM 2: Y_zipf response (Gaussian with identity link)
  mod2 <- glmmTMB(
    Y_zipf ~ richness + (1 | plot),
    data   = d_sub,
    family = gaussian(link = "identity")
  )
  
  # Build the SEM
  sem_fit <- psem(
    mod1,
    mod2
  )
  
  # ---- summaries ----
  message("\n--- piecewiseSEM summary ---")
  print(summary(sem_fit))
  
  message("\n--- Standardized coefficients (coefs, standardized = 'scale') ---")
  print(coefs(sem_fit, standardize = "scale"))
  
  message("\n--- Component / conditional R^2 ---")
  print(rsquared(sem_fit))
  
  message("\n--- Fisher's C (overall test of directed separation) ---")
  print(fisherC(sem_fit))
  
  # ---- DHARMa diagnostics ----
  message("\n--- DHARMa diagnostics: mod1 (biomass ~ ...) ---")
  res1 <- simulateResiduals(mod1, n = 1000)
  print(testResiduals(res1))          # uniformity test
  print(testDispersion(res1))         # dispersion test
  print(testZeroInflation(res1))      # zero-inflation test
  # Uncomment to plot if running interactively:
  # plot(res1)
  
  message("\n--- DHARMa diagnostics: mod2 (Y_zipf ~ richness) ---")
  res2 <- simulateResiduals(mod2, n = 1000)
  print(testResiduals(res2))
  print(testDispersion(res2))
  # Zero inflation doesn't really apply to Gaussian, but DHARMa allows the call:
  # print(testZeroInflation(res2))
  # plot(res2)
  
  # ---- optional: quick SEM path plot ----
  # (piecewiseSEM::plot tries to build a path diagram; works best with clean names)
  # If you prefer a title, pass the 'title' argument:
  # p <- plot(sem_fit, title = paste0("SEM (", treat_label, ")"))
  # print(p)
  
  invisible(list(mod1 = mod1, mod2 = mod2, sem = sem_fit,
                 dharma1 = res1, dharma2 = res2))
}

# ---- split by treatment and run ----
# Map treatment codes to friendly labels (optional)
label_map <- c(c = "Control", w = "Warming", p = "Perturbed", wp = "Combined")

fits <- list()
for (tr in c("c", "w", "p", "wp")) {
  d_sub <- dat %>% filter(treatment == tr)
  if (nrow(d_sub) == 0) {
    message("No rows for treatment: ", tr)
    next
  }
  fits[[tr]] <- fit_sem_for_treatment(d_sub, treat_label = label_map[[tr]])
}

# After running, you can access, for example:
# coefs(fits[["c"]]$sem, standardize = "scale")
# fisherC(fits[["c"]]$sem)
# rsquared(fits[["c"]]$sem)
