

library(piecewiseSEM) nlme
library(nlme)


arkaute <- read.csv("data/arkaute_norm.csv") %>% 
  mutate(
    year = as.factor(year),
    date = ymd(date),
    omw_date = as.factor(omw_date),
    one_month_window = as.factor(one_month_window),
    sampling = as.factor(sampling),
    plot = as.factor(plot),
    treatment = as.factor(treatment))




# Model
#mod1 = lme(DOC ~ plast_tot, random = ~ 1 | mesocosm/day, na.action = na.omit, plastics_sub)
mod2 = lme(leu_aminopept ~ plast_tot + temp, random = ~ 1 | mesocosm/day, na.action = na.omit,plastics_sub)
mod3 = lme(ammonium ~ leu_aminopept + plast_tot, random = ~ 1 | mesocosm/day, na.action = na.omit,plastics_sub)
mod4 = lme(fvfm ~ ammonium + temp, random = ~ 1 | mesocosm/day, na.action = na.omit, plastics_sub)



global_model <- piecewiseSEM::psem(
  #mod1,
  mod2,
  mod3,
  mod4
)
summary(global_model)
plot(global_model)