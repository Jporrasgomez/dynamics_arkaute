



# Structural equation models
rm(list = ls(all.names = TRUE))

pacman::p_load(dplyr,reshape2,tidyverse, lubridate, ggplot2, ggpubr, gridExtra,
               car, ggsignif, dunn.test, rstatix)


theme_set(theme_bw() +
            theme(
              legend.position = "right",
              panel.grid = element_blank(),
              strip.background = element_blank(),
              strip.text = element_text(face = "bold"),
              text = element_text(size = 11)))

arkaute <- read.csv("data/arkaute.csv") %>% 
  mutate(
    year = as.factor(year),
    date = ymd(date),
    omw_date = as.factor(omw_date),
    one_month_window = as.factor(one_month_window),
    sampling = as.factor(sampling),
    plot = as.factor(plot),
    treatment = as.factor(treatment)) %>% 
  filter(sampling != "0")



arkaute_norm <- read.csv("data/arkaute_norm.csv") %>% 
  mutate(
    year = as.factor(year),
    date = ymd(date),
    omw_date = as.factor(omw_date),
    one_month_window = as.factor(one_month_window),
    sampling = as.factor(sampling),
    plot = as.factor(plot),
    treatment = as.factor(treatment))


plot(arkaute$richness ~ arkaute$mean_temperature)
plot(arkaute$abundance ~ arkaute$mean_temperature)
plot(arkaute$biomass012 ~ arkaute$mean_temperature)
plot(arkaute$Y_zipf ~ arkaute$mean_temperature)
plot(arkaute$NMDS2 ~ arkaute$mean_temperature)
plot(arkaute$PC2 ~ arkaute$mean_temperature)


plot(arkaute$richness ~ arkaute$mean_vwc)
plot(arkaute$abundance ~ arkaute$mean_vwc)
plot(arkaute$biomass012 ~ arkaute$mean_vwc)
plot(arkaute$Y_zipf ~ arkaute$mean_vwc)
plot(arkaute$NMDS2 ~ arkaute$mean_vwc)
plot(arkaute$PC2 ~ arkaute$mean_vwc)



# Cargar las librerías necesarias
library(lme4)
library(glmmTMB)
library(performance)
library(emmeans)
library(DHARMa)


# Modelo GLMM con distribución Poisson
model_poisson <- glmmTMB(richness ~ treatment + (1 | plot),
                         data = arkaute,
                         family = poisson)

# Chequeo de sobredispersión
check_overdispersion(model_poisson)

# Si hay sobredispersión significativa, usar binomial negativa
model_nb <- glmmTMB(richness ~ treatment + (1 | plot),
                    data = arkaute,
                    family = nbinom2)

check_overdispersion(model_nb)
plot(simulateResiduals(model_nb))

# Resumen del modelo binomial negativo
summary(model_nb)

# Comparaciones post-hoc entre tratamientos
emmeans(model_nb, pairwise ~ treatment)



library(nlme)

# Modelo lineal mixto
model_lmm <- lme(richness ~ treatment,
                 random = ~1 | plot,
                 data = arkaute_norm)

# Resumen del modelo
summary(model_lmm)

emmeans(model_lmm, pairwise ~ treatment)

plot(model_lmm)


# PARa la misma variable, el GLM detecta diferencias significativas entre riqueza y tratamiento y el LME no. 
##| ========================================================
##| MODELAJE DE ABUNDANCE SIN CEROS ESTRUCTURALES
##| ========================================================
##|
##| En algunos muestreos se introdujeron ceros manualmente
##| porque se retiró toda la cobertura vegetal (perturbación).
##|
##| Estos ceros:
##|   - No provienen de observaciones "naturales".
##|   - Representan un estado forzado experimental (ausencia total).
##|
##| Por tanto:
##| ✅ Se pueden incluir en gráficas descriptivas o log-response ratios.
##| ❌ Pero NO deben incluirse en modelos de tipo GLMM (como Gamma o Tweedie),
##|    ya que estas distribuciones suponen valores > 0 continuos.
##|
##| Solución: Filtrar los ceros antes del ajuste del modelo.
##|
##| ========================================================
##| Filtrar los datos para eliminar ceros estructurales:
##| 
arkaute_no0 <- arkaute %>% 
  filter(!(sampling == 1 & treatment %in% c("p", "wp")))


model_abundance <- glmmTMB(
  abundance ~ treatment + (1 | plot),
  data = arkaute_no0,
  family = Gamma(link = "log")
)

check_overdispersion(model_abundance)

##| Chequeo de sobredispersión del modelo
##| --------------------------------------
##| La función `check_overdispersion()` indica si hay sobredispersión o subdispersión
##| comparando la varianza observada con la esperada bajo el modelo (en este caso, un GLMM con glmmTMB).
##|
##| Resultado:
##|   - ratio de dispersión = 0.488
##|   - p-valor < 0.001
##|
##| Esto sugiere **subdispersión significativa**, es decir, la varianza observada es 
##| menor de lo que se esperaría bajo una distribución Poisson (o binomial negativa, si se usara).
##|
##| 🔍 Interpretación:
##|   - La subdispersión no es tan común como la sobredispersión, y puede deberse a:
##|     - Estructura del diseño muy controlada.
##|     - Restricciones en la variabilidad (p. ej., efectos de techo o suelo).
##|     - Errores en la estructura del modelo (por ejemplo, variables predictoras omitidas).
##|
##| ⚠️ Implicaciones:
##|   - La subdispersión puede llevar a **sobreestimar la significación estadística** 
##|     (es decir, p-valores más bajos de lo que deberían ser).
##|   - No hay un ajuste directo como en el caso de la sobredispersión (donde pasarías a binomial negativa).
##|   - Se recomienda ser conservador con las inferencias, y complementar con diagnósticos visuales
##|     y sensibilidad del modelo.

plot(simulateResiduals(model_abundance))

##| Diagnóstico de residuos con DHARMa para model_abundance
##| --------------------------------------------------------
##| Se usó `simulateResiduals()` seguido de `plot()` para evaluar la adecuación del modelo.
##|
##| 1. QQ plot (izquierda):
##|    - La desviación clara respecto a la línea diagonal indica que los residuos simulados
##|      no siguen la distribución esperada bajo el modelo.
##|    - KS test: p = 0 → desviación significativa de la uniformidad.
##|    - Dispersion test: p = 0 → fuerte evidencia de **subdispersión**.
##|    - Outlier test: p = 0.00024 → posible presencia de outliers importantes.
##|
##| 2. Boxplot de residuos por predictores categóricos (derecha):
##|    - Rojo indica diferencias significativas en la varianza residual entre grupos.
##|    - Levene Test significativo → violación de la homogeneidad de varianzas.
##|
##| ✳️ Conclusión:
##|    - El modelo presenta **problemas claros de ajuste**: subdispersión, varianza no homogénea
##|      y residuos no uniformes.
##|    - Es recomendable revisar:
##|       • Si hay estructura no modelada (p. ej., variables omitidas).
##|       • La distribución usada (quizás un modelo `beta` para proporciones o transformación logit).
##|       • Posibles outliers o errores en los datos (especialmente 0s o >100).
##|
##| ⚠️ Sugerencias:
##|    - Considerar modelar con `beta regression` (`beta_family()` en `glmmTMB`) si los datos están entre 0 y 1.
##|    - Si hay valores 0 o >1, transformar previamente (p. ej., `(abundance + ε)/(max + 2ε)`) o usar un modelo alternativo como `Tweedie`.


epsilon <- 0.001
max_abund <- max(arkaute$abundance, na.rm = TRUE)

arkaute <- arkaute %>%
  mutate(abundance_beta = (abundance + epsilon) / (max_abund + 2 * epsilon))


model_beta <- glmmTMB(
  abundance_beta ~ treatment + (1 | plot),
  data = arkaute,
  family = beta_family(link = "logit")
)

check_overdispersion(model_beta)
plot(simulateResiduals(model_beta))
