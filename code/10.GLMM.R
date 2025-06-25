



# Structural equation models
rm(list = ls(all.names = TRUE))

pacman::p_load(dplyr,reshape2,tidyverse, lubridate, ggplot2, ggpubr, gridExtra,
               car, ggsignif, dunn.test, rstatix)

source("code/palettes_labels.R")

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
  filter(sampling != "0") %>%                               # quito pre-muestreo porque no nos es relevante
  filter(!(sampling == "1" & treatment %in% c("p", "wp")))  # quito estos muestreos para p y wp porque añaden ruido con los 0



arkaute_norm <- read.csv("data/arkaute_norm_all.csv") %>%   # aqui cargo los datos de arkaute normalizados a nivel de variable, sin diferenciar por tratamientos
  mutate(
    year = as.factor(year),
    date = ymd(date),
    omw_date = as.factor(omw_date),
    one_month_window = as.factor(one_month_window),
    sampling = as.factor(sampling),
    plot = as.factor(plot),
    treatment = as.factor(treatment))


## Antes de nada, vamos a ver si mean_vwc actua como covariable ##

ggplot(arkaute, aes(x = treatment, y = mean_vwc, fill = treatment)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.3) +
  theme_minimal() +
  labs(title = "Distribución de humedad por tratamiento",
       y = "Humedad (mean_vwc)", x = "Tratamiento")

ggplot(arkaute, aes(x = plot, y = mean_vwc)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Distribución de humedad por parcela",
       y = "Humedad (mean_vwc)", x = "Parcela")

ggplot(arkaute, aes(x = perturbation, y = mean_vwc, fill = perturbation)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.3) +
  theme_minimal()

ggplot(arkaute, aes(x = OTC, y = mean_vwc, fill = OTC)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.3) +
  theme_minimal()

##| ¿Cuánta colinealidad hay entre `mean_vwc` y `treatment`?

summary(lm(mean_vwc ~ treatment, data = arkaute))  # R² bajo = aporta info extra

# el modelo indica que el modelo mean_vwc ~ treatment explica un 8.1% de la varianza total observada
# en el a humedad del suelo. 91.9% de la variabilidad en mean_vwc no se explica por el tratameiento. 
# Esto significa que treatment y mean_vwc no están altamente colineados, por lo que incluir ambos en el modelo 
# tiene sentido. 


# Cargar las librerías necesarias
library(lme4)
library(glmmTMB)
library(performance)
library(emmeans)
library(DHARMa)




#### GLMM RICHNESS ######################################################################

# Si hay sobredispersión significativa, usar binomial negativa
model_nb <- glmmTMB(richness ~ treatment + scale(mean_vwc) + (1 | plot),
                    data = arkaute,
                    family = nbinom2(link = "log"))

check_overdispersion(model_nb)
plot(simulateResiduals(model_nb)) # si los residuales se ajustan al modelo y los boxplots salen grises esque es un buen modelo

# Resumen del modelo binomial negativo
summary(model_nb)

# Comparaciones post-hoc entre tratamientos
emmeans(model_nb, pairwise ~ treatment)
 # Se confirman LRR results para c-w y wp-p. Añade diferencia entre p-c que no se ve en el LRR


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


 #### GLMM ABUNDANCE ######################################################################

model_abundance <- glmmTMB(
  abundance ~ treatment + scale(mean_vwc) + (1 | plot),
  data = arkaute,
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



model_abundance_tw <- glmmTMB(
  abundance ~ treatment  + scale(mean_vwc) + (1 | plot),
  data = arkaute,
  family = tweedie(link = "log")
)

check_overdispersion(model_abundance_tw)
plot(simulateResiduals(model_abundance_tw))

arkaute <- arkaute %>%
  mutate(log_abundance = log(abundance + 1))  # +1 evita log(0)

# Modelo GLMM con distribución normal
model_abundance_log <- glmmTMB(
  log_abundance ~ treatment + scale(mean_vwc) + (1 | plot),
  data = arkaute,
  family = gaussian()
)

check_overdispersion(model_abundance_log)
sim_log_abund <- simulateResiduals(model_abundance_log)
plot(sim_log_abund)
 # no consigo que se ajuste ningún modelo..

#### GLMM para eveness ######################################################################

##| Modelo GLMM para Y_zipf (evenness)
##| -----------------------------------
##| Se modela el coeficiente gamma del modelo Zipf como proxy de evenness.
##| Más cercano a 0 = mayor evenness; más negativo = más dominancia.
##| Se usa distribución normal porque Y_zipf es continuo y ya transformado.
##| Incluimos:
##| - `treatment`: efecto fijo de interés.
##| - `mean_vwc`: covariable centrada/escalada.
##| - `plot`: efecto aleatorio (estructura jerárquica).

model_evenness <- glmmTMB(
  Y_zipf ~ treatment +  scale(mean_vwc) + (1 | plot),
  data = arkaute,
  family = gaussian()
)

summary(model_evenness)

plot(simulateResiduals(model_evenness))

emmeans(model_evenness, pairwise ~ treatment, type = "response")

## Se confirma LRR para p-c, pero no para wp-c ni p-wp !! Problema?? :((((((((


# El modelo no es perfecto pero es válido. 



### GLMM PARA BIOMASA ##################################################################

model_biomass <- glmmTMB(
  biomass ~ treatment + scale(mean_vwc) + (1 | plot),
  data = arkaute,
  family = Gamma(link = "log")
)

summary(model_biomass)

##| Diagnóstico de residuos
plot(simulateResiduals(model_biomass))


emmeans(model_biomass, pairwise ~ treatment, type = "response")



### GLMM PARA BIOMASA012 ######################################################################

model_biomass012 <- glmmTMB(
  biomass012 ~ treatment + scale(mean_vwc) + (1 | plot),
  data = arkaute,
  family = Gamma(link = "log")
)

summary(model_biomass012)

##| Diagnóstico de residuos
plot(simulateResiduals(model_biomass012))

# MOdelo aceptable

emmeans(model_biomass012, pairwise ~ treatment, type = "response")
# Confirmo resultados LRR entre c-p y wp-p

#### GLMM PARA NMDS1  y NMDS2 ############################################################################

##| Modelo GLMM para NMDS1
##| -----------------------
##| NMDS1 es un eje de ordenación, continuo, positivo y con ligera asimetría a la derecha.
##| Se modela con distribución Gamma y link logarítmico.
##| Efectos fijos: treatment + mean_vwc (escalado).
##| Efecto aleatorio: plot (estructura experimental).

model_NMDS1 <- glmmTMB(
  NMDS1 ~ treatment + scale(mean_vwc) + (1 | plot),
  data = arkaute,
  family = Gamma(link = "log")
)

summary(model_NMDS1)

##| Diagnóstico de residuos
plot(simulateResiduals(model_NMDS1))
emmeans(model_NMDS1, pairwise ~ treatment, type = "response")

# Confirma LRR difrencias entre p-c pero no ve diferencias entre wp-c. 


model_NMDS2 <- glmmTMB(
  NMDS2 ~ treatment + scale(mean_vwc) + (1 | plot),
  data = arkaute,
  family = Gamma(link = "log")
)

summary(model_NMDS2)

##| Diagnóstico de residuos
plot(simulateResiduals(model_NMDS2))
# Modelo algo limitado. 

emmeans(model_NMDS2, pairwise ~ treatment, type = "response")

## COnfirma diferencias entre c-p y c-wp 


#### GLMM PARA PC1 Y PC2 ############################################################################

model_PC1 <- glmmTMB(
  PC1 ~ treatment + scale(mean_vwc) + (1 | plot),
  data = arkaute,
  family = tweedie(link = "log")
)

summary(model_PC1)
##| Diagnóstico de residuos
plot(simulateResiduals(model_PC1))
# Modelo muy limitado. 

emmeans(model_PC1, pairwise ~ treatment, type = "response")
# No confirma nada porque no encuentro un modelo que se ajuste bien


model_PC2 <- glmmTMB(
  PC2 ~ treatment + scale(mean_vwc) + (1 | plot),
  data = arkaute,
  family = tweedie(link = "log")
)


summary(model_PC2)
##| Diagnóstico de residuos
plot(simulateResiduals(model_PC2))
# Modelo  limitado.

emmeans(model_PC2, pairwise ~ treatment, type = "response")
# Confirma LRR differences c-p y c-wp














## GLMM en base a tiempo?

model_richness_time <- glmmTMB(richness ~ treatment * sampling + scale(mean_vwc) + (1 | plot),
                    data = arkaute,
                    family = nbinom2(link = "log"))




summary(model_richness_time)
##| Diagnóstico de residuos
plot(simulateResiduals(model_richness_time))
# Modelo  limitado.

emmeans(model_richness_time, pairwise ~ treatment, type = "response")
# Confirma LRR differences c-p y c-wp
