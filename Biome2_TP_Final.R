# LIBRARIES ####################################################################

library(dplyr)
library(psych)
library(ggplot2)
library(glmmTMB)
library(lme4)
library(lmerTest)
library(car)
library(patchwork)
library(betareg)
library(DHARMa)
library(emmeans)


# IMPORTO DATOS ################################################################

datos <- read.csv("EPM Tam +Mili - Hoja 1.csv", header = T, sep=",", dec=",", stringsAsFactors = T)

## Exploración de la base de datos =============================================

datos
str(datos)
summary(datos)
head(datos)

## Limpieza de la base de datos ================================================

Datos <- datos %>%
  mutate(
    # Pasamos las entradas a brazos abiertos a integer
    Brazos_abiertos_entradas = as.integer(Brazos_abiertos_entradas),
    # Pasamos el número de camada a factor
    camada = factor(camada)
  )%>%
  # Filtramos al dato mal copiado
  filter(
    ID != "R35"
  )%>%
  # Nos quedamos con las columnas que nos sirven
  select(
    ID,                          # Identificador del raton
    camada,                      # VE efectos aleatorios
    sexo,                        # VE efectos fijos
    FLX_SUC,                     # VE efectos fijos
    SAL_CNO,                     # VE efectos fijos
    tratamiento,                 # Combinacion de FLX_SUC y SAL_CNO
    Brazos_abiertos_entradas     # VR discreta: conteo en continuo
  )

# Verficamos que se hayan realizado bien los cambios
Datos
str(Datos)
summary(Datos)
head(Datos)


# ESTADÍSTICA DESCRIPTIVA ######################################################

## Tablas descriptivas =========================================================

### Tabla resumen de los datos por tratamiento ---------------------------------

resumen <- Datos %>% 
  group_by(tratamiento) %>%
  summarise(media = mean(Brazos_abiertos_entradas),
            sd = sd(Brazos_abiertos_entradas),
            min =min(Brazos_abiertos_entradas),
            max =max(Brazos_abiertos_entradas))
resumen

### Tabla resumen de los datos por tratamiento y sexo --------------------------

resumenxsexo <- Datos %>%
  group_by(FLX_SUC, SAL_CNO, sexo) %>%
  summarise(
    n     = sum(!is.na(Brazos_abiertos_entradas)), # Conteo excluyendo NAs
    media = mean(Brazos_abiertos_entradas, na.rm = TRUE),
    DE    = sd(Brazos_abiertos_entradas, na.rm = TRUE),
    var   = var(Brazos_abiertos_entradas, na.rm = TRUE),
    min   = min(Brazos_abiertos_entradas, na.rm = TRUE),
    max   = max(Brazos_abiertos_entradas, na.rm = TRUE),
    .groups = "drop" # Rompe el agrupamiento al terminar
  )

resumenxsexo

## Gráficos exploratorios ======================================================

### Boxplot con un color por camada --------------------------------------------

p1 <- ggplot(Datos, aes(x = tratamiento, y = Brazos_abiertos_entradas)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = factor(camada)), width = 0.15, size = 2, alpha = 0.8) +
  labs(x = "Grupo de tratamiento",
       y = "Nº de entradas a brazos abiertos (en 10 min)",
       color = "Camada") +
  theme_classic()

p1

### Boxplot mas sencillo, solo con outliers ------------------------------------

p2 <- ggplot(Datos, aes(x = tratamiento, y = Brazos_abiertos_entradas)) +
  geom_boxplot(fill = "white", color = "black") +
  labs(x = "Grupo de tratamiento",
       y = "Nº de entradas a brazos abiertos (en 15 min)") +
  theme_classic()

p2

### Boxplot diferenciado por sexo ----------------------------------------------

p3 <- ggplot(Datos, aes(x = tratamiento, y = Brazos_abiertos_entradas, fill = sexo)) +
  geom_boxplot(color = "black") +
  facet_wrap(~sexo,
             labeller = as_labeller(c(hembra = "Hembras", macho = "Machos"))) +
  scale_fill_manual(values = c("hembra" = "#E69F00", "macho" = "#56B4E9")) +
  labs(x = "Grupo de tratamiento",
       y = "Nº de entradas a brazos abiertos (en 10 min)") +
  theme_classic() +
  theme(legend.position = "none")

p3

### Gráfico de perfiles solo promedio ------------------------------------------

p4 <- ggplot(Datos, aes(x = FLX_SUC,
                        y = Brazos_abiertos_entradas,
                        colour = SAL_CNO,
                        group = SAL_CNO)) +
        stat_summary(fun = mean, geom = "point", size = 3) +
        stat_summary(fun = mean, geom = "line", linewidth = 1) +
        facet_wrap(~sexo,
                   labeller = as_labeller(c(macho = "Machos", 
                                            hembra = "Hembras"))) +
        scale_colour_manual(
          values = c("SAL" = "#85C0F9",
                     "CNO" = "#F5793A"),
          labels = c("SAL" = "Salina",
                     "CNO" = "CNO")
        ) +
        scale_x_discrete(
          labels = c("SUC" = "SUC",
                     "FLX" = "FLX")
        ) +
        xlab("Tratamiento") +
        ylab("N° promedio de entradas a brazos abiertos") +
        ggtitle("Entradas a brazos abiertos vs. tratamiento") +
        labs(colour = "Administración de CNO")

p4

### Gráfico de perfiles promedio + observaciones -------------------------------

p5 <- ggplot(Datos, aes(x = FLX_SUC,
                        y = Brazos_abiertos_entradas,
                        colour = SAL_CNO,
                        fill = SAL_CNO,
                        group = SAL_CNO)) +
        geom_jitter(shape = 21,
                    colour = "white",
                    stroke = 0.3,
                    width = 0.08,
                    height = 0,
                    size = 2,
                    alpha = 0.8) +
        stat_summary(fun = mean, geom = "line", linewidth = 1) +
        stat_summary(fun = mean, geom = "point",
                     shape = 21, size = 2, stroke = 1.5) +
        facet_wrap(~sexo,
                   labeller = as_labeller(c(macho = "Machos",
                                            hembra = "Hembras"))) +
        scale_colour_manual(
          values = c("SAL" = "#2B7BBF",
                     "CNO" = "#C4501B"),
          labels = c("SAL" = "Salina",
                     "CNO" = "CNO")
        ) +
        scale_fill_manual(
          values = c("SAL" = "#85C0F9",
                     "CNO" = "#F5793A"),
          labels = c("SAL" = "Salina",
                     "CNO" = "CNO")
        ) +
        xlab("Tratamiento") +
        ylab("N° promedio de entradas a brazos abiertos") +
        ggtitle("Entradas a brazos abiertos vs. tratamiento") +
        labs(colour = "Administración de CNO",
             fill = "Administración de CNO")

p5


# AJUSTE DEL MODELO ############################################################

## Modelo 1: Poisson ===========================================================

### Ajuste del modelo ----------------------------------------------------------

modelo1 <- glmmTMB(
  Brazos_abiertos_entradas ~ FLX_SUC * SAL_CNO * sexo + (1 | camada),
  data = Datos,
  family = poisson(link = "log")
)

summary(modelo1)
Anova(modelo1, type = "III") # Usamos Anova tipo III porque tenemos grupos desbalanceados

### Supuestos efectos fijos ----------------------------------------------------

residuos_m1 <- simulateResiduals(modelo1)          # Simulamos residuos con DHARMa

plot(residuos_m1)
testUniformity(residuos_m1)                        # Test uniformidad
testDispersion(residuos_m1)                        # Test dispersión
testZeroInflation(residuos_m1)                     # Test inflación en ceros
plotResiduals(residuos_m1, form = fitted(modelo1)) # Residuos frente a valores ajustados
testOutliers(residuos_m1)                          # Test Outliers

# Calculamos estadístico de dispersión manualmente
sum(residuals(modelo1, type="pearson")^2)/df.residual(modelo1)

### Supuestos efectos aleatorios -----------------------------------------------

ef_camadas_m1 <- ranef(modelo1)$cond$camada[["(Intercept)"]]

qqPlot(
  ef_camadas_m1,
  main = "QQ Plot de efectos aleatorios de las camadas",
  ylab = "Efectos aleatorios estimados"
)

shapiro.test(ef_camadas_m1)       # Shapiro-Wilks test

## Modelo 2: Binomial Negativa =================================================

### Ajuste del modelo ----------------------------------------------------------

modelo2 <- glmmTMB(
  Brazos_abiertos_entradas ~ FLX_SUC * SAL_CNO * sexo + (1 | camada),
  family = "nbinom2",
  data = Datos
)

summary(modelo2)
Anova(modelo2, type = "III") # Usamos Anova tipo III porque tenemos grupos desbalanceados

### Supuestos efectos fijos ----------------------------------------------------

residuos_m2 <- simulateResiduals(modelo2)          # Simulamos residuos con DHARMa

plot(residuos_m2)
testUniformity(residuos_m2)                        # Test uniformidad
testDispersion(residuos_m2)                        # Test dispersión
testZeroInflation(residuos_m2)                     # Test inflación en ceros
plotResiduals(residuos_m2, form = fitted(modelo2)) # Residuos frente a valores ajustados
testOutliers(residuos_m2)                          # Test Outliers

# Calculamos estadístico de dispersión manualmente
sum(residuals(modelo2, type="pearson")^2)/df.residual(modelo2)

### Supuestos efectos aleatorios -----------------------------------------------

ef_camadas_m2 <- ranef(modelo2)$cond$camada[["(Intercept)"]]

qqPlot(
  ef_camadas_m2,
  main = "QQ Plot de efectos aleatorios de las camadas",
  ylab = "Efectos aleatorios estimados"
)

shapiro.test(ef_camadas_m2)       # Shapiro-Wilks test

## Modelo 3: Conway-Maxwell-Poisson ============================================

### Ajuste del modelo ----------------------------------------------------------

modelo3 <- glmmTMB(
  Brazos_abiertos_entradas ~ FLX_SUC * SAL_CNO * sexo + (1 | camada),
  family = "compois",
  data = Datos
)

summary(modelo3)
Anova(modelo3, type = "III") # Usamos Anova tipo III porque tenemos grupos desbalanceados

### Supuestos efectos fijos ----------------------------------------------------

residuos_m3 <- simulateResiduals(modelo3)          # Simulamos residuos con DHARMa

plot(residuos_m3)
testUniformity(residuos_m3)                        # Test uniformidad
testDispersion(residuos_m3)                        # Test dispersión
testZeroInflation(residuos_m3)                     # Test inflación en ceros
plotResiduals(residuos_m3, form = fitted(modelo3)) # Residuos frente a valores ajustados
testOutliers(residuos_m3)                          # Test Outliers

# Calculamos estadístico de dispersión manualmente
sum(residuals(modelo3, type="pearson")^2)/df.residual(modelo3)

### Supuestos efectos aleatorios -----------------------------------------------

ef_camadas_m3 <- ranef(modelo3)$cond$camada[["(Intercept)"]]

qqPlot(
  ef_camadas_m3,
  main = "QQ Plot de efectos aleatorios de las camadas",
  ylab = "Efectos aleatorios estimados"
)

shapiro.test(ef_camadas_m3)       # Shapiro-Wilks test

## Modelo 4: Tweedie ===========================================================

### Ajuste del modelo ----------------------------------------------------------

modelo4 <- glmmTMB(
  Brazos_abiertos_entradas ~ FLX_SUC * SAL_CNO * sexo + (1 | camada),
  family = "tweedie",
  data = Datos
)

summary(modelo4)
Anova(modelo4, type = "III") # Usamos Anova tipo III porque tenemos grupos desbalanceados

### Supuestos efectos fijos ----------------------------------------------------

residuos_m4 <- simulateResiduals(modelo4)          # Simulamos residuos con DHARMa

plot(residuos_m4)
testUniformity(residuos_m4)                        # Test uniformidad
testDispersion(residuos_m4)                        # Test dispersión
testZeroInflation(residuos_m4)                     # Test inflación en ceros
plotResiduals(residuos_m4, form = fitted(modelo4)) # Residuos frente a valores ajustados
testOutliers(residuos_m4)                          # Test Outliers

# Calculamos estadístico de dispersión manualmente
sum(residuals(modelo4, type="pearson")^2)/df.residual(modelo4)

### Supuestos efectos aleatorios -----------------------------------------------

ef_camadas_m4 <- ranef(modelo4)$cond$camada[["(Intercept)"]]

qqPlot(
  ef_camadas_m4,
  main = "QQ Plot de efectos aleatorios de las camadas",
  ylab = "Efectos aleatorios estimados"
)

shapiro.test(ef_camadas_m4)       # Shapiro-Wilks test

## Modelo 5: OLRE ==============================================================

### Ajuste del modelo ----------------------------------------------------------

modelo5 <- glmmTMB(
  Brazos_abiertos_entradas ~ FLX_SUC * SAL_CNO * sexo + (1 | camada) + (1 | ID),
  family = "poisson",
  data = Datos
)

summary(modelo5)
Anova(modelo5, type = "III") # Usamos Anova tipo III porque tenemos grupos desbalanceados

### Supuestos efectos fijos ----------------------------------------------------

residuos_m5 <- simulateResiduals(modelo5, simulateREs = "unconditional") # Simulamos residuos con DHARMa

plot(residuos_m5)
testUniformity(residuos_m5)                        # Test uniformidad
testDispersion(residuos_m5)                        # Test dispersión
testZeroInflation(residuos_m5)                     # Test inflación en ceros
plotResiduals(residuos_m5, form = fitted(modelo5)) # Residuos frente a valores ajustados OJO QUE ESTE DA EN ROJO!!! PREGUNTAR
testOutliers(residuos_m5)                          # Test Outliers

# Calculamos estadístico de dispersión manualmente
sum(residuals(modelo5, type="pearson")^2)/df.residual(modelo5) 

# OJO!!: EL DHARMA DA BIEN PERO EL MANUAL DA SUBDISPERSIÓN, PREGUNTAR!!!

### Supuestos efectos aleatorios -----------------------------------------------

ef_camadas_m5 <- ranef(modelo5)$cond$camada[["(Intercept)"]]

qqPlot(
  ef_camadas_m5,
  main = "QQ Plot de efectos aleatorios de las camadas",
  ylab = "Efectos aleatorios estimados"
)

shapiro.test(ef_camadas_m5)       # Shapiro-Wilks test

## Comparación de modelos por AIC ==============================================

# Los modelos candidatos son modelo2, modelo3 y modelo4, MODELO 5 HAY PREGUNTAS QUE HACER PERO LO PONGO POR LAS DUDAS

AIC(modelo2, modelo3, modelo4, modelo5)

# Peleado entre el modelo2 y el modelo5. Preferimos el modelo2
