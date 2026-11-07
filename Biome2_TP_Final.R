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

#Holaaaa, Naza 

# IMPORTO DATOS ################################################################

datos <- read.csv("EPM Tam +Mili - Hoja 1.csv", header = T, sep=",", dec=",", stringsAsFactors = T)

## Exploración de la base de datos =============================================

datos
str(datos)
summary(datos)
head(datos)

## Limpieza de la base de datos ================================================



datos$Brazos_abiertos_entradas <- as.integer(datos$Brazos_abiertos_entradas)
datos$camada <- as.factor(datos$camada)

datos.v2 <- datos %>% select(-c(marca, ID, Brazos_cerrados_entradas, Brazos_cerrados_tiempo, Brazos_abiertos_tiempo))
summary(datos.v2)
str(datos.v2)

# ESTADÍSTICA DESCRIPTIVA ######################################################

# Tablas descriptivas ==========================================================

describeBy(
  Brazos_abiertos_entradas ~ FLX_SUC + SAL_CNO, 
  data = datos.v2, 
  mat = TRUE, 
  digits = 2
)

datos.v2 %>%
  group_by(FLX_SUC, SAL_CNO) %>%
  summarise_all(.funs = c(
    n = length, 
    media = mean(Brazos_abiertos_entradas, na.rm = TRUE),
    DE = sd(Brazos_abiertos_entradas, na.rm = TRUE),
    var = var(Brazos_abiertos_entradas, na.rm = TRUE),
    min = min, 
    max = max
  ))

datos.v2 %>%
  group_by(FLX_SUC, SAL_CNO) %>%
  summarise(
    n     = sum(!is.na(Brazos_abiertos_entradas)), # Conteo excluyendo NAs de forma segura
    media = mean(Brazos_abiertos_entradas, na.rm = TRUE),
    DE    = sd(Brazos_abiertos_entradas, na.rm = TRUE),
    var   = var(Brazos_abiertos_entradas, na.rm = TRUE),
    min   = min(Brazos_abiertos_entradas, na.rm = TRUE),
    max   = max(Brazos_abiertos_entradas, na.rm = TRUE),
    .groups = "drop" # Rompe el agrupamiento al terminar
  )

p2 <- ggplot(
  datos.v2,
  aes(
    x = tratamiento,
    y = Brazos_abiertos_entradas,
    color = camada
  )
) +
  geom_boxplot(
    aes(fill = sexo),
    color = "black",
    outlier.shape = NA,
    alpha = 0.4,
    position = position_dodge(0.8)
  ) +
  geom_point(
    aes(color = camada, group = sexo),
    position = position_jitterdodge(
      jitter.width = 0.15, 
      dodge.width = 0.8
    ),
    size = 2,
    alpha = 0.8
  ) +
  scale_fill_manual(values = c("Hembra" = "honeydew2", "Macho" = "white")) +
  labs(
    x = "Tratamiento",
    y = "Entradas a los brazos abiertos en 15 min",
    fill = "Sexo",
    color = "Camada"
  ) +
  scale_color_brewer(
    palette = "Paired"
  ) +
  theme_classic()
p2

modelo1 <- glmmTMB(
  Brazos_abiertos_entradas ~ FLX_SUC * SAL_CNO * sexo + (1 | camada),
  data = datos,
  family = poisson(link = "log")
)

summary(modelo1)
Anova(modelo1, type = "III")

sum(residuals(modelo1, type="pearson")^2)/df.residual(modelo1)

sim_entradas <- simulateResiduals(fittedModel = modelo1, plot = T)

plot(sim_entradas)
testDispersion(sim_entradas)
testZeroInflation(sim_entradas)

p2 <- ggplot(
  datos,
  aes(
    x = tratamiento,
    y = Brazos_abiertos_entradas,
    color = camada
  )
) +
  geom_boxplot(
    aes(fill = sexo),
    color = "black",
    outlier.shape = NA,
    alpha = 0.4,
    position = position_dodge(0.8)
  ) +
  geom_point(
    aes(color = camada, group = sexo),
    position = position_jitterdodge(
      jitter.width = 0.15, 
      dodge.width = 0.8
    ),
    size = 2,
    alpha = 0.8
  ) +
  scale_fill_manual(values = c("hembra" = "honeydew2", "macho" = "white")) +
  labs(
    x = "Tratamiento",
    y = "Entradas a brazos abiertos en 15 min",
    fill = "Sexo",
    color = "Camada"
  ) +
  scale_color_brewer(
    palette = "Paired"
  ) +
  theme_classic()
p2




datos <- read.csv("EPM Tam +Mili - Hoja 1.csv", header = T, sep=",", dec=",", stringsAsFactors = T)
datos
str(datos)
summary(datos)
head(datos)

# Filtramos el dato mal copiado
datos <- datos %>% 
  filter(ID != "R35")

#Como el summary quedaba muy largo, saque las variables que no nos interesa, en el siguiente código solo agrego las que vamos y podríamos usar:

Datos <- datos[, c("sexo", "camada", "tratamiento","ID","Brazos_abiertos_entradas")]

library(dplyr)

Datos <- datos %>% 
  select(sexo, camada, tratamiento, ID, Brazos_abiertos_entradas)

#Paso el numero de camada a factor y las entradas a brazos abiertos a integer porque tiene que ser discreto

Datos$camada <- factor(Datos$camada)

Datos$Brazos_abiertos_entradas <- as.integer(Datos$Brazos_abiertos_entradas)

head(Datos)
summary(Datos)

#Hago un resumen exploratorio de cómo resultan los datos en cada tratamiento

library(dplyr)

resumen <- Datos %>% 
  group_by(tratamiento) %>%
  summarise(media = mean(Brazos_abiertos_entradas),
            sd = sd(Brazos_abiertos_entradas),
            min =min(Brazos_abiertos_entradas),
            max =max(Brazos_abiertos_entradas))
resumen


resumenxsexo <- datos %>%
  group_by(FLX_SUC, SAL_CNO, sexo) %>%
  summarise(
    n     = sum(!is.na(Brazos_abiertos_entradas)), # Conteo excluyendo NAs de forma segura
    media = mean(Brazos_abiertos_entradas, na.rm = TRUE),
    DE    = sd(Brazos_abiertos_entradas, na.rm = TRUE),
    var   = var(Brazos_abiertos_entradas, na.rm = TRUE),
    min   = min(Brazos_abiertos_entradas, na.rm = TRUE),
    max   = max(Brazos_abiertos_entradas, na.rm = TRUE),
    .groups = "drop" # Rompe el agrupamiento al terminar
  )

resumenxsexo


#grafico exploratorios


library(ggplot2)

ggplot(Datos, aes(x = tratamiento, y = Brazos_abiertos_entradas)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = factor(camada)), width = 0.15, size = 2, alpha = 0.8) +
  labs(x = "Grupo de tratamiento",
       y = "Nº de entradas a brazos abiertos (en 15 min)",
       color = "Camada") +
  theme_classic()

#Boxplot mas sencillo, solo con outliers, porque puede ser abrumador pirmero mostrar el boxplot con los colores

ggplot(Datos, aes(x = tratamiento, y = Brazos_abiertos_entradas)) +
  geom_boxplot(fill = "white", color = "black") +
  labs(x = "Grupo de tratamiento",
       y = "Nº de entradas a brazos abiertos (en 15 min)") +
  theme_classic()

# 1. Creamos una columna nueva que agrupe todo lo que no sea la camada 5 como "Otras"
Datos <- Datos %>%
  mutate(camada_destacada = if_else(camada == 5 | camada == "5", "Camada 5", "Otras"))

# 2. Hacemos el gráfico mapeando el color a esta nueva variable
ggplot(Datos, aes(x = tratamiento, y = Brazos_abiertos_entradas)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(
    aes(color = camada_destacada), 
    width = 0.15, 
    size = 2, 
    alpha = 0.8
  ) +
  # Definimos los colores manualmente: un color llamativo para la 5 y gris para el resto
  scale_color_manual(
    values = c("Camada 5" = "#E41A1C", "Otras" = "grey70")
  ) +
  labs(
    x = "Grupo de tratamiento",
    y = "Nº de entradas a brazos abiertos (en 15 min)",
    color = "Referencia"
  ) +
  theme_classic()

Datos <- Datos %>%
  mutate(
    grupo_color = case_when(
      (camada == 5 | camada == "5") & sexo == "hembra" ~ "Camada 5 (Hembra)",
      (camada == 5 | camada == "5") & sexo == "macho"  ~ "Camada 5 (Macho)",
      TRUE ~ "Otras camadas"
    )
  )

# 2. Generamos el gráfico
ggplot(Datos, aes(x = tratamiento, y = Brazos_abiertos_entradas)) +
  # Boxplot de fondo
  geom_boxplot(outlier.shape = NA, alpha = 0.3) +
  
  # Puntos mapeados a nuestra nueva variable de color
  geom_jitter(
    aes(color = grupo_color), 
    width = 0.15, 
    size = 2.5, 
    alpha = 0.85
  ) +
  
  # Asignamos colores específicos para la camada 5 y gris para el resto
  scale_color_manual(
    values = c(
      "Camada 5 (Hembra)" = "#E41A1C", # Un tono llamativo (ej. rojo)
      "Camada 5 (Macho)"  = "#377EB8", # Otro tono llamativo (ej. azul)
      "Otras camadas"     = "grey70"   # Gris neutro para el resto
    )
  ) +
  
  labs(
    x = "Grupo de tratamiento",
    y = "Nº de entradas a brazos abiertos (en 15 min)",
    color = "Grupos"
  ) +
  theme_classic()

ggplot(Datos, aes(x = tratamiento, y = Brazos_abiertos_entradas)) +
  # Boxplot de fondo
  geom_boxplot(outlier.shape = NA, alpha = 0.3) +
  
  # Puntos con color según el sexo
  geom_jitter(
    aes(color = sexo), 
    width = 0.15, 
    size = 2, 
    alpha = 0.85
  ) +
  
  # Elige los colores que quieras para cada sexo (asegúrate de que 
  # "hembra" y "macho" coincidan con los textos exactos de tus datos)
  scale_color_manual(
    values = c("hembra" = "#E41A1C", "macho" = "#377EB8") 
  ) +
  
  labs(
    x = "Grupo de tratamiento",
    y = "Nº de entradas a brazos abiertos (en 15 min)",
    color = "Sexo"
  ) +
  theme_classic()





modelo1 <- glmmTMB(
  Brazos_abiertos_entradas ~ FLX_SUC * SAL_CNO * sexo + (1 | camada),
  data = datos,
  family = poisson(link = "log")
)

summary(modelo1)
Anova(modelo1, type = "III")

sum(residuals(modelo1, type="pearson")^2)/df.residual(modelo1)

sim_entradas <- simulateResiduals(fittedModel = modelo1, plot = T)

plot(sim_entradas)
testDispersion(sim_entradas)
testZeroInflation(sim_entradas)
plotResiduals(sim_entradas, form = )

efectos_camadas <- ranef(modelo1)$camada[, "(Intercept)"]

qqPlot(
  efectos_camadas,
  main = "QQ Plot de efectos aleatorios de las camadas",
  ylab = "Efectos aleatorios estimados"
)


# Supuestos efectos aleatorios

efectos_camadas <- ranef(modelo1)$cond$camada[, "(Intercept)"]
str(ranef(modelo1))
efectos_camadas <- ranef(modelo1)$cond$camada[, "(Intercept)"]

qqnorm(efectos_camadas, main = "QQ plot de efectos aleatorios de camada",
       ylab = "Efectos aleatorios estimados")
qqline(efectos_camadas)
qqPlot(
  efectos_camadas,
  main = "QQ Plot de efectos aleatorios de las camadas",
  ylab = "Efectos aleatorios estimados"
)
shapiro.test(efectos_camadas)


# AJUSTE DEL MODELO ############################################################

datos$Brazos_abiertos_entradas <- as.integer(datos$Brazos_abiertos_entradas)

# Modelo 1: Poisson ============================================================

# Ajuste del modelo ------------------------------------------------------------

modelo1 <- glmmTMB(
  Brazos_abiertos_entradas ~ FLX_SUC * SAL_CNO * sexo + (1 | camada),
  data = datos,
  family = poisson(link = "log")
)

summary(modelo1)
Anova(modelo1, type = "III")
Anova(modelo1, type = "II")

# Supuestos efectos fijos ------------------------------------------------------

residuos_m1 <- simulateResiduals(modelo1)          # Simulamos residuos con DHARMa

plot(residuos_m1)
testUniformity(residuos_m1)                        # Test uniformidad
testDispersion(residuos_m1)                        # Test dispersión
testZeroInflation(residuos_m1)                     # Test inflación en ceros
plotResiduals(residuos_m1, form = fitted(modelo1)) # Residuos frente a valores ajustados
testOutliers(residuos_m1)                          # Test Outliers


sum(residuals(modelo1, type="pearson")^2)/df.residual(modelo1)

# Supuestos efectos aleatorios -------------------------------------------------

ef_camadas_m1 <- ranef(modelo1)$cond$camada[["(Intercept)"]]

qqPlot(
  ef_camadas_m1,
  main = "QQ Plot de efectos aleatorios de las camadas",
  ylab = "Efectos aleatorios estimados"
)

shapiro.test(ef_camadas_m1)       # Shapiro-Wilks test

# Modelo 2: Binomial Negativa ==================================================

# Ajuste del modelo ------------------------------------------------------------

modelo2 <- glmmTMB(
  Brazos_abiertos_entradas ~ FLX_SUC * SAL_CNO * sexo + (1 | camada),
  family = "nbinom2",
  data = datos
)

summary(modelo2)
Anova(modelo2, type = "III")
Anova(modelo2, type = "II")

# Supuestos efectos fijos ------------------------------------------------------

residuos_m2 <- simulateResiduals(modelo2)          # Simulamos residuos con DHARMa

plot(residuos_m2)
testUniformity(residuos_m2)                        # Test uniformidad
testDispersion(residuos_m2)                        # Test dispersión
testZeroInflation(residuos_m2)                     # Test inflación en ceros
plotResiduals(residuos_m2, form = fitted(modelo2)) # Residuos frente a valores ajustados
testOutliers(residuos_m2)                          # Test Outliers


sum(residuals(modelo2, type="pearson")^2)/df.residual(modelo2)

# Da todo bien excepto dispersión, pruebo usando dispformula

modelo2_disp <- glmmTMB(
  Brazos_abiertos_entradas ~ FLX_SUC * SAL_CNO * sexo + (1 | camada),
  dispformula = ~ FLX_SUC * SAL_CNO * sexo,
  family = "nbinom2",
  data = datos
)

residuos_m2disp <- simulateResiduals(modelo2_disp)          # Simulamos residuos con DHARMa

plot(residuos_m2disp)
testUniformity(residuos_m2disp)                        # Test uniformidad
testDispersion(residuos_m2disp)                        # Test dispersión
testZeroInflation(residuos_m2disp)                     # Test inflación en ceros
plotResiduals(residuos_m2disp, form = fitted(modelo2_disp)) # Residuos frente a valores ajustados
testOutliers(residuos_m2disp)                          # Test Outliers


sum(residuals(modelo2_disp, type="pearson")^2)/df.residual(modelo2_disp)

# Ahora da no significativo el test de dispersión, nos quedamos con este modelo2

# Supuestos efectos aleatorios -------------------------------------------------

ef_camadas_m2 <- ranef(modelo2_disp)$cond$camada[["(Intercept)"]]

qqPlot(
  ef_camadas_m2,
  main = "QQ Plot de efectos aleatorios de las camadas",
  ylab = "Efectos aleatorios estimados"
)

shapiro.test(ef_camadas_m2)       # Shapiro-Wilks test

# Modelo 3: Conway-Maxwell-Poisson =============================================

# Ajuste del modelo ------------------------------------------------------------

modelo3 <- glmmTMB(
  Brazos_abiertos_entradas ~ FLX_SUC * SAL_CNO * sexo + (1 | camada),
  family = "compois",
  data = datos
)

summary(modelo3)
Anova(modelo3, type = "III")
Anova(modelo3, type = "II")

#### Supuestos efectos fijos ---------------------------------------------------

residuos_m3 <- simulateResiduals(modelo3)          # Simulamos residuos con DHARMa

plot(residuos_m3)
testUniformity(residuos_m3)                        # Test uniformidad
testDispersion(residuos_m3)                        # Test dispersión
testZeroInflation(residuos_m3)                     # Test inflación en ceros
plotResiduals(residuos_m3, form = fitted(modelo3)) # Residuos frente a valores ajustados
testOutliers(residuos_m3)                          # Test Outliers


sum(residuals(modelo3, type="pearson")^2)/df.residual(modelo3)

# Da todo bien excepto dispersión, pruebo usando dispformula

modelo3_disp <- glmmTMB(
  Brazos_abiertos_entradas ~ FLX_SUC * SAL_CNO * sexo + (1 | camada),
  dispformula = ~ FLX_SUC * SAL_CNO * sexo,
  family = "compois",
  data = datos
)

residuos_m3disp <- simulateResiduals(modelo3_disp)          # Simulamos residuos con DHARMa

plot(residuos_m3disp)
testUniformity(residuos_m3disp)                        # Test uniformidad
testDispersion(residuos_m3disp)                        # Test dispersión
testZeroInflation(residuos_m3disp)                     # Test inflación en ceros
plotResiduals(residuos_m3disp, form = fitted(modelo3_disp)) # Residuos frente a valores ajustados
testOutliers(residuos_m3disp)                          # Test Outliers

sum(residuals(modelo3_disp, type="pearson")^2)/df.residual(modelo3_disp)

# Ahora da no significativo el test de dispersión, nos quedamos con este modelo3

#### Supuestos efectos aleatorios ----------------------------------------------

ef_camadas_m3 <- ranef(modelo3_disp)$cond$camada[["(Intercept)"]]

qqPlot(
  ef_camadas_m3,
  main = "QQ Plot de efectos aleatorios de las camadas",
  ylab = "Efectos aleatorios estimados"
)

shapiro.test(ef_camadas_m3)       # Shapiro-Wilks test

# Modelo 4: Tweedie ============================================================

#### Ajuste del modelo ---------------------------------------------------------

modelo4 <- glmmTMB(
  Brazos_abiertos_entradas ~ FLX_SUC * SAL_CNO * sexo + (1 | camada),
  family = "tweedie",
  data = datos
)

summary(modelo4)
Anova(modelo4, type = "III")
Anova(modelo4, type = "II")

#### Supuestos efectos fijos ---------------------------------------------------

residuos_m4 <- simulateResiduals(modelo4)          # Simulamos residuos con DHARMa

plot(residuos_m4)
testUniformity(residuos_m4)                        # Test uniformidad
testDispersion(residuos_m4)                        # Test dispersión
testZeroInflation(residuos_m4)                     # Test inflación en ceros
plotResiduals(residuos_m4, form = fitted(modelo4)) # Residuos frente a valores ajustados
testOutliers(residuos_m4)                          # Test Outliers


sum(residuals(modelo4, type="pearson")^2)/df.residual(modelo4)

# Da todo bien excepto dispersión, pruebo usando dispformula

modelo4_disp <- glmmTMB(
  Brazos_abiertos_entradas ~ FLX_SUC * SAL_CNO * sexo + (1 | camada),
  dispformula = ~ FLX_SUC * SAL_CNO * sexo,
  family = "tweedie",
  data = datos
)

residuos_m4disp <- simulateResiduals(modelo4_disp)          # Simulamos residuos con DHARMa

plot(residuos_m4disp)
testUniformity(residuos_m4disp)                        # Test uniformidad
testDispersion(residuos_m4disp)                        # Test dispersión
testZeroInflation(residuos_m4disp)                     # Test inflación en ceros
plotResiduals(residuos_m4disp, form = fitted(modelo4_disp)) # Residuos frente a valores ajustados
testOutliers(residuos_m4disp)                          # Test Outliers

sum(residuals(modelo3_disp, type="pearson")^2)/df.residual(modelo3_disp)

#### Supuestos efectos aleatorios ----------------------------------------------

ef_camadas_m4 <- ranef(modelo4_disp)$cond$camada[["(Intercept)"]]

qqPlot(
  ef_camadas_m4,
  main = "QQ Plot de efectos aleatorios de las camadas",
  ylab = "Efectos aleatorios estimados"
)

shapiro.test(ef_camadas_m4)       # Shapiro-Wilks test

# Modelo 5: OLRE ===============================================================

#### Ajuste del modelo ---------------------------------------------------------

modelo5 <- glmmTMB(
  Brazos_abiertos_entradas ~ FLX_SUC * SAL_CNO * sexo + (1 | camada) + (1 | ID),
  family = "poisson",
  data = datos
)

summary(modelo5)
Anova(modelo5, type = "III")
Anova(modelo5, type = "II")

#### Supuestos efectos fijos ---------------------------------------------------

residuos_m5 <- simulateResiduals(modelo5)          # Simulamos residuos con DHARMa

plot(residuos_m5)
testUniformity(residuos_m5)                        # Test uniformidad
testDispersion(residuos_m5)                        # Test dispersión
testZeroInflation(residuos_m5)                     # Test inflación en ceros
plotResiduals(residuos_m5, form = fitted(modelo5)) # Residuos frente a valores ajustados
testOutliers(residuos_m5)                          # Test Outliers


sum(residuals(modelo5, type="pearson")^2)/df.residual(modelo5)

# Da mal el KS Test (uniformidad), NO es un modelo candidato, no se compara con el resto por AIC

#### Supuestos efectos aleatorios ----------------------------------------------

ef_camadas_m5 <- ranef(modelo5)$cond$camada[["(Intercept)"]]

qqPlot(
  ef_camadas_m5,
  main = "QQ Plot de efectos aleatorios de las camadas",
  ylab = "Efectos aleatorios estimados"
)

shapiro.test(ef_camadas_m5)       # Shapiro-Wilks test

## Comparación de modelos por AIC ==============================================

AIC(modelo2_disp, modelo3_disp, modelo4_disp)
