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
    camada = factor(camada),
    # Fijación explícita de niveles de referencia (baselines)
 #   sexo = relevel(factor(sexo), ref = "hembra"),
  #  FLX_SUC = relevel(factor(FLX_SUC), ref = "SUC"),
   # SAL_CNO = relevel(factor(SAL_CNO), ref = "SAL")
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
    Brazos_abiertos_entradas     # VR discreta: conteo en continuo
  )

# Verficamos que se hayan realizado bien los cambios
Datos
str(Datos)
summary(Datos)
head(Datos)

# Ratones por camada
table(Datos$camada)

# Ratones por combinación de VEs fijas
ftable(Datos$FLX_SUC, Datos$SAL_CNO, Datos$sexo)

Anova(modelo2, type = "II")
# ESTADÍSTICA DESCRIPTIVA ######################################################

## Tablas descriptivas =========================================================

### Tabla resumen de los datos por tratamiento ---------------------------------

resumen <- Datos %>% 
  group_by(FLX_SUC, SAL_CNO) %>%
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

p1 <- ggplot(Datos, aes(x = interaction(FLX_SUC, SAL_CNO, sep = " + ", lex.order = TRUE),
                        y = Brazos_abiertos_entradas)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = factor(camada)), width = 0.15, size = 2, alpha = 0.8) +
  labs(x = "Grupo de tratamiento",
       y = "Nº de entradas a brazos abiertos (en 10 min)",
       color = "Camada") +
  theme_classic()

p1

### Boxplot mas sencillo, solo con outliers ------------------------------------

p2 <- ggplot(Datos, aes(x = interaction(FLX_SUC, SAL_CNO, sep = " + ", lex.order = TRUE),
                        y = Brazos_abiertos_entradas)) +
  geom_boxplot(fill = "white", color = "black") +
  labs(x = "Grupo de tratamiento",
       y = "Nº de entradas a brazos abiertos (en 15 min)") +
  theme_classic()

p2

### Boxplot diferenciado por sexo ----------------------------------------------

p3 <- ggplot(Datos, aes(x = interaction(FLX_SUC, SAL_CNO, sep = " + ", lex.order = TRUE),
                        y = Brazos_abiertos_entradas,
                        fill = sexo)) +
  geom_boxplot(color = "black") +
  facet_wrap(~sexo,
             labeller = as_labeller(c(hembra = "Hembras", macho = "Machos"))) +
  scale_fill_manual(values = c("hembra" = "#F5793A", "macho" = "#56B4E9")) +
  labs(x = "Grupo de tratamiento",
       y = "Nº de entradas a brazos abiertos (en 10 min)") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust = 1))

p3

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

library(performance)

icc(modelo2)

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

icc(modelo5)

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


# RESULTADOS ###################################################################

summary(modelo2)
Anova(modelo2, type = "III")
Anova(modelo2, type = "II")

#####NICOLE####

options(emmeans= list(emmeans = list(infer = c(TRUE, F)),
                      contrast = list(infer = c(TRUE, TRUE))))

efsimple_escalalog<- emmeans(modelo2, pairwise ~ SAL_CNO | sexo)
confint(efsimple_escalalog)

efsimple_escalarespuesta<- emmeans(modelo2, pairwise ~ SAL_CNO | sexo,type="response")
confint(efsimple_escalarespuesta,infer = TRUE)

summary(efsimple_escalarespuesta$contrasts, infer = c(TRUE, TRUE)) #con los p valores


plot(efsimple_escalarespuesta, comparisons = TRUE)

library(emmeans)
library(ggplot2)

library(emmeans)
library(ggplot2)

emm <- emmeans(modelo2, ~ SAL_CNO | sexo, type = "response")
emm_df <- as.data.frame(emm)
emm_df$SAL_CNO <- factor(emm_df$SAL_CNO, levels = c("SAL", "CNO"))

colores <- c("hembra" = "#F5793A", "macho" = "#56B4E9")


y_max   <- max(emm_df$asymp.UCL[emm_df$sexo == "macho"])
y_barra <- y_max * 1.08
y_pata  <- y_barra * 0.97   

ggplot(emm_df, aes(x = SAL_CNO, y = response, colour = sexo)) +
  geom_linerange(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                 position = position_dodge(width = 0.4),
                 linewidth = 1.2) +
  geom_point(position = position_dodge(width = 0.4),
             size = 3.5) +
  
  annotate("segment", x = 1.1, xend = 2.1, y = y_barra, yend = y_barra,
           colour = "#56B4E9", linewidth = 0.8) +
  annotate("segment", x = 1.1, xend = 1.1, y = y_pata, yend = y_barra,
           colour = "#56B4E9", linewidth = 0.8) +
  annotate("segment", x = 2.1, xend = 2.1, y = y_pata, yend = y_barra,
           colour = "#56B4E9", linewidth = 0.8) +
  annotate("text", x = 1.6, y = y_barra * 1.02, label = "*",
           colour = "#56B4E9", size = 8) +
  scale_colour_manual(values = colores,
                      labels = c("hembra" = "Hembra", "macho" = "Macho")) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.12))) +
  labs(x = "Tratamiento",
       y = "Media estimada de entradas a brazos abiertos",
       colour = "Sexo") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor.y = element_line(colour = "grey95"),
    panel.grid.minor.x = element_blank(),
    axis.line = element_line(colour = "grey50", linewidth = 0.5),
    axis.ticks = element_line(colour = "grey50"),
    axis.ticks.length = unit(0.2, "cm"),
    axis.text = element_text(colour = "grey40", size = 12),
    axis.title = element_text(colour = "grey20", size = 15),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "right"
  )
##### NAZA

# Coeficientes del modelo
library(emmeans)

cel <- as.data.frame(emmeans(modelo2, ~ FLX_SUC * SAL_CNO * sexo))   # escala log
mu  <- mean(cel$emmean)

alfa <- as.data.frame(emmeans(modelo2, ~ FLX_SUC))
alfa$alfa <- alfa$emmean - mu

beta <- as.data.frame(emmeans(modelo2, ~ SAL_CNO))
beta$beta <- beta$emmean - mu

gamma <- as.data.frame(emmeans(modelo2, ~ sexo))
gamma$gamma <- gamma$emmean - mu

cel$alfabeta <- cel$emmean - mu -
  alfa$alfa[match(cel$FLX_SUC, alfa$FLX_SUC)] -
  beta$beta[match(cel$SAL_CNO, beta$SAL_CNO)]

cel$alfagamma <- cel$emmean - mu -
  alfa$alfa[match(cel$FLX_SUC, alfa$FLX_SUC)] -
  gamma$gamma[match(cel$sexo, gamma$sexo)]

cel$betagamma <- cel$emmean - mu -
  beta$beta[match(cel$SAL_CNO, beta$SAL_CNO)] -
  gamma$gamma[match(cel$sexo, gamma$sexo)]

cel$alfabetagamma <- cel$emmean - mu -
  alfa$alfa[match(cel$FLX_SUC, alfa$FLX_SUC)] -
  beta$beta[match(cel$SAL_CNO, beta$SAL_CNO)] -
  gamma$gamma[match(cel$sexo, gamma$sexo)] 


library(emmeans)
library(dplyr)

cel <- as.data.frame(emmeans(modelo2, ~ FLX_SUC * SAL_CNO * sexo))
mu  <- mean(cel$emmean)

# Efectos principales
alfa  <- as.data.frame(emmeans(modelo2, ~ FLX_SUC)) %>% mutate(alfa  = emmean - mu)
beta  <- as.data.frame(emmeans(modelo2, ~ SAL_CNO)) %>% mutate(beta  = emmean - mu)
gamma <- as.data.frame(emmeans(modelo2, ~ sexo))    %>% mutate(gamma = emmean - mu)

# Interacciones dobles (a partir de las marginales de 2 factores)
ab <- as.data.frame(emmeans(modelo2, ~ FLX_SUC * SAL_CNO)) %>%
  left_join(select(alfa, FLX_SUC, alfa), by = "FLX_SUC") %>%
  left_join(select(beta, SAL_CNO, beta), by = "SAL_CNO") %>%
  mutate(alfabeta = emmean - mu - alfa - beta)

ag <- as.data.frame(emmeans(modelo2, ~ FLX_SUC * sexo)) %>%
  left_join(select(alfa, FLX_SUC, alfa), by = "FLX_SUC") %>%
  left_join(select(gamma, sexo, gamma), by = "sexo") %>%
  mutate(alfagamma = emmean - mu - alfa - gamma)

bg <- as.data.frame(emmeans(modelo2, ~ SAL_CNO * sexo)) %>%
  left_join(select(beta, SAL_CNO, beta), by = "SAL_CNO") %>%
  left_join(select(gamma, sexo, gamma), by = "sexo") %>%
  mutate(betagamma = emmean - mu - beta - gamma)

# Interacción triple (por celda)
cel <- cel %>%
  select(FLX_SUC, SAL_CNO, sexo, emmean) %>%
  left_join(select(alfa,  FLX_SUC, alfa),  by = "FLX_SUC") %>%
  left_join(select(beta,  SAL_CNO, beta),  by = "SAL_CNO") %>%
  left_join(select(gamma, sexo, gamma),    by = "sexo") %>%
  left_join(select(ab, FLX_SUC, SAL_CNO, alfabeta),  by = c("FLX_SUC", "SAL_CNO")) %>%
  left_join(select(ag, FLX_SUC, sexo, alfagamma),    by = c("FLX_SUC", "sexo")) %>%
  left_join(select(bg, SAL_CNO, sexo, betagamma),    by = c("SAL_CNO", "sexo")) %>%
  mutate(alfabetagamma = emmean - mu - alfa - beta - gamma -
           alfabeta - alfagamma - betagamma)
  
# Estimación de medias
  
emm_options(emmeans = list(infer = c(TRUE, FALSE)),
            contrast = list(infer = c(TRUE, TRUE)))

# 1. Obtener las medias estimadas del modelo
medias.log <- emmeans(modelo2, ~ FLX_SUC * SAL_CNO * sexo, type = "link")
medias.log
medias.rta <- emmeans(modelo2, ~ FLX_SUC * SAL_CNO * sexo, type = "response")
medias.rta


# Comparaciones

# El orden interno de las 8 celdas en la grilla de emmeans es:
# 1: FLX     CNO     hembra
# 2: SUC     CNO     hembra
# 3: FLX     SAL     hembra 
# 4: SUC     SAL     hembra 
# 5: FLX     CNO     macho
# 6: SUC     CNO     macho
# 7: FLX     SAL     macho 
# 8: SUC     SAL     macho



tuk <- emmeans(modelo2,~ FLX_SUC * SAL_CNO * sexo,
               type = "response", adjust = "tukey")
plot(tuk$emmeans, comparisons = TRUE)
tuk
contrast(tuk, method = "pairwise")

ef_todas <- emmeans(modelo2, pairwise ~ SAL_CNO * sexo,
                    type = "response", adjust = "tukey")
plot(ef_todas$emmeans, comparisons = TRUE)
ef_todas

ef_todasxsexo <- emmeans(modelo2, pairwise ~ SAL_CNO | sexo,
                    type = "response", adjust = "tukey")
plot(ef_todas$emmeans, comparisons = TRUE)
ef_todasxsexo

emm_options(emmeans = list(infer = c(TRUE, FALSE)),
            contrast = list(infer = c(TRUE, TRUE)))


# Medias ajustadas por combinación
mediasxsexo <- emmeans(modelo2, ~ FLX_SUC * SAL_CNO | sexo, type = "response")
mediasxsexo

medias <- emmeans(modelo2, ~ FLX_SUC * SAL_CNO, type = "response")
medias

# --- CONTRASTE 1: Fenotipo Ansioso (FLX SAL vs SUC SAL) ---
# Evaluamos si FLX_SAL disminuye las entradas respecto a SUC_SAL dentro de cada sexo o general
c_fenotipo <- contrast(medias, method = "pairwise", combine = TRUE)
c_fenotipo

# --- CONTRASTE 2: Rescate por CNO en FLX (FLX CNO vs FLX SAL) ---
# Evaluamos si FLX_CNO aumenta las entradas respecto a FLX_SAL
# Se extrae directamente de las comparaciones dentro de FLX_SUC = SUC / FLX:
emm_rescate <- emmeans(modelo2, ~ SAL_CNO | FLX_SUC * sexo, type = "response")
contrast(emm_rescate, method = "revpairwise") # Muestra CNO / SAL

# --- CONTRASTE 3: Diferencia de Sexo en FLX SAL (Hembra FLX SAL vs Macho FLX SAL) ---
emm_sexo <- emmeans(modelo2, ~ sexo | FLX_SUC * SAL_CNO, type = "response")
contrast(emm_sexo, method = "pairwise") # Muestra hembra / macho o viceversa




library(emmeans)


# 1. Obtener las medias estimadas del modelo
medias.log <- emmeans(modelo2, ~ FLX_SUC * SAL_CNO * sexo, type = "link")
medias.log
medias.rta <- emmeans(modelo2, ~ FLX_SUC * SAL_CNO * sexo, type = "response")
medias.rta

# El orden interno de las 8 celdas en la grilla 'emm' (por orden alfabético de R) es:
# 1: FLX     CNO     hembra
# 2: SUC     CNO     hembra
# 3: FLX     SAL     hembra 
# 4: SUC     SAL     hembra 
# 5: FLX     CNO     macho
# 6: SUC     CNO     macho
# 7: FLX     SAL     macho 
# 8: SUC     SAL     macho

hipotesis <- list(
  # H1: FLX_SAL vs SUC_SAL (Promediando ambos sexos)
  # Compara el promedio de [3: FLX_SAL_H, 7: FLX_SAL_M] vs [4: SUC_SAL_H, 8: SUC_SAL_M]
  "H1: FLX_SAL vs SUC_SAL (General)"       = c(0, 0,  0.5, -0.5, 0, 0,  0.5, -0.5),
  
  # H2: FLX_CNO vs FLX_SAL (Promediando ambos sexos)
  # Compara el promedio de [1: FLX_CNO_H, 5: FLX_CNO_M] vs [3: FLX_SAL_H, 7: FLX_SAL_M]
  "H2: FLX_CNO vs FLX_SAL (General)"       = c(0.5, 0, -0.5,  0, 0.5, 0, -0.5,  0),
  
  # H3: Hembra FLX_SAL vs Macho FLX_SAL (Específica por sexo)
  # Compara directamente el nivel 3 (FLX_SAL_Hembra) vs el nivel 7 (FLX_SAL_Macho)
  "H3: Hembra vs Macho (en FLX_SAL)"       = c(0, 0,  1,    0,   0, 0, -1,    0)
)

# Ejecutar y mostrar ÚNICAMENTE estas 3 comparaciones en la escala real (Ratio de entradas)
res_final <- contrast(emm, hipotesis, type = "response", adjust = "none")
summary(res_final, infer = c(TRUE, TRUE))

# Verificación de ortogonalidad de contrastes

c1 <- c( 0,  0,  0.5,  -0.5, 0, 0,  0.5,  -0.5)
c2 <- c(0.5, 0,  -0.5,  0,  0.5,  0,  -0.5,  0)
c3 <- c( 0, 0,  1,  0,  0,  0,  -1,  0)

sum(c1 * c2)  
sum(c1 * c3)  # 0 ✓
sum(c2 * c3)

contrastes <- list("SUC vs FLX" = c1, "CNO vs SAL" = c2, "FLX machos vs FLX hembras" = c3)

# Bonferroni
res_bonf <- contrast(medias.log, contrastes, type = "response", adjust = "bonferroni")
res_bonf
summary(res_bonf, infer = c(TRUE, TRUE))

# Opción 2: Verificación de ortogonalidad de contrastes

# H1a: SUC vs FLX en hembras
c1a <- c( 0,  0,  1,  -1, 0, 0,  0,  0)
# H1b: SUC vs FLX en machos
c1b <- c( 0,  0,  0,  0, 0, 0,  1,  -1)
# H2: CNO vs SAL promediando hembras y machos
c2 <- c(0.5, 0,  -0.5,  0,  0.5,  0,  -0.5,  0)
# H3: Hembra FLX vs Macho FLX
c3 <- c( 0, 0,  1,  0,  0,  0,  -1,  0)

sum(c1a * c1b) 
sum(c1a * c2)  
sum(c1a * c3) 
sum(c1b * c2) 
sum(c1b * c3)
sum(c2 * c3)


# H1a: SUC vs FLX en hembras
c1a <- c( 0,  0,  1,  -1, 0, 0,  0,  0)
# H1b: SUC vs FLX en machos
c1b <- c( 0,  0,  0,  0, 0, 0,  1,  -1)
# H2: CNO vs SAL promediando hembras y machos
c2a <- c(1, 0,  -1,  0,  0,  0,  0,  0)
c2b <- c(0, 0,  0,  0,  1,  0,  1,  0)
# H3: Hembra FLX vs Macho FLX
c3 <- c( 0, 0,  1,  0,  0,  0,  -1,  0)

sum(c1a * c1b) 
sum(c1a * c2a) 
sum(c1a * c2b) 
sum(c1a * c3) 
sum(c1b * c2a) 
sum(c1b * c2b) 
sum(c1b * c3)
sum(c2a * c2b)
sum(c2a * c3)
sum(c2b * c3)



# ==============================================================================
# SECCIÓN 5: RESULTADOS Y CONTRASTES A PRIORI
# ==============================================================================

library(emmeans)
library(car)

# 1. Evaluación de efectos fijos globales mediante ANOVA de Wald Tipo III
Anova(modelo2, type = "III")

# 2. Generación de la grilla de medias marginales estimadas (escala log)
emm <- emmeans(modelo2, ~ FLX_SUC * SAL_CNO * sexo)

# ------------------------------------------------------------------------------
# COMPROBACIÓN AUTOMÁTICA DE LA GRILLA Y VECTOR DE CONTRASTES
# ------------------------------------------------------------------------------
# Convertimos la grilla a dataframe para inspeccionar el orden exacto de las celdas
grilla_df <- as.data.frame(emm)[, c("FLX_SUC", "SAL_CNO", "sexo")]
grilla_df$Fila <- 1:nrow(grilla_df)

cat("\n==================================================================\n")
cat("COMPROBACIÓN 1: GRILLA DE MEDIAS Y ORDEN DE NIVELES EN EMMEANS\n")
cat("==================================================================\n")
print(grilla_df[, c("Fila", "FLX_SUC", "SAL_CNO", "sexo")])

# ------------------------------------------------------------------------------
# DEFINICIÓN DE VECTORES DE CONTRASTE (Ajustados dinámicamente según la grilla)
# ------------------------------------------------------------------------------
# Identificamos las filas correspondientes a cada grupo para evitar errores manuales:
f_flx_sal_h <- which(grilla_df$FLX_SUC == "FLX" & grilla_df$SAL_CNO == "SAL" & grilla_df$sexo == "hembra")
f_suc_sal_h <- which(grilla_df$FLX_SUC == "SUC" & grilla_df$SAL_CNO == "SAL" & grilla_df$sexo == "hembra")

f_flx_sal_m <- which(grilla_df$FLX_SUC == "FLX" & grilla_df$SAL_CNO == "SAL" & grilla_df$sexo == "macho")
f_suc_sal_m <- which(grilla_df$FLX_SUC == "SUC" & grilla_df$SAL_CNO == "SAL" & grilla_df$sexo == "macho")

f_flx_cno_h <- which(grilla_df$FLX_SUC == "FLX" & grilla_df$SAL_CNO == "CNO" & grilla_df$sexo == "hembra")
f_flx_cno_m <- which(grilla_df$FLX_SUC == "FLX" & grilla_df$SAL_CNO == "CNO" & grilla_df$sexo == "macho")

# Construcción programática de los vectores de contraste (longitud 8)
vec_h1a <- numeric(8); vec_h1a[f_flx_sal_h] <- 1; vec_h1a[f_suc_sal_h] <- -1
vec_h1b <- numeric(8); vec_h1b[f_flx_sal_m] <- 1; vec_h1b[f_suc_sal_m] <- -1

vec_h2  <- numeric(8)
vec_h2[f_flx_cno_h] <- 0.5; vec_h2[f_flx_cno_m] <- 0.5
vec_h2[f_flx_sal_h] <- -0.5; vec_h2[f_flx_sal_m] <- -0.5

vec_h3  <- numeric(8); vec_h3[f_flx_sal_h] <- 1; vec_h3[f_flx_sal_m] <- -1

# Lista de hipótesis planteadas
hipotesis <- list(
  "H1a: FLX_SAL vs SUC_SAL (Hembras)"         = vec_h1a,
  "H1b: FLX_SAL vs SUC_SAL (Machos)"          = vec_h1b,
  "H2:  FLX_CNO vs FLX_SAL (Rescate CNO Gen)" = vec_h2,
  "H3:  Hembra vs Macho (en FLX_SAL)"         = vec_h3
)

# ------------------------------------------------------------------------------
# COMPROBACIÓN 2: VERIFICACIÓN MATEMÁTICA Y DE PESOS ASIGNADOS
# ------------------------------------------------------------------------------
cat("\n==================================================================\n")
cat("COMPROBACIÓN 2: MATRIZ DE PESOS Y VERIFICACIÓN DE SUMA CERO\n")
cat("==================================================================\n")

matriz_pesos <- do.call(rbind, hipotesis)
colnames(matriz_pesos) <- paste0("Fila_", 1:8)
cat("Suma de coeficientes por contraste (debe ser siempre 0):\n")
print(rowSums(matriz_pesos))

cat("\nMatriz detallada de coeficientes aplicados:\n")
print(matriz_pesos)

# ------------------------------------------------------------------------------
# EJECUCIÓN DE CONTRASTES EN ESCALA NATURAL (RATIO DE CONTEOS)
# ------------------------------------------------------------------------------
comp_apriori <- contrast(emm, hipotesis, type = "response", adjust = "none")

cat("\n==================================================================\n")
cat("RESULTADOS DE CONTRASTES A PRIORI (Escala real de la VR)\n")
cat("==================================================================\n")
summary(comp_apriori, infer = c(TRUE, TRUE))


emm <- emmeans(modelo, ~ tratamiento)

# Definir la matriz de contrastes
mis_contrastes <- list(
  "A vs resto"   = c( 3, -1, -1, -1) / 6,
  "B vs (C, D)"  = c( 0,  2, -1, -1) / 4,
  "C vs D"       = c( 0,  0,  1, -1) / 2
)

contrast(emm, method = mis_contrastes)

# Con intervalos de confianza
confint(contrast(emm, method = mis_contrastes))

contrast(emm, method = mis_contrastes, type = "response")

#Si los grupos tienen distintos tamaños (diseño desbalanceado), la ortogonalidad ponderada requiere dividir por el $n$ de cada grupo:

#n <- c(10, 10, 10, 10)  # tamaños de cada grupo

sum(c1 * c2 / n)  # debe ser 0 para ortogonalidad ponderada




library(emmeans)
library(car)

# 1. Generar la grilla de medias marginales estimadas (escala log)
emm <- emmeans(modelo2, ~ FLX_SUC * SAL_CNO * sexo)

# 2. Verificar el orden exacto de las 8 celdas en tu grilla de R:
grilla_df <- as.data.frame(emm)[, c("FLX_SUC", "SAL_CNO", "sexo")]
print(grilla_df)
# Orden alfabético por defecto en R:
# 1: FLX, CNO, hembra | 2: SUC, CNO, hembra | 3: FLX, SAL, hembra | 4: SUC, SAL, hembra
# 5: FLX, CNO, macho  | 6: SUC, CNO, macho  | 7: FLX, SAL, macho  | 8: SUC, SAL, macho

# 3. Definición de la matriz de contrastes a priori
mis_contrastes <- list(
  # H1: FLX_SAL vs SUC_SAL (Efecto Fluoxetina promediando sexos)
  "H1: FLX_SAL vs SUC_SAL (Efecto FLX)"      = c( 0,   0,   0.5, -0.5,  0,   0,   0.5, -0.5),
  
  # H2: FLX_CNO vs FLX_SAL (Rescate por inhibición de CPF promediando sexos)
  "H2: FLX_CNO vs FLX_SAL (Rescate CNO)"     = c( 0.5, 0,  -0.5,  0,   0.5,  0,  -0.5,  0),
  
  # H3: Hembra FLX_SAL vs Macho FLX_SAL (Resiliencia por sexo)
  "H3: Hembra vs Macho en FLX_SAL"           = c( 0,   0,   1,    0,   0,   0,  -1,    0)
)

# 4. Calcular contrastes en la escala del predictor lineal (diferencia de logs)
res_link <- contrast(emm, method = mis_contrastes)
print(res_link)

# 5. Calcular contrastes en la escala real de la VR (Ratio de conteos e Intervalos de Confianza del 95%)
res_response <- contrast(emm, method = mis_contrastes, type = "response")

# Mostrar p-valores e intervalos de confianza simultáneamente
summary(res_response, infer = c(TRUE, TRUE))

# Alternativa para ver solo los intervalos de confianza:
confint(res_response)


# ------------------------------------------------------------------------------
# CONTRASTES BONFERRONI CODIGO DE CELI (NO CHEQUEADO POR NAZA)
# ------------------------------------------------------------------------------
medias.log <- emmeans(modelo2, ~ FLX_SUC * SAL_CNO * sexo, type = "link")
print(medias.log)
#FLX_SUC SAL_CNO sexo   emmean    SE  df asymp.LCL asymp.UCL
#SUC     SAL     hembra   1.78 0.244 Inf     1.303      2.26
#FLX     SAL     hembra   2.21 0.203 Inf     1.816      2.61
#SUC     CNO     hembra   1.50 0.236 Inf     1.041      1.97
#FLX     CNO     hembra   1.87 0.190 Inf     1.501      2.24
#SUC     SAL     macho    1.66 0.228 Inf     1.215      2.11
#FLX     SAL     macho    1.43 0.228 Inf     0.987      1.88
#SUC     CNO     macho    1.90 0.230 Inf     1.451      2.35
#FLX     CNO     macho    2.24 0.194 Inf     1.858      2.62

contrastes_planeados <- list(
  # H1: FLX_SAL [filas 2, 6] vs SUC_SAL [filas 1, 5]
  "H1: FLX_SAL vs SUC_SAL (General)" = c(-0.5,  0.5, 0,   0, -0.5,  0.5, 0,   0),
  
  # H2: FLX_CNO [filas 4, 8] vs FLX_SAL [filas 2, 6]
  "H2: FLX_CNO vs FLX_SAL (General)" = c( 0,   -0.5, 0, 0.5,  0,   -0.5, 0, 0.5),
  
  # H3: Hembra FLX_SAL [fila 2] vs Macho FLX_SAL [fila 6]
  "H3: Hembra vs Macho (en FLX_SAL)" = c( 0,    1,   0,   0,  0,   -1,   0,   0)
)
# EN ESCALA log (LINK)
bonfe_log<-contrast(medias.log, method = contrastes_planeados, type = "link", adjust = "bonferroni")
bonfe_log
# EN ESCALA NATURAL (RATIO DE CONTEOS)
bonfe_resp<-contrast(medias.log, method = contrastes_planeados, type = "response", adjust = "bonferroni")
bonfe_resp


# Gráfico final
library(emmeans)
library(ggplot2)

# Medias estimadas (escala respuesta) con IC 95%
emm <- as.data.frame(
  emmeans(modelo2, ~ FLX_SUC * SAL_CNO * sexo, type = "response")
)
niveles <- c("SUC SAL", "FLX SAL", "FLX CNO", "SUC CNO")
emm$grupo <- factor(paste(emm$FLX_SUC, emm$SAL_CNO), levels = niveles)

# Estrellas desde los contrastes (orden: c1, c2, c3)
comp <- as.data.frame(comp_vr)
est <- as.character(cut(comp$p.value,
                        breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                        labels = c("***", "**", "*", "ns")))

ymax <- max(emm$asymp.UCL)
tick <- 0.03 * ymax
dodge_w <- 0.5
off <- dodge_w / 4          # distancia del centro a cada punto (hembra/macho)

corchete <- function(x, xend, y, label) {
  list(
    annotate("segment", x = x,    xend = xend, y = y,        yend = y),
    annotate("segment", x = x,    xend = x,    y = y - tick, yend = y),
    annotate("segment", x = xend, xend = xend, y = y - tick, yend = y),
    annotate("text", x = (x + xend) / 2, y = y + tick, label = label,
             vjust = 0, size = if (label == "ns") 4.5 else 7)
  )
}

ggplot(emm, aes(x = grupo, y = response, color = sexo)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(dodge_w), width = 0, linewidth = 0.7) +
  geom_point(position = position_dodge(dodge_w), size = 3) +
  # c3: hembras vs machos dentro de FLX SAL (grupo 2)
  corchete(2 - off, 2 + off, ymax * 1.08, est[3]) +
  # c1: FLX vs SUC dentro de SAL (grupos 1 y 2)
  corchete(1, 2, ymax * 1.22, est[1]) +
  # c2: CNO vs SAL dentro de FLX (grupos 2 y 3)
  corchete(2, 3, ymax * 1.36, est[2]) +
  scale_color_manual(values = c(hembra = "#E41A1C", macho = "#377EB8"),
                     labels = c("Hembra", "Macho")) +
  scale_x_discrete(labels = c("SUC\nSAL", "FLX\nSAL", "FLX\nCNO", "SUC\nCNO")) +
  coord_cartesian(ylim = c(0, ymax * 1.55), clip = "off") +
  labs(x = "Tratamiento neonatal",
       y = "Entradas a brazos abiertos en 10 minutos predichas",
       color = "Sexo") +
  theme_bw(base_size = 14) +
  theme(panel.grid.minor = element_blank())

options(contrasts = c("contr.sum", "contr.poly"))
modelo2 <- glmmTMB(
  Brazos_abiertos_entradas ~ FLX_SUC * SAL_CNO * sexo + (1 | camada),
  family = "nbinom2",
  data = Datos
)


tabla.summary <- summary(modelo2)
tabla.anova <- Anova(modelo2, type="III")

options(contrasts = c("contr.treatment", "contr.poly"))

tabla.summary
tabla.anova
