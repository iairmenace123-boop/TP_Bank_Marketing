library(tidyverse)
library(ggplot2)
library(dplyr)

#ANALISIS DE OUTLIERS Y DATOS FALTANTES

datos_filtrados <- datos %>%
  filter(duration >= 6)

variables <- c("duration", "age", "euribor3m")

outliers_iqr <- datos_filtrados %>%
  select(all_of(variables)) %>%
  summarise(across(
    everything(),
    ~ {
      Q1 <- quantile(.x, 0.25, na.rm = TRUE)
      Q3 <- quantile(.x, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      lower <- Q1 - 1.5 * IQR
      upper <- Q3 + 1.5 * IQR
      sum(.x < lower | .x > upper, na.rm = TRUE)
    },
    .names = "outliers_{col}"
  ))

outliers_iqr
#determinamos el percentil 0.99 para usar el metodo winsorización
limite1 <- quantile(datos$duration, 0.99, na.rm = TRUE)
limite <- round(limite1,0)
limite
datos_filtrados$duration <- pmin(datos_filtrados$duration, limite)

write.csv(datos_filtrados, "datos_limpios.csv", row.names = FALSE)
getwd()

#Analisis de impacto

summary(datos_filtrados$duration)
summary(datos$duration)

ggplot(datos_filtrados, aes(x = duration)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    title = "Distribución de duración s/outliers",
    x = "Duracción",
    y = ""
  ) +
  theme_minimal()
#minimo cambio
library(sandwich)
library(lmtest)

# H0: si las variabes son relevantes para explicar
#

regresion1<-lm(y_z ~ duration + age + euribor3m + contact, data=datos_filtrados)
summary(regresion1)
vcov_hc3 <- vcovHC(regresion1, type = "HC3")
coeftest(regresion1, vcov. = vcov_hc3)

#Interpretacion de los coeficientes, si confirma lo que pensabamos y explicacion 
#posible de efecto causal

#Fly to quality, faltarian variables que 
library(broom)
# Modelo de regresión
modelo <- lm(y_z ~ duration, data = datos_filtrados)

# Probabilidades predichas
datos_filtrados$prob_pred <- predict(regresion1)

datos_filtrados$y_pred <- ifelse(datos_filtrados$prob_pred >= 0.5, 1, 0)

# Matriz de confusión
tabla_confusion <- table(
  Real = datos_filtrados$y_z,
  Predicho = datos_filtrados$y_pred
)

tabla_confusion

# Accuracy
accuracy <- sum(diag(tabla_confusion)) / sum(tabla_confusion)
accuracy



hist(datos_filtrados$prob_pred,
     breaks = 40,
     xlab = "Probabilidad predicha",
     main = "Distribución de probabilidades predichas")
################################################################################