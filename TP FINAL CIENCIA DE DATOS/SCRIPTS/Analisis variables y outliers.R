library(tidyverse)
library(ggplot2)
library(dplyr)

datos<-read.csv2("~/Facultad/2025/TP FINAL CIENCIA DE DATOS/DATA/Base cruda.csv")
#tipos de contactos
table(datos$contact) #Podemos ver que el 63% de los contactos de hicieron por celular
datos %>%
  count(contact) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = contact, y = pct)) +
  geom_col(fill = "#1f77b4") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Metodos de contacto",
       x = "Tipo de contacto",
       y = "Porcentaje")  #Grafico 1- Metodos de contacto

#contactos por mes
table(datos$month)
df_mes <- datos %>%
  count(month)
df_mes$month <- factor(df_mes$month,
                       levels = c("jan","feb","mar","apr","may","jun",
                                  "jul","aug","sep","oct","nov","dec"))
ggplot(df_mes, aes(x = month, y = n)) +
  geom_col(fill = "#1f77b4") +
  labs(title = "Cantidad de contactos por mes",
       x = "Mes",
       y = "Cantidad de contactos",       caption = "Nota: No hubo contactos en enero y febrero.")+
  theme_minimal()#Grafico 2 - Mes de contacto

suma_may_ago <- df_mes %>%
  filter(month %in% c("may", "jun", "jul", "aug")) %>%
  summarise(total = sum(n)) %>%
  pull(total)
total_general <- sum(df_mes$n)

suma_may_ago/total_general #% de contactos entre mayo y agosto
#duracion
table(datos$duration)
summary(datos$duration)
sd(datos$duration)
max(datos$duration)
Q1 <- quantile(datos$duration, 0.25, na.rm = TRUE)
Q3 <- quantile(datos$duration, 0.75, na.rm = TRUE)
IQR_value <- IQR(datos$duration, na.rm = TRUE)

lim_inf <- Q1 - 1.5 * IQR_value
lim_sup <- Q3 + 1.5 * IQR_value

# Ajustar límite inferior (duration no puede ser negativo)
lim_inf <- max(0, lim_inf)

# Ver límites corregidos
lim_inf
lim_sup

# Identificar outliers verdaderos
outliers <- datos %>% filter(duration < lim_inf | duration > lim_sup)
outliers


ggplot(datos, aes(x = log1p(duration))) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    title = "Distribución de duration (log-transformado)",
    x = "log(duration + 1)",
    y = "Frecuencia"
  ) +
  theme_minimal()

#edad
summary(datos$age)
hist(datos$age,
     main = "Distribución de edad",
     xlab = "Edad",
     ylab = "Frecuencia",
     col = "steelblue",
     border = "white")
#tasa euribor
datos$euribor3m <- as.numeric(datos$euribor3m)
exp(mean(log(datos$euribor3m)))
boxplot(datos$euribor3m,
        main = "Distribución de euribor3m",
        ylab = "euribor3m",
        col = "lightblue")
hist(datos$euribor3m,
     main = "Histograma de euribor3m",
     xlab = "euribor3m",
     col = "lightblue",
     border = "white")

summary(datos$euribor3m)


options(scipen = 999)

#tranformamos variables
datos <- datos %>% 
  mutate(suscribe = ifelse(y == "yes", 1, 0))
datos <- subset(datos, select = -y_z)
names(datos)[names(datos) == "y"] <- "suscribio"


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
library(ggplot2)
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
