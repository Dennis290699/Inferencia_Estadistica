# Instalar y cargar paquetes necesarios
if (!require(readxl)) install.packages("readxl", dependencies=TRUE)
if (!require(dplyr)) install.packages("dplyr", dependencies=TRUE)
if (!require(ggplot2)) install.packages("ggplot2", dependencies=TRUE)

# Cargar librerías necesarias
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(broom)

# Modificar la ruta para la carga y lectura de los datos
enfermeria <- read_excel("C:/Users/User/Downloads/ehep - copia.xlsx", sheet = "POBLACION ENFERMERIA")
medicina <- read_excel("C:/Users/User/Downloads/ehep - copia.xlsx", sheet = "POBLACION MEDICINA")






# Pregunta 1: Resumen descriptivo y gráficos
cat("\nPregunta 1: Resumen descriptivo y gráficos\n")
set.seed(123)  # Para reproducibilidad

# Extraer muestras aleatorias de 200 observaciones
enfermeria_sample <- enfermeria %>% sample_n(200)
medicina_sample <- medicina %>% sample_n(200)

# Resumen descriptivo
get_summary <- function(data) {
  data %>%
    summarise(
      media = mean(CALIFICACION),
      mediana = median(CALIFICACION),
      varianza = var(CALIFICACION),
      desviacion_estandar = sd(CALIFICACION),
      minimo = min(CALIFICACION),
      maximo = max(CALIFICACION)
    )
}

enfermeria_summary <- get_summary(enfermeria_sample)
medicina_summary <- get_summary(medicina_sample)

# Imprimir resúmenes descriptivos
cat("Resumen descriptivo de Enfermería:\n")
print(enfermeria_summary)
cat("\nResumen descriptivo de Medicina:\n")
print(medicina_summary)

# Graficar histogramas superpuestos
ggplot() +
  geom_histogram(data=enfermeria_sample, aes(x=CALIFICACION), fill='blue', alpha=0.5, bins=20, position="identity") +
  geom_histogram(data=medicina_sample, aes(x=CALIFICACION), fill='red', alpha=0.5, bins=20, position="identity") +
  labs(title="Histograma de Calificaciones", x="Calificación", y="Frecuencia") +
  theme_minimal()
ggsave("histograma_calificaciones.png")

# Graficar boxplots
ggplot() +
  geom_boxplot(aes(y=CALIFICACION, x="Enfermería"), data=enfermeria_sample, fill='blue') +
  geom_boxplot(aes(y=CALIFICACION, x="Medicina"), data=medicina_sample, fill='red') +
  labs(title="Boxplot de Calificaciones", x="", y="Calificación") +
  theme_minimal()
ggsave("boxplot_calificaciones.png")






# Pregunta 2: Intervalos de confianza
cat("\nPregunta 2: Intervalos de confianza\n")

# Funciones para calcular intervalos de confianza
mean_ci <- function(data) t.test(data)$conf.int

var_ci <- function(data) {
  n <- length(data)
  variance <- var(data)
  alpha <- 0.05
  lower <- (n - 1) * variance / qchisq(1 - alpha / 2, df = n - 1)
  upper <- (n - 1) * variance / qchisq(alpha / 2, df = n - 1)
  c(lower, upper)
}

diff_mean_ci <- function(data1, data2) t.test(data1, data2)$conf.int

var_ratio_ci <- function(data1, data2) {
  n1 <- length(data1)
  n2 <- length(data2)
  var1 <- var(data1)
  var2 <- var(data2)
  alpha <- 0.05
  lower <- (var1 / var2) / qf(1 - alpha / 2, df1 = n1 - 1, df2 = n2 - 1)
  upper <- (var1 / var2) / qf(alpha / 2, df1 = n1 - 1, df2 = n2 - 1)
  c(lower, upper)
}

prop_ci <- function(successes, n) {
  prop <- successes / n
  error <- qnorm(0.975) * sqrt((prop * (1 - prop)) / n)
  c(prop - error, prop + error)
}

diff_prop_ci <- function(successes1, n1, successes2, n2) {
  prop1 <- successes1 / n1
  prop2 <- successes2 / n2
  error <- qnorm(0.975) * sqrt((prop1 * (1 - prop1) / n1) + (prop2 * (1 - prop2) / n2))
  diff <- prop1 - prop2
  c(diff - error, diff + error)
}

# Suponiendo que las proporciones son de aprobados (calificación >= 70)
enfermeria_successes <- sum(enfermeria$CALIFICACION >= 70)
medicina_successes <- sum(medicina$CALIFICACION >= 70)

# Calcular intervalos de confianza
medicina_mean_ci <- mean_ci(medicina$CALIFICACION)
enfermeria_mean_ci <- mean_ci(enfermeria$CALIFICACION)

medicina_var_ci <- var_ci(medicina$CALIFICACION)
enfermeria_var_ci <- var_ci(enfermeria$CALIFICACION)

diff_mean_ci <- diff_mean_ci(medicina$CALIFICACION, enfermeria$CALIFICACION)
var_ratio_ci <- var_ratio_ci(medicina$CALIFICACION, enfermeria$CALIFICACION)

medicina_prop_ci <- prop_ci(medicina_successes, nrow(medicina))
enfermeria_prop_ci <- prop_ci(enfermeria_successes, nrow(enfermeria))

diff_prop_ci <- diff_prop_ci(medicina_successes, nrow(medicina), enfermeria_successes, nrow(enfermeria))

# Imprimir resultados
cat("\nIntervalos de confianza para la media de Medicina:", medicina_mean_ci, "\n")
cat("Intervalos de confianza para la media de Enfermería:", enfermeria_mean_ci, "\n")
cat("\nIntervalos de confianza para la varianza de Medicina:", medicina_var_ci, "\n")
cat("Intervalos de confianza para la varianza de Enfermería:", enfermeria_var_ci, "\n")
cat("\nIntervalo de confianza para la diferencia de medias:", diff_mean_ci, "\n")
cat("\nIntervalo de confianza para la razón entre varianzas:", var_ratio_ci, "\n")
cat("\nIntervalos de confianza para la proporción de aprobados en Medicina:", medicina_prop_ci, "\n")
cat("Intervalos de confianza para la proporción de aprobados en Enfermería:", enfermeria_prop_ci, "\n")
cat("\nIntervalo de confianza para la diferencia de proporciones de aprobados:", diff_prop_ci, "\n")






# Pregunta 3: Pruebas de hipótesis
cat("\nPregunta 3: Pruebas de hipótesis\n")

# Pruebas de hipótesis
t_test_media_78 <- t.test(medicina$CALIFICACION, mu = 78)
t_test_media_80 <- t.test(medicina$CALIFICACION, mu = 80, alternative = "greater")
t_test_diferencia_medias <- t.test(medicina$CALIFICACION, enfermeria$CALIFICACION, alternative = "less")
var_test <- var.test(medicina$CALIFICACION, enfermeria$CALIFICACION)
prop_test_med <- prop.test(medicina_successes, nrow(medicina), p = 0.6, alternative = "greater")


# Imprimir resultados
cat("\nPrueba de hipótesis para la media de Medicina = 78:\n")
print(t_test_media_78)

cat("\nPrueba de hipótesis para la media de Medicina >= 80:\n")
print(t_test_media_80)

cat("\nPrueba de hipótesis para la diferencia de medias entre Medicina y Enfermería:\n")
print(t_test_diferencia_medias)

cat("\nPrueba de hipótesis para la diferencia de varianzas entre Medicina y Enfermería:\n")
print(var_test)

cat("\nPrueba de hipótesis para la proporción de aprobados en Medicina:\n")
print(prop_test_med)








# Pregunta 4: Prueba de Kolmogorov-Smirnov y prueba de bondad de ajuste
cat("\nPregunta 4: Prueba de Kolmogorov-Smirnov y prueba de bondad de ajuste\n")

# Parámetros para la distribución normal
media_enf <- 69.69
desviacion_estandar_enf <- sqrt(215.15)

media_med <- 77.98
desviacion_estandar_med <- sqrt(258.89)

# Categorización de calificaciones
categorias <- seq(0, 120, by = 20)

enfermeria$categoria <- cut(enfermeria$CALIFICACION,
                            breaks = categorias,
                            labels = c("0-20", "20-40", "40-60", "60-80", "80-100", "100-120"),
                            right = FALSE)

medicina$categoria <- cut(medicina$CALIFICACION,
                          breaks = categorias,
                          labels = c("0-20", "20-40", "40-60", "60-80", "80-100", "100-120"),
                          right = FALSE)

# Función para calcular frecuencias esperadas bajo distribución normal
calc_freq_esp <- function(media, desviacion_estandar, categorias) {
  sapply(1:(length(categorias) - 1), function(i) {
    p1 <- pnorm(categorias[i + 1], mean = media, sd = desviacion_estandar)
    p2 <- pnorm(categorias[i], mean = media, sd = desviacion_estandar)
    p1 - p2
  })
}

# Prueba de Kolmogorov-Smirnov
ks_enfermeria <- ks.test(enfermeria$CALIFICACION, "pnorm", mean = media_enf, sd = desviacion_estandar_enf)
ks_medicina <- ks.test(medicina$CALIFICACION, "pnorm", mean = media_med, sd = desviacion_estandar_med)

# Imprimir resultados de Kolmogorov-Smirnov
cat("Prueba de Kolmogorov-Smirnov para enfermería:\n")
print(ks_enfermeria)
cat("\n")

cat("Prueba de Kolmogorov-Smirnov para medicina:\n")
print(ks_medicina)
cat("\n")






# Pregunta 5: Regresión con variable dependiente adicional
cat("\nPregunta 5: Regresión con variable dependiente adicional\n")

# Calcular frecuencias esperadas y pruebas de bondad de ajuste
freq_esp_enf <- calc_freq_esp(media_enf, desviacion_estandar_enf, categorias) * nrow(enfermeria)
freq_esp_med <- calc_freq_esp(media_med, desviacion_estandar_med, categorias) * nrow(medicina)

# Prueba de bondad de ajuste
freq_obs_enf <- table(enfermeria$categoria)
freq_obs_med <- table(medicina$categoria)

test_enf <- chisq.test(freq_obs_enf, p = freq_esp_enf / sum(freq_esp_enf), rescale.p = TRUE)
test_med <- chisq.test(freq_obs_med, p = freq_esp_med / sum(freq_esp_med), rescale.p = TRUE)

# Imprimir resultados de bondad de ajuste
cat("Prueba de bondad de ajuste para enfermería:\n")
print(test_enf)
cat("\n")

cat("Prueba de bondad de ajuste para medicina:\n")
print(test_med)
cat("\n")
