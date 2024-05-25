# Funciones para intervalos de confianza

# Función para intervalo de confianza de la media con varianza conocida
ic_media_varianza_conocida <- function(mu_muestral, sigma_poblacional, n, nivel_confianza) {
  # Nivel de confianza en proporción
  nivel_confianza_prop <- 1 - nivel_confianza / 2
  
  # Cálculo del error estándar
  error_estandar <- sigma_poblacional / sqrt(n)
  
  # Límites del intervalo de confianza
  limite_inferior <- mu_muestral - error_estandar * qnorm(nivel_confianza_prop)
  limite_superior <- mu_muestral + error_estandar * qnorm(nivel_confianza_prop)
  
  # Retornar el intervalo de confianza
  return(c(limite_inferior, limite_superior))
}

# Función para intervalo de confianza de la media con datos
ic_media_datos <- function(datos, nivel_confianza) {
  # Calcular la media muestral y la varianza muestral
  mu_muestral <- mean(datos)
  sigma_muestral <- sqrt(var(datos) / length(datos))
  
  # Calcular el error estándar
  error_estandar <- sigma_muestral / sqrt(length(datos))
  
  # Límites del intervalo de confianza
  limite_inferior <- mu_muestral - error_estandar * qnorm(1 - nivel_confianza / 2)
  limite_superior <- mu_muestral + error_estandar * qnorm(1 - nivel_confianza / 2)
  
  # Retornar el intervalo de confianza
  return(c(limite_inferior, limite_superior))
}

# Función para graficar el intervalo de confianza
graficar_ic_media <- function(datos, nivel_confianza) {
  # Obtener el intervalo de confianza
  ic <- ic_media_datos(datos, nivel_confianza)
  
  # Calcular la densidad de probabilidad normal
  x <- seq(min(datos), max(datos), length = 100)
  densidad <- dnorm(x, mean(datos), sd(datos))
  
  # Gráfico de la distribución de probabilidad
  plot(x, densidad, type = "l", main = "Distribución de probabilidad", xlab = "Valores", ylab = "Densidad de probabilidad")
  
  # Agregar el intervalo de confianza
  abline(v = ic[1], col = "red", lty = 2)
  abline(v = ic[2], col = "red", lty = 2)
}

# Función para intervalo de confianza de la proporción
ic_proporcion <- function(x_exitos, n, nivel_confianza) {
  # Proporción muestral
  p_muestral <- mean(x_exitos)
  
  # Error estándar
  error_estandar <- sqrt(p_muestral * (1 - p_muestral) / n)
  
  # Límites del intervalo de confianza
  limite_inferior <- p_muestral - error_estandar * qnorm(1 - nivel_confianza / 2)
  limite_superior <- p_muestral + error_estandar * qnorm(1 - nivel_confianza / 2)
  
  # Retornar el intervalo de confianza
  return(c(limite_inferior, limite_superior))
}

# Función para intervalo de confianza de la varianza
ic_varianza <- function(sigma_muestral, n, nivel_confianza) {
  # Grados de libertad
  gl <- n - 1
  
  # Límites del intervalo de confianza
  limite_inferior <- (n - 1) * sigma_muestral^2 / qchisq(1 - nivel_confianza / 2, gl)
  limite_superior <- (n - 1) * sigma_muestral^2 / qchisq(nivel_confianza / 2, gl)
  
  # Retornar el intervalo de confianza
  return(c(limite_inferior, limite_superior))
}

# Test con datos
# a) Intervalo de confianza para la media poblacional (con varianza conocida)
# Generar datos aleatorios
datos <- rnorm(100, mean = 50, sd = 10)

# Nivel de confianza
nivel_confianza <- 0.95

# Calcular el intervalo de confianza
ic <- ic_media_varianza_conocida(mean(datos), sd(datos), length(datos), nivel_confianza)

# Mostrar el intervalo de confianza
cat("Intervalo de confianza para la media poblacional (con varianza conocida):\n", ic, "\n\n")

# b) Intervalo de confianza para la media poblacional (con datos)
# Generar datos aleatorios
datos <- rnorm(100)

# Calcular el intervalo de confianza
ic <- ic_media_datos(datos, nivel_confianza)

# Mostrar el intervalo de confianza
cat("Intervalo de confianza para la media poblacional (con datos):\n", ic, "\n\n")

# c) Graficar intervalo de confianza
graficar_ic_media(datos, nivel_confianza)

# d) Intervalo de confianza para la proporción
# Generar datos aleatorios (éxitos/total)
x_exitos <- rbinom(100, size = 1, prob = 0.5)

# Calcular el intervalo de confianza
ic <- ic_proporcion(x_exitos, length(x_exitos), nivel_confianza)

# Mostrar el intervalo de confianza
cat("Intervalo de confianza para la proporción:\n", ic, "\n\n")

# e) Intervalo de confianza para la varianza poblacional
# Generar datos aleatorios
datos <- rnorm(100)

# Calcular el intervalo de confianza
ic <- ic_varianza(sd(datos), length(datos), nivel_confianza)

# Mostrar el intervalo de confianza
cat("Intervalo de confianza para la varianza poblacional:\n", ic, "\n")
