library(ggplot2)
library(gridExtra)

# Establecer semilla para reproducibilidad
set.seed(123)

# Generar una poblacion de 1000 datos con media 25 y desviacion estandar 5
population <- rnorm(1000, mean = 25, sd = 5)

# Función para calcular intervalos de confianza
calc_ci <- function(sample, conf_level) {
  mean_sample <- mean(sample)
  sd_sample <- sd(sample)
  error_margin <- qnorm((1 + conf_level) / 2) * (sd_sample / sqrt(length(sample)))
  c(mean_sample - error_margin, mean_sample + error_margin)
}

# Generar 100 muestras aleatorias y calcular intervalos de confianza
generate_ci_data <- function(conf_level) {
  sample_size <- 30
  num_samples <- 100
  samples <- replicate(num_samples, sample(population, sample_size, replace = TRUE))
  cis <- apply(samples, 2, calc_ci, conf_level)
  ci_data <- data.frame(
    Muestra = 1:num_samples,
    Inferior = cis[1, ],
    Superior = cis[2, ],
    Media = colMeans(samples)
  )
  ci_data
}

# Generar datos para tres niveles de confianza
ci_data_68 <- generate_ci_data(0.68)
ci_data_95 <- generate_ci_data(0.95)
ci_data_997 <- generate_ci_data(0.997)

# Función para crear la grafica
plot_ci <- function(ci_data, conf_level) {
  ggplot(ci_data, aes(x = Muestra, y = Media)) +
    geom_errorbar(aes(ymin = Inferior, ymax = Superior), width = 0.2, color = "royalblue") +
    geom_hline(yintercept = 25, linetype = "dashed") +  # Media de la poblacion
    labs(title = paste("Intervalos de Confianza (", conf_level * 100, "%)", sep = ""),
         x = "Muestra",
         y = "Edad Media") +
    theme_minimal() +
    coord_flip()
}

# Crear graficas
plot_68 <- plot_ci(ci_data_68, 0.68)
plot_95 <- plot_ci(ci_data_95, 0.95)
plot_997 <- plot_ci(ci_data_997, 0.997)

# Combinar graficas
grid.arrange(plot_68, plot_95, plot_997, nrow = 1)
