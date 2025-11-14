############################################################
# Tarea 3 - Métodos Cuantitativos I
# Pregunta 1: Importar datos, crear fte y estadística descriptiva
# Archivo: scripts/p1_pregunta1.R
############################################################

# 0. Limpiar entorno de trabajo --------------------------------------------
rm(list = ls())

# 1. Cargar paquetes necesarios -------------------------------------------
# Usaremos el "tidyverse" (incluye dplyr y readr).
# La primera vez que corras este script, R instalará el paquete si no lo tienes.

if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
} else {
  library(tidyverse)
}

# 2. Definir rutas básicas -------------------------------------------------
# Asumimos que estás parado en la carpeta raíz del proyecto "tarea1_metodos"
# y que dentro existen las carpetas "data" y "scripts".

ruta_datos <- file.path("data", "CK1994.csv")

# Carpeta donde vamos a guardar tablas de salida
if (!dir.exists("output")) {
  dir.create("output")
}

# 3. Importar base CK1994 --------------------------------------------------
# El diccionario de datos indica que los valores "." representan datos perdidos,
# así que los declaramos como NA.

ck <- read_csv(
  file = ruta_datos,
  na = "."
)

# Mirar dimensiones de la base (deberían ser 820 x 26)
dim(ck)

# 4. Asegurar que las variables clave sean numéricas -----------------------
# En el CSV algunas columnas vienen como texto. Convertimos las que necesitamos.

ck <- ck |>
  mutate(
    empft = as.numeric(empft),   # trabajadores full-time
    emppt = as.numeric(emppt),   # trabajadores part-time
    nmgrs = as.numeric(nmgrs),   # managers + asistentes
    state = as.numeric(state),   # 1 = New Jersey, 0 = Pennsylvania
    time  = as.numeric(time)     # 0 = primera encuesta, 1 = segunda
  )

# 5. Crear variable fte (full-time equivalente) ----------------------------
# fte = empft + emppt/2 + nmgrs

ck <- ck |>
  mutate(
    fte = empft + emppt / 2 + nmgrs
  )

# 6. Cantidad de observaciones totales y por estado/periodo ----------------

# Número total de observaciones en la muestra original
total_obs <- nrow(ck)
cat("Número total de observaciones en la muestra original:", total_obs, "\n\n")

# Distribución por estado
# Recordatorio: state = 1 (Nueva Jersey), 0 (Pennsylvania)
tabla_state <- ck |>
  mutate(
    estado = if_else(state == 1, "Nueva Jersey", "Pennsylvania")
  ) |>
  count(estado)

cat("Número de observaciones por estado:\n")
print(tabla_state)
cat("\n")

# Distribución por periodo de tiempo
# time = 0 (primer periodo, antes del aumento), 1 (segundo periodo, después)
tabla_time <- ck |>
  mutate(
    periodo = if_else(time == 1, "Segundo periodo (post)", "Primer periodo (pre)")
  ) |>
  count(periodo)

cat("Número de observaciones por periodo de tiempo:\n")
print(tabla_time)
cat("\n")

# 7. Construir panel balanceado -------------------------------------------
# Queremos quedarnos solo con los restaurantes que:
#   (a) aparecen en ambos periodos (time = 0 y time = 1)
#   (b) tienen fte observado (no NA) en ambos periodos.

panel_balanceado <- ck |>
  # Primero, eliminar filas donde fte es NA
  filter(!is.na(fte)) |>
  group_by(store) |>
  # Nos quedamos solo con stores que tienen observaciones en los dos periodos
  filter(n_distinct(time) == 2) |>
  ungroup()

# Comprobaciones útiles
n_obs_balanceado   <- nrow(panel_balanceado)
n_stores_balanceado <- n_distinct(panel_balanceado$store)

cat("Número de observaciones en el panel balanceado:", n_obs_balanceado, "\n")
cat("Número de restaurantes en el panel balanceado:", n_stores_balanceado, "\n\n")

# 8. Tabla de estadística descriptiva para fte, state y time ---------------
# Usamos el panel balanceado.

# Calculamos media, desviación estándar, mínimo, máximo y número de observaciones
tabla_descriptiva <- tibble(
  variable = c("fte", "state", "time"),
  media = c(
    mean(panel_balanceado$fte,   na.rm = TRUE),
    mean(panel_balanceado$state, na.rm = TRUE),
    mean(panel_balanceado$time,  na.rm = TRUE)
  ),
  desviacion_estandar = c(
    sd(panel_balanceado$fte,   na.rm = TRUE),
    sd(panel_balanceado$state, na.rm = TRUE),
    sd(panel_balanceado$time,  na.rm = TRUE)
  ),
  minimo = c(
    min(panel_balanceado$fte,   na.rm = TRUE),
    min(panel_balanceado$state, na.rm = TRUE),
    min(panel_balanceado$time,  na.rm = TRUE)
  ),
  maximo = c(
    max(panel_balanceado$fte,   na.rm = TRUE),
    max(panel_balanceado$state, na.rm = TRUE),
    max(panel_balanceado$time,  na.rm = TRUE)
  ),
  n = c(
    sum(!is.na(panel_balanceado$fte)),
    sum(!is.na(panel_balanceado$state)),
    sum(!is.na(panel_balanceado$time))
  )
)

# Redondeamos para que la tabla sea más limpia
tabla_descriptiva <- tabla_descriptiva |>
  mutate(
    media               = round(media, 3),
    desviacion_estandar = round(desviacion_estandar, 3),
    minimo              = round(minimo, 3),
    maximo              = round(maximo, 3)
  )

cat("Tabla de estadística descriptiva (panel balanceado):\n")
print(tabla_descriptiva)

# 9. Guardar resultados para usar en el informe y en las siguientes preguntas ----

# Guardar panel balanceado para reutilizarlo en otros scripts (preguntas 2–12)
write_csv(
  panel_balanceado,
  file = file.path("data", "CK1994_panel_balanceado.csv")
)

# Guardar tabla descriptiva en CSV para poder copiarla con formato al informe
write_csv(
  tabla_descriptiva,
  file = file.path("output", "tabla_descriptiva_p1.csv")
)

# Fin del script de la Pregunta 1
############################################################
