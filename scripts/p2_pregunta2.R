############################################################
# Tarea 3 - Métodos Cuantitativos I
# Pregunta 2: Promedios y desviaciones estándar de FTE
# por grupo de tratamiento/control y periodo (pre / post)
# Archivo: scripts/p2_pregunta2.R
############################################################

# 0. Limpiar entorno de trabajo --------------------------------------------
rm(list = ls())

# 1. Cargar paquetes -------------------------------------------------------
if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
} else {
  library(tidyverse)
}

# 2. Cargar panel balanceado ----------------------------------------------
# Asumimos que el directorio de trabajo es la carpeta raíz del proyecto
# y que el archivo CK1994_panel_balanceado.csv está en la carpeta "data".

ruta_panel <- file.path("data", "CK1994_panel_balanceado.csv")

panel <- read_csv(
  file = ruta_panel,
  na = "."
)

# Aseguramos tipos numéricos para state, time y fte (por seguridad)
panel <- panel |>
  mutate(
    state = as.numeric(state),
    time  = as.numeric(time),
    fte   = as.numeric(fte)
  )

# 3. Definir grupos de tratamiento y control -------------------------------
# Tratamiento: Nueva Jersey (state = 1)
# Control: Pennsylvania (state = 0)
# time = 0: antes del aumento del salario mínimo
# time = 1: después del aumento del salario mínimo

panel <- panel |>
  mutate(
    treat  = if_else(state == 1, 1, 0),
    grupo  = if_else(treat == 1, "Tratamiento (NJ)", "Control (PA)"),
    periodo = if_else(time == 0, "Pre (time = 0)", "Post (time = 1)")
  )

# 4. Tabla de promedio y desviación estándar de FTE ------------------------
# Queremos la tabla con:
#   - Tratamiento, pre
#   - Tratamiento, post
#   - Control, pre
#   - Control, post

tabla_p2 <- panel |>
  group_by(grupo, periodo) |>
  summarise(
    media_fte = mean(fte, na.rm = TRUE),
    sd_fte    = sd(fte, na.rm = TRUE),
    n         = sum(!is.na(fte)),
    .groups   = "drop"
  )

# Ordenamos filas para que queden en el orden pedido
tabla_p2 <- tabla_p2 |>
  mutate(
    grupo = factor(grupo,
                   levels = c("Tratamiento (NJ)", "Control (PA)")),
    periodo = factor(periodo,
                     levels = c("Pre (time = 0)", "Post (time = 1)"))
  ) |>
  arrange(grupo, periodo)

# Redondeamos para presentación
tabla_p2 <- tabla_p2 |>
  mutate(
    media_fte = round(media_fte, 3),
    sd_fte    = round(sd_fte, 3)
  )

cat("Tabla Pregunta 2: Promedio y desviación estándar de FTE por grupo y periodo\n")
print(tabla_p2)
cat("\n")

# 5. Cálculo explícito de las cuatro celdas clave -------------------------

# Tratamiento (NJ)
mean_treat_pre  <- panel |> filter(treat == 1, time == 0) |> summarise(m = mean(fte)) |> pull(m)
sd_treat_pre    <- panel |> filter(treat == 1, time == 0) |> summarise(s = sd(fte)) |> pull(s)

mean_treat_post <- panel |> filter(treat == 1, time == 1) |> summarise(m = mean(fte)) |> pull(m)
sd_treat_post   <- panel |> filter(treat == 1, time == 1) |> summarise(s = sd(fte)) |> pull(s)

# Control (PA)
mean_ctrl_pre   <- panel |> filter(treat == 0, time == 0) |> summarise(m = mean(fte)) |> pull(m)
sd_ctrl_pre     <- panel |> filter(treat == 0, time == 0) |> summarise(s = sd(fte)) |> pull(s)

mean_ctrl_post  <- panel |> filter(treat == 0, time == 1) |> summarise(m = mean(fte)) |> pull(m)
sd_ctrl_post    <- panel |> filter(treat == 0, time == 1) |> summarise(s = sd(fte)) |> pull(s)

# Mostramos estos valores en consola (sin redondear y redondeados)
cat("Tratamiento (NJ), periodo pre:   media =", round(mean_treat_pre, 3),
    ", sd =", round(sd_treat_pre, 3), "\n")
cat("Tratamiento (NJ), periodo post:  media =", round(mean_treat_post, 3),
    ", sd =", round(sd_treat_post, 3), "\n")
cat("Control (PA), periodo pre:       media =", round(mean_ctrl_pre, 3),
    ", sd =", round(sd_ctrl_pre, 3), "\n")
cat("Control (PA), periodo post:      media =", round(mean_ctrl_post, 3),
    ", sd =", round(sd_ctrl_post, 3), "\n\n")

# 6. Cálculo del estimador DID descriptivo --------------------------------
# DID = (Cambio en NJ) - (Cambio en PA)
#     = (mean_treat_post - mean_treat_pre) - (mean_ctrl_post - mean_ctrl_pre)

did_est <- (mean_treat_post - mean_treat_pre) - (mean_ctrl_post - mean_ctrl_pre)

cat("Estimador DID descriptivo (en FTE):\n")
cat("  (NJ post - NJ pre) - (PA post - PA pre) =", round(did_est, 3), "\n\n")

# 7. Guardar tabla para usar en el informe --------------------------------

if (!dir.exists("output")) {
  dir.create("output")
}

write_csv(
  tabla_p2,
  file = file.path("output", "tabla_p2_tratamiento_control.csv")
)

############################################################
# Sugerencia de interpretación (texto para el informe)
#
# A partir de la tabla, se observa que el empleo full-time equivalente
# en Nueva Jersey (grupo de tratamiento) aumenta levemente entre el
# primer y el segundo periodo, mientras que en Pennsylvania (grupo de
# control) disminuye. El estimador descriptivo de diferencias en
# diferencias es positivo, del orden de 2.7 trabajadores equivalentes
# a tiempo completo por local. Esto sugiere, a nivel puramente
# descriptivo y sin controlar por otras variables, que el aumento del
# salario mínimo en Nueva Jersey no habría reducido el empleo promedio
# en estos restaurantes, sino que está asociado a un aumento relativo
# del empleo en comparación con Pennsylvania.
############################################################
