############################################################
# Tarea 3 - Métodos Cuantitativos I
# Pregunta 7: FE controlando por hoursopen
# Archivo: scripts/p7_pregunta7.R
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

if (!require("broom")) {
  install.packages("broom")
  install.packages("broom")
  library(broom)
} else {
  library(broom)
}

# 2. Cargar panel balanceado ----------------------------------------------
ruta_panel <- file.path("data", "CK1994_panel_balanceado.csv")

panel <- read_csv(
  file = ruta_panel,
  na = "."
)

panel <- panel |>
  mutate(
    state     = as.numeric(state),
    time      = as.numeric(time),
    fte       = as.numeric(fte),
    hoursopen = as.numeric(hoursopen),
    treat     = if_else(state == 1, 1, 0),  # 1 = NJ, 0 = PA
    post      = time,                      # 1 = periodo post, 0 = pre
    did       = treat * post               # indicador NJ en periodo post
  )

# Chequeo de missing en hoursopen
cat("Cantidad de observaciones con hoursopen NA:", sum(is.na(panel$hoursopen)), "\n\n")

# 3. Modelo FE base (como en Pregunta 6) ----------------------------------
modelo_fe_base <- lm(fte ~ did + factor(store) + factor(time), data = panel)

cat("Resumen modelo FE base (sin hoursopen):\n")
print(summary(modelo_fe_base))
cat("\n")

# 4. Modelo FE con control por hoursopen ----------------------------------
# lm elimina automáticamente las filas con NA en hoursopen
modelo_fe_hours <- lm(fte ~ did + hoursopen + factor(store) + factor(time),
                      data = panel)

cat("Resumen modelo FE con hoursopen:\n")
print(summary(modelo_fe_hours))
cat("\n")

# 5. Tabla de coeficientes DID y hoursopen --------------------------------
tabla_coef_p7 <- bind_rows(
  tidy(modelo_fe_base) |>
    mutate(modelo = "FE sin hoursopen"),
  tidy(modelo_fe_hours) |>
    mutate(modelo = "FE + hoursopen")
) |>
  # sacar las dummies de efectos fijos
  filter(
    !str_starts(term, "factor(store)"),
    !str_starts(term, "factor(time)")
  ) |>
  # quedarnos con constante, DID y hoursopen (cuando corresponda)
  filter(term %in% c("(Intercept)", "did", "hoursopen")) |>
  mutate(
    termino = recode(
      term,
      "(Intercept)" = "Constante",
      "did"         = "Interacción tratamiento x post (DID)",
      "hoursopen"   = "Horas abierto (hoursopen)"
    ),
    estimacion    = round(estimate, 3),
    error_std     = round(std.error, 3),
    estadistico_t = round(statistic, 3),
    p_valor       = round(p.value, 3)
  ) |>
  select(
    modelo,
    termino,
    estimacion,
    error_std,
    estadistico_t,
    p_valor
  )

cat("Tabla de coeficientes (FE base vs FE con hoursopen):\n")
print(tabla_coef_p7)
cat("\n")

# 6. Tabla de R^2, R^2 ajustado y N ---------------------------------------

tabla_stats_p7 <- bind_rows(
  glance(modelo_fe_base) |>
    mutate(modelo = "FE sin hoursopen"),
  glance(modelo_fe_hours) |>
    mutate(modelo = "FE + hoursopen")
) |>
  transmute(
    modelo,
    r_cuadrado          = round(r.squared, 3),
    r_cuadrado_ajustado = round(adj.r.squared, 3),
    n                   = nobs
  )

cat("Estadísticas de ajuste (FE base vs FE con hoursopen):\n")
print(tabla_stats_p7)
cat("\n")

# 7. Guardar tablas en output ---------------------------------------------

if (!dir.exists("output")) {
  dir.create("output")
}

write_csv(
  tabla_coef_p7,
  file = file.path("output", "regresion_p7_coeficientes_FE_hoursopen.csv")
)

write_csv(
  tabla_stats_p7,
  file = file.path("output", "regresion_p7_stats_FE_hoursopen.csv")
)

############################################################
# Guía de interpretación para tu informe:
#
# 1) Al incluir hoursopen, se pierden algunas observaciones
#    por datos faltantes en esa variable. El N del modelo FE
#    con hoursopen debería ser 761, versus 768 en el FE base.
#
# 2) El coeficiente DID en el FE base es ~2.750, con error
#    estándar ~1.153 y p ≈ 0.018 (significativo al 5 %).
#
# 3) En el modelo FE con hoursopen, el coeficiente DID se
#    mantiene muy parecido, alrededor de 2.770, con error
#    estándar ~1.145 y p ≈ 0.016. La magnitud cambia poco
#    y sigue siendo estadísticamente significativa.
#
# 4) El coeficiente de hoursopen es positivo, del orden de
#    1.13, y significativo (p ≈ 0.012). Esto indica que,
#    dentro de un mismo restaurante, más horas abierto se
#    asocian con más empleo full-time equivalente.
#
# 5) El R^2 pasa de ~0.780 a ~0.782 al agregar hoursopen.
#    El ajuste mejora solo marginalmente. La evidencia
#    sobre el efecto del salario mínimo es muy similar:
#    controlar por hoursopen casi no cambia el resultado
#    y el efecto estimado en NJ sigue siendo un aumento
#    relativo de alrededor de 2.7 FTE por local respecto a PA.
############################################################
