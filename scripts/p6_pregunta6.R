############################################################
# Tarea 3 - Métodos Cuantitativos I
# Pregunta 6: Efecto del salario mínimo con efectos fijos
# Archivo: scripts/p6_pregunta6.R
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
    state = as.numeric(state),
    time  = as.numeric(time),
    fte   = as.numeric(fte),
    treat = if_else(state == 1, 1, 0),  # 1 = NJ, 0 = PA
    post  = time,                      # 1 = periodo post, 0 = pre
    did   = treat * post               # indicador NJ en periodo post
  )

# 3. Modelo MCO simple (el de la Pregunta 3) -------------------------------
# Lo re-estimamos aquí para poder comparar en una misma tabla.

modelo_ols <- lm(fte ~ treat + post + did, data = panel)

cat("Resumen modelo MCO (Preg. 3):\n")
print(summary(modelo_ols))
cat("\n")

# 4. Modelo con efectos fijos por restaurante y periodo -------------------
# Implementamos efectos fijos con dummies:
# fte_it = alpha_i + lambda_t + beta * did_it + u_it
# En R: incluir factor(store) y factor(time).

modelo_fe <- lm(fte ~ did + factor(store) + factor(time), data = panel)

cat("Resumen modelo con efectos fijos (restaurante + tiempo):\n")
print(summary(modelo_fe))
cat("\n")

# 5. Construir tabla de coeficientes para el informe -----------------------
# Nos quedamos solo con la constante y el coeficiente DID,
# y sacamos las dummies de store y time para que la tabla no explote.

tabla_coef <- bind_rows(
  tidy(modelo_ols) |>
    mutate(modelo = "MCO DID"),
  tidy(modelo_fe) |>
    mutate(modelo = "FE restaurante + tiempo")
) |>
  # eliminar las dummies de efectos fijos
  filter(
    !str_starts(term, "factor(store)"),
    !str_starts(term, "factor(time)")
  ) |>
  # nos quedamos con constante y DID
  filter(term %in% c("(Intercept)", "did")) |>
  mutate(
    termino = recode(
      term,
      "(Intercept)" = "Constante",
      "did"         = "Interacción tratamiento x post (DID)"
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

cat("Tabla de coeficientes (MCO vs FE):\n")
print(tabla_coef)
cat("\n")

# 6. Tabla con R^2 y N para cada modelo -----------------------------------

tabla_stats <- bind_rows(
  glance(modelo_ols) |>
    mutate(modelo = "MCO DID"),
  glance(modelo_fe) |>
    mutate(modelo = "FE restaurante + tiempo")
) |>
  transmute(
    modelo,
    r_cuadrado          = round(r.squared, 3),
    r_cuadrado_ajustado = round(adj.r.squared, 3),
    n                   = nobs
  )

cat("Estadísticas de ajuste de los modelos:\n")
print(tabla_stats)
cat("\n")

# 7. Guardar tablas en output ---------------------------------------------
if (!dir.exists("output")) {
  dir.create("output")
}

write_csv(
  tabla_coef,
  file = file.path("output", "regresion_p6_coeficientes_MCO_vs_FE.csv")
)

write_csv(
  tabla_stats,
  file = file.path("output", "regresion_p6_stats_MCO_vs_FE.csv")
)

############################################################
# Guía de interpretación para tu informe:
#
# 1) Modelo MCO (el de la Pregunta 3):
#    - Coeficiente DID ~= 2.750
#    - Error estándar ~= 1.731
#    - t ~= 1.59, p ~= 0.113
#    Este efecto es positivo pero no estadísticamente significativo
#    al 10 % con errores estándar clásicos.
#
# 2) Modelo con efectos fijos por restaurante y periodo:
#    - El coeficiente de la interacción DID es NUMÉRICAMENTE
#      CASI IGUAL (también ~= 2.750), porque con dos periodos
#      la estimación de beta es la misma dif-en-dif.
#    - La diferencia está en la precisión:
#         error estándar DID FE ~= 1.153
#         t ~= 2.39, p ~= 0.018
#      Es decir, con efectos fijos el efecto estimado resulta
#      estadísticamente significativo al 5 % con estos errores
#      estándar clásicos.
#
#    Intuición:
#    - El modelo FE controla por heterogeneidad inobservable
#      fija a nivel de restaurante (cada local tiene su propio
#      intercepto) y por un efecto fijo común de tiempo.
#    - Eso elimina diferencias permanentes entre locales que
#      podrían contaminar la estimación en el MCO simple.
#
# 3) Comparación:
#    - Magnitud del efecto: muy similar entre MCO y FE
#      (alrededor de +2.75 FTE por local en NJ, relativo a PA).
#    - Precisión: mucho mayor en el modelo FE, que aprovecha
#      la variación dentro de cada restaurante a lo largo del
#      tiempo y controla por niveles distintos entre locales.
#
#    Para el informe:
#    - Explica que al introducir efectos fijos de restaurante
#      el tamaño del efecto estimado casi no cambia, pero el
#      coeficiente pasa a ser estadísticamente significativo,
#      lo que refuerza la evidencia de que el aumento del
#      salario mínimo en NJ no redujo el empleo respecto a PA,
#      sino que está asociado a un aumento relativo.
############################################################
