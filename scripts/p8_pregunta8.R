############################################################
# Tarea 3 - Métodos Cuantitativos I
# Pregunta 8: DID con efectos fijos y efectos heterogéneos
# por región de Nueva Jersey (norte, centro, sur)
# Archivo: scripts/p8_pregunta8.R
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

# Para usar linearHypothesis sin pisar recode, usamos requireNamespace
if (!requireNamespace("car", quietly = TRUE)) {
  install.packages("car")
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
    southj    = as.numeric(southj),
    centralj  = as.numeric(centralj),
    northj    = as.numeric(northj),
    post      = time,
    treat     = if_else(state == 1, 1, 0),
    did       = treat * post,
    did_north   = northj   * post,
    did_central = centralj * post,
    did_south   = southj   * post
  )

# 3. Chequeo rápido de distribución de regiones (solo NJ) -----------------
cat("Distribución de restaurantes de NJ por región (norte/centro/sur):\n")

dist_regiones <- panel |>
  filter(state == 1) |>
  group_by(store) |>
  summarise(
    southj   = first(southj),
    centralj = first(centralj),
    northj   = first(northj),
    .groups  = "drop"
  ) |>
  count(southj, centralj, northj)

print(dist_regiones)
cat("\n")

# 4. Modelo FE base de la Pregunta 6 (un solo DID) ------------------------
modelo_fe_base <- lm(
  fte ~ did + factor(store) + factor(time),
  data = panel
)

cat("Resumen modelo FE base (DID único):\n")
print(summary(modelo_fe_base))
cat("\n")

# 5. Modelo FE con efectos heterogéneos por región ------------------------
# fte_it = alpha_i + lambda_t +
#          beta_N * did_north_it +
#          beta_C * did_central_it +
#          beta_S * did_south_it + u_it

modelo_fe_regiones <- lm(
  fte ~ did_north + did_central + did_south +
    factor(store) + factor(time),
  data = panel
)

cat("Resumen modelo FE con DID por región (norte/centro/sur):\n")
print(summary(modelo_fe_regiones))
cat("\n")

# 6. Tabla de coeficientes (DID único vs DID por región) ------------------

tabla_coef_p8 <- bind_rows(
  tidy(modelo_fe_base) |>
    mutate(modelo = "FE DID único"),
  tidy(modelo_fe_regiones) |>
    mutate(modelo = "FE DID por región")
) |>
  # quitar dummies de efectos fijos
  filter(
    !str_starts(term, "factor(store)"),
    !str_starts(term, "factor(time)")
  ) |>
  # términos de interés
  filter(term %in% c("(Intercept)", "did",
                     "did_north", "did_central", "did_south")) |>
  mutate(
    termino = dplyr::recode(
      term,
      "(Intercept)" = "Constante",
      "did"         = "Interacción tratamiento x post (DID total NJ)",
      "did_north"   = "DID región norte (NJ vs PA)",
      "did_central" = "DID región centro (NJ vs PA)",
      "did_south"   = "DID región sur (NJ vs PA)"
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

cat("Tabla de coeficientes (FE DID único vs FE DID por región):\n")
print(tabla_coef_p8)
cat("\n")

# 7. Test de igualdad de efectos entre regiones ---------------------------
# H0: beta_north = beta_central = beta_south
# Implementado como:
#   did_north = did_central
#   did_north = did_south

test_igualdad <- car::linearHypothesis(
  modelo_fe_regiones,
  c("did_north = did_central", "did_north = did_south")
)

cat("Test de igualdad de efectos entre regiones (H0: norte = centro = sur):\n")
print(test_igualdad)
cat("\n")

# Extraer estadísticos del test
F_stat <- test_igualdad$F[2]
p_val  <- test_igualdad[["Pr(>F)"]][2]
df_num <- test_igualdad$Df[2]
df_den <- test_igualdad$Res.Df[2]

tabla_test_p8 <- tibble(
  restriccion   = "H0: DID norte = DID centro = DID sur",
  estadistico_F = round(F_stat, 3),
  gl_num        = df_num,
  gl_den        = df_den,
  p_valor       = round(p_val, 3)
)

cat("Resumen numérico del test de igualdad de efectos:\n")
print(tabla_test_p8)
cat("\n")

# 8. Guardar tablas en output ---------------------------------------------

if (!dir.exists("output")) {
  dir.create("output")
}

write_csv(
  tabla_coef_p8,
  file = file.path("output", "regresion_p8_coeficientes_FE_regiones.csv")
)

write_csv(
  tabla_test_p8,
  file = file.path("output", "regresion_p8_test_igualdad_regiones.csv")
)

############################################################
# Guía de interpretación para tu informe (no se ejecuta):
#
# 1) Los coeficientes DID por región (modelo FE DID por región)
#    deberían salir aproximadamente:
#      - DID región norte (NJ vs PA)  ~ 3.02  (se ~ 1.25, p ~ 0.016)
#      - DID región centro (NJ vs PA) ~ 1.64  (se ~ 1.57, p ~ 0.295)
#      - DID región sur (NJ vs PA)    ~ 2.98  (se ~ 1.41, p ~ 0.035)
#
# 2) Comparados con el DID único (~2.75) del modelo FE,
#    los efectos por región están en la misma magnitud general.
#
# 3) El test conjunto H0: norte = centro = sur da un estadístico F
#    cercano a 0.54 con p ~ 0.58. Esto indica que no hay evidencia
#    estadística fuerte de que el efecto del aumento del salario
#    mínimo difiera entre norte, centro y sur de Nueva Jersey.
############################################################
