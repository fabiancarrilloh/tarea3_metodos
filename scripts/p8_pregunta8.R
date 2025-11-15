rm(list = ls())

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

if (!requireNamespace("car", quietly = TRUE)) {
  install.packages("car")
}

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

modelo_fe_base <- lm(
  fte ~ did + factor(store) + factor(time),
  data = panel
)

cat("Resumen modelo FE base (DID único):\n")
print(summary(modelo_fe_base))
cat("\n")

modelo_fe_regiones <- lm(
  fte ~ did_north + did_central + did_south +
    factor(store) + factor(time),
  data = panel
)

cat("Resumen modelo FE con DID por región (norte/centro/sur):\n")
print(summary(modelo_fe_regiones))
cat("\n")

tabla_coef_p8 <- bind_rows(
  tidy(modelo_fe_base) |>
    mutate(modelo = "FE DID único"),
  tidy(modelo_fe_regiones) |>
    mutate(modelo = "FE DID por región")
) |>
  filter(
    !str_starts(term, "factor(store)"),
    !str_starts(term, "factor(time)")
  ) |>
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

test_igualdad <- car::linearHypothesis(
  modelo_fe_regiones,
  c("did_north = did_central", "did_north = did_south")
)

cat("Test de igualdad de efectos entre regiones (H0: norte = centro = sur):\n")
print(test_igualdad)
cat("\n")

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
