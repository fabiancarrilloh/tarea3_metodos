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
    treat = if_else(state == 1, 1, 0),
    post  = time,
    did   = treat * post
  )

modelo_ols <- lm(fte ~ treat + post + did, data = panel)

cat("Resumen modelo MCO (Preg. 3):\n")
print(summary(modelo_ols))
cat("\n")

modelo_fe <- lm(fte ~ did + factor(store) + factor(time), data = panel)

cat("Resumen modelo con efectos fijos (restaurante + tiempo):\n")
print(summary(modelo_fe))
cat("\n")

tabla_coef <- bind_rows(
  tidy(modelo_ols) |>
    mutate(modelo = "MCO DID"),
  tidy(modelo_fe) |>
    mutate(modelo = "FE restaurante + tiempo")
) |>
  filter(
    !str_starts(term, "factor(store)"),
    !str_starts(term, "factor(time)")
  ) |>
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
