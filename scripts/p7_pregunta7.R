rm(list = ls())

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
    treat     = if_else(state == 1, 1, 0),
    post      = time,
    did       = treat * post
  )

cat("Cantidad de observaciones con hoursopen NA:", sum(is.na(panel$hoursopen)), "\n\n")

modelo_fe_base <- lm(fte ~ did + factor(store) + factor(time), data = panel)

cat("Resumen modelo FE base (sin hoursopen):\n")
print(summary(modelo_fe_base))
cat("\n")

modelo_fe_hours <- lm(fte ~ did + hoursopen + factor(store) + factor(time),
                      data = panel)

cat("Resumen modelo FE con hoursopen:\n")
print(summary(modelo_fe_hours))
cat("\n")

tabla_coef_p7 <- bind_rows(
  tidy(modelo_fe_base) |>
    mutate(modelo = "FE sin hoursopen"),
  tidy(modelo_fe_hours) |>
    mutate(modelo = "FE + hoursopen")
) |>
  filter(
    !str_starts(term, "factor(store)"),
    !str_starts(term, "factor(time)")
  ) |>
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
