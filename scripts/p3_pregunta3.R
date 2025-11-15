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
    fte   = as.numeric(fte)
  )

panel <- panel |>
  mutate(
    treat = if_else(state == 1, 1, 0),
    post  = time,
    did   = treat * post
  )

tabla_grupos <- panel |>
  group_by(treat, post) |>
  summarise(
    n = n(),
    media_fte = mean(fte, na.rm = TRUE),
    .groups = "drop"
  )

cat("Resumen por grupo y periodo (comprobación):\n")
print(tabla_grupos)
cat("\n")

modelo_did <- lm(fte ~ treat + post + did, data = panel)

cat("Resumen del modelo de diferencias en diferencias (MCO):\n")
print(summary(modelo_did))
cat("\n")

tabla_reg_p3 <- tidy(modelo_did) |>
  mutate(
    termino = recode(
      term,
      "(Intercept)" = "Constante",
      "treat"       = "Tratamiento (NJ = 1)",
      "post"        = "Periodo post (time = 1)",
      "did"         = "Interacción tratamiento x post (DID)"
    ),
    estimacion = round(estimate, 3),
    error_std  = round(std.error, 3),
    estadistico_t = round(statistic, 3),
    p_valor    = round(p.value, 3)
  ) |>
  select(
    termino,
    estimacion,
    error_std,
    estadistico_t,
    p_valor
  )

cat("Tabla lista para usar en el informe (coeficientes MCO):\n")
print(tabla_reg_p3)
cat("\n")

if (!dir.exists("output")) {
  dir.create("output")
}

write_csv(
  tabla_reg_p3,
  file = file.path("output", "regresion_p3_did_ols.csv")
)
