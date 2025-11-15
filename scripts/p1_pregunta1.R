rm(list = ls())

if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
} else {
  library(tidyverse)
}

ruta_datos <- file.path("data", "CK1994.csv")

if (!dir.exists("output")) {
  dir.create("output")
}

ck <- read_csv(
  file = ruta_datos,
  na = "."
)

dim(ck)

ck <- ck |>
  mutate(
    empft = as.numeric(empft),
    emppt = as.numeric(emppt),
    nmgrs = as.numeric(nmgrs),
    state = as.numeric(state),
    time  = as.numeric(time)
  )

ck <- ck |>
  mutate(
    fte = empft + emppt / 2 + nmgrs
  )

total_obs <- nrow(ck)
cat("Número total de observaciones en la muestra original:", total_obs, "\n\n")

tabla_state <- ck |>
  mutate(
    estado = if_else(state == 1, "Nueva Jersey", "Pennsylvania")
  ) |>
  count(estado)

cat("Número de observaciones por estado:\n")
print(tabla_state)
cat("\n")

tabla_time <- ck |>
  mutate(
    periodo = if_else(time == 1, "Segundo periodo (post)", "Primer periodo (pre)")
  ) |>
  count(periodo)

cat("Número de observaciones por periodo de tiempo:\n")
print(tabla_time)
cat("\n")

panel_balanceado <- ck |>
  filter(!is.na(fte)) |>
  group_by(store) |>
  filter(n_distinct(time) == 2) |>
  ungroup()

n_obs_balanceado   <- nrow(panel_balanceado)
n_stores_balanceado <- n_distinct(panel_balanceado$store)

cat("Número de observaciones en el panel balanceado:", n_obs_balanceado, "\n")
cat("Número de restaurantes en el panel balanceado:", n_stores_balanceado, "\n\n")

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

tabla_descriptiva <- tabla_descriptiva |>
  mutate(
    media               = round(media, 3),
    desviacion_estandar = round(desviacion_estandar, 3),
    minimo              = round(minimo, 3),
    maximo              = round(maximo, 3)
  )

cat("Tabla de estadística descriptiva (panel balanceado):\n")
print(tabla_descriptiva)

write_csv(
  panel_balanceado,
  file = file.path("data", "CK1994_panel_balanceado.csv")
)

write_csv(
  tabla_descriptiva,
  file = file.path("output", "tabla_descriptiva_p1.csv")
)
