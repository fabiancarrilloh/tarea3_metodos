rm(list = ls())

if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
} else {
  library(tidyverse)
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
    post  = time
  )

cat("Valores distintos de la variable time:\n")
print(sort(unique(panel$time)))
cat("\n")

resumen_grupos <- panel |>
  group_by(treat, post) |>
  summarise(
    n = n(),
    media_fte = mean(fte, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    grupo = if_else(treat == 1, "Tratamiento (NJ)", "Control (PA)"),
    periodo = if_else(post == 1, "Post (time = 1)", "Pre (time = 0)")
  ) |>
  select(grupo, periodo, n, media_fte)

resumen_grupos <- resumen_grupos |>
  mutate(
    media_fte = round(media_fte, 3)
  )

cat("Número de observaciones y media de FTE por grupo y periodo:\n")
print(resumen_grupos)
cat("\n")

if (!dir.exists("output")) {
  dir.create("output")
}

write_csv(
  resumen_grupos,
  file = file.path("output", "resumen_p4_grupos_periodo.csv")
)
