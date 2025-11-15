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
    fte   = as.numeric(fte)
  )

panel <- panel |>
  mutate(
    treat  = if_else(state == 1, 1, 0),
    grupo  = if_else(treat == 1, "Tratamiento (NJ)", "Control (PA)"),
    periodo = if_else(time == 0, "Pre (time = 0)", "Post (time = 1)")
  )

tabla_p2 <- panel |>
  group_by(grupo, periodo) |>
  summarise(
    media_fte = mean(fte, na.rm = TRUE),
    sd_fte    = sd(fte, na.rm = TRUE),
    n         = sum(!is.na(fte)),
    .groups   = "drop"
  )

tabla_p2 <- tabla_p2 |>
  mutate(
    grupo = factor(grupo,
                   levels = c("Tratamiento (NJ)", "Control (PA)")),
    periodo = factor(periodo,
                     levels = c("Pre (time = 0)", "Post (time = 1)"))
  ) |>
  arrange(grupo, periodo)

tabla_p2 <- tabla_p2 |>
  mutate(
    media_fte = round(media_fte, 3),
    sd_fte    = round(sd_fte, 3)
  )

cat("Tabla Pregunta 2: Promedio y desviación estándar de FTE por grupo y periodo\n")
print(tabla_p2)
cat("\n")

mean_treat_pre  <- panel |> filter(treat == 1, time == 0) |> summarise(m = mean(fte)) |> pull(m)
sd_treat_pre    <- panel |> filter(treat == 1, time == 0) |> summarise(s = sd(fte)) |> pull(s)

mean_treat_post <- panel |> filter(treat == 1, time == 1) |> summarise(m = mean(fte)) |> pull(m)
sd_treat_post   <- panel |> filter(treat == 1, time == 1) |> summarise(s = sd(fte)) |> pull(s)

mean_ctrl_pre   <- panel |> filter(treat == 0, time == 0) |> summarise(m = mean(fte)) |> pull(m)
sd_ctrl_pre     <- panel |> filter(treat == 0, time == 0) |> summarise(s = sd(fte)) |> pull(s)

mean_ctrl_post  <- panel |> filter(treat == 0, time == 1) |> summarise(m = mean(fte)) |> pull(m)
sd_ctrl_post    <- panel |> filter(treat == 0, time == 1) |> summarise(s = sd(fte)) |> pull(s)

cat("Tratamiento (NJ), periodo pre:   media =", round(mean_treat_pre, 3),
    ", sd =", round(sd_treat_pre, 3), "\n")
cat("Tratamiento (NJ), periodo post:  media =", round(mean_treat_post, 3),
    ", sd =", round(sd_treat_post, 3), "\n")
cat("Control (PA), periodo pre:       media =", round(mean_ctrl_pre, 3),
    ", sd =", round(sd_ctrl_pre, 3), "\n")
cat("Control (PA), periodo post:      media =", round(mean_ctrl_post, 3),
    ", sd =", round(sd_ctrl_post, 3), "\n\n")

did_est <- (mean_treat_post - mean_treat_pre) - (mean_ctrl_post - mean_ctrl_pre)

cat("Estimador DID descriptivo (en FTE):\n")
cat("  (NJ post - NJ pre) - (PA post - PA pre) =", round(did_est, 3), "\n\n")

if (!dir.exists("output")) {
  dir.create("output")
}

write_csv(
  tabla_p2,
  file = file.path("output", "tabla_p2_tratamiento_control.csv")
)
