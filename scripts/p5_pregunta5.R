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

modelo_did <- lm(fte ~ treat + post + did, data = panel)

cat("Resumen corto del modelo DID (coeficientes):\n")
print(coef(summary(modelo_did)))
cat("\n")

betas <- coef(modelo_did)
beta0 <- betas["(Intercept)"]
beta1 <- betas["treat"]
beta2 <- betas["post"]
beta3 <- betas["did"]

cat("beta0 (Constante)                =", beta0, "\n")
cat("beta1 (Tratamiento NJ)           =", beta1, "\n")
cat("beta2 (Periodo post)             =", beta2, "\n")
cat("beta3 (Interacción DID)          =", beta3, "\n\n")

resumen_grupos <- panel |>
  group_by(treat, post) |>
  summarise(
    n = n(),
    media_fte = mean(fte, na.rm = TRUE),
    .groups = "drop"
  )

cat("Medias observadas de FTE por grupo y periodo:\n")
print(resumen_grupos)
cat("\n")

fte_cf_nj_post_modelo <- beta0 + beta1 + beta2

mean_nj_pre  <- resumen_grupos |>
  filter(treat == 1, post == 0) |>
  pull(media_fte)

mean_pa_pre  <- resumen_grupos |>
  filter(treat == 0, post == 0) |>
  pull(media_fte)

mean_pa_post <- resumen_grupos |>
  filter(treat == 0, post == 1) |>
  pull(media_fte)

cambio_pa <- mean_pa_post - mean_pa_pre
fte_cf_nj_post_medias <- mean_nj_pre + cambio_pa

cat("Contrafactual NJ post (modelo DID):", fte_cf_nj_post_modelo, "\n")
cat("Contrafactual NJ post (con medias):", fte_cf_nj_post_medias, "\n\n")

mean_nj_post_obs <- resumen_grupos |>
  filter(treat == 1, post == 1) |>
  pull(media_fte)

did_est <- mean_nj_post_obs - fte_cf_nj_post_medias

cat("Media observada de FTE en NJ post:", mean_nj_post_obs, "\n")
cat("Media contrafactual NJ post (sin aumento salario mínimo):", fte_cf_nj_post_medias, "\n")
cat("Efecto DID (observado - contrafactual) =", did_est, "\n\n")

tabla_p5 <- tibble(
  indicador = c(
    "Media FTE NJ pre",
    "Media FTE PA pre",
    "Media FTE PA post",
    "Cambio en PA (post - pre)",
    "Media FTE NJ post observada",
    "Media FTE NJ post contrafactual (sin aumento)",
    "Efecto DID (observado - contrafactual)"
  ),
  valor = c(
    mean_nj_pre,
    mean_pa_pre,
    mean_pa_post,
    cambio_pa,
    mean_nj_post_obs,
    fte_cf_nj_post_medias,
    did_est
  )
) |>
  mutate(
    valor = round(valor, 3)
  )

cat("Tabla resumen para la Pregunta 5:\n")
print(tabla_p5)
cat("\n")

if (!dir.exists("output")) {
  dir.create("output")
}

write_csv(
  tabla_p5,
  file = file.path("output", "tabla_p5_contrafactual_NJ.csv")
)
