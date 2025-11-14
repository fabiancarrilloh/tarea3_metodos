############################################################
# Tarea 3 - Métodos Cuantitativos I
# Pregunta 5: Empleo FTE contrafactual en NJ sin aumento
# del salario mínimo (bajo supuestos DID)
# Archivo: scripts/p5_pregunta5.R
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
    treat = if_else(state == 1, 1, 0),
    post  = time,
    did   = treat * post
  )

# 3. Volver a estimar el modelo DID (por claridad) ------------------------
# fte = beta0 + beta1 * treat + beta2 * post + beta3 * did + u

modelo_did <- lm(fte ~ treat + post + did, data = panel)

cat("Resumen corto del modelo DID (coeficientes):\n")
print(coef(summary(modelo_did)))
cat("\n")

# Extraemos coeficientes
betas <- coef(modelo_did)
beta0 <- betas["(Intercept)"]
beta1 <- betas["treat"]
beta2 <- betas["post"]
beta3 <- betas["did"]

cat("beta0 (Constante)                =", beta0, "\n")
cat("beta1 (Tratamiento NJ)           =", beta1, "\n")
cat("beta2 (Periodo post)             =", beta2, "\n")
cat("beta3 (Interacción DID)          =", beta3, "\n\n")

# 4. Medias observadas por grupo y periodo ---------------------------------
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

# 5. Calcular el contrafactual para NJ en el periodo post ------------------
# Dos formas equivalentes:

# (a) Usando el modelo DID:
#     E[Y | NJ, post, sin tratamiento] = beta0 + beta1 + beta2
fte_cf_nj_post_modelo <- beta0 + beta1 + beta2

# (b) Usando promedios de P2:
#     E[Y_NJ_post^0] = mean(NJ, pre) + (mean(PA, post) - mean(PA, pre))

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

# 6. Comparar con el valor observado en NJ post ----------------------------
mean_nj_post_obs <- resumen_grupos |>
  filter(treat == 1, post == 1) |>
  pull(media_fte)

did_est <- mean_nj_post_obs - fte_cf_nj_post_medias

cat("Media observada de FTE en NJ post:", mean_nj_post_obs, "\n")
cat("Media contrafactual NJ post (sin aumento salario mínimo):", fte_cf_nj_post_medias, "\n")
cat("Efecto DID (observado - contrafactual) =", did_est, "\n\n")

# 7. Armar tabla resumen para el informe ----------------------------------
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

# 8. Guardar salida en output ----------------------------------------------
if (!dir.exists("output")) {
  dir.create("output")
}

write_csv(
  tabla_p5,
  file = file.path("output", "tabla_p5_contrafactual_NJ.csv")
)

############################################################
# Para escribir en el informe:
#
# Asumiendo que el supuesto de tendencias paralelas y los demás
# supuestos de DID se cumplen, el empleo full-time equivalente
# promedio en Nueva Jersey en el periodo posterior al aumento
# del salario mínimo habría sido aproximadamente 18.18 trabajadores
# por local si no hubiese habido cambio en el salario mínimo.
#
# El empleo observado en ese periodo es cercano a 20.93 FTE, de modo
# que el estimador DID sugiere un efecto de alrededor de 2.75 FTE
# adicionales por local respecto a ese contrafactual.
############################################################
