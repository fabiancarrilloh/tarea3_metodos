############################################################
# Tarea 3 - Métodos Cuantitativos I
# Pregunta 3: Modelo de diferencias en diferencias con MCO
# Archivo: scripts/p3_pregunta3.R
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

# broom viene con tidyverse en instalaciones recientes
if (!require("broom")) {
  install.packages("broom")
  library(broom)
} else {
  library(broom)
}

# 2. Cargar panel balanceado ----------------------------------------------
# Usamos el archivo generado en la Pregunta 1
ruta_panel <- file.path("data", "CK1994_panel_balanceado.csv")

panel <- read_csv(
  file = ruta_panel,
  na = "."
)

# Aseguramos tipos numéricos
panel <- panel |>
  mutate(
    state = as.numeric(state),
    time  = as.numeric(time),
    fte   = as.numeric(fte)
  )

# 3. Definir variables de DID ---------------------------------------------
# Tratamiento: Nueva Jersey (state = 1)
# Control: Pennsylvania (state = 0)
# Periodo post: time = 1, periodo pre: time = 0

panel <- panel |>
  mutate(
    treat = if_else(state == 1, 1, 0),
    post  = time,
    did   = treat * post
  )

# Chequeo rápido de tamaños de grupo y periodo
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

# 4. Estimar el modelo DID con MCO ----------------------------------------
# fte_it = beta0 + beta1 * treat_i + beta2 * post_t + beta3 * did_it + u_it

modelo_did <- lm(fte ~ treat + post + did, data = panel)

cat("Resumen del modelo de diferencias en diferencias (MCO):\n")
print(summary(modelo_did))
cat("\n")

# 5. Construir tabla de resultados para el informe -------------------------
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

# 6. Guardar tabla en CSV --------------------------------------------------
if (!dir.exists("output")) {
  dir.create("output")
}

write_csv(
  tabla_reg_p3,
  file = file.path("output", "regresion_p3_did_ols.csv")
)

############################################################
# Notas para interpretación (para tu informe, no es código):
#
# - La constante (Constante) es el empleo promedio fte del grupo de control
#   (Pennsylvania) en el periodo pre. Debería estar alrededor de 23.44.
#
# - El coeficiente "Tratamiento (NJ = 1)" es la diferencia promedio en fte
#   entre Nueva Jersey y Pennsylvania en el periodo pre. Sale negativa,
#   aproximadamente -2.96, lo que indica que antes del aumento del salario
#   mínimo los locales de NJ tenían, en promedio, unos 3 trabajadores
#   equivalentes a tiempo completo menos que los de PA.
#
# - El coeficiente "Periodo post (time = 1)" es el cambio promedio en fte
#   en Pennsylvania entre el periodo pre y el post. Es aproximadamente -2.30,
#   lo que indica una caída del empleo en el grupo de control.
#
# - El coeficiente "Interacción tratamiento x post (DID)" es el estimador
#   de diferencias en diferencias. Numéricamente es cercano a 2.75.
#   Esto significa que, comparado con la evolución en Pennsylvania,
#   el empleo en Nueva Jersey aumentó en torno a 2.75 trabajadores
#   equivalentes a tiempo completo por local.
#
# - Con los errores estándar clásicos de MCO, este coeficiente no resulta
#   estadísticamente significativo al 10 % (p-valor alrededor de 0.11),
#   por lo que no se puede rechazar la hipótesis de que el efecto verdadero
#   sea 0. Aun así, la señal del coeficiente sugiere que el aumento del
#   salario mínimo en NJ no redujo el empleo respecto a PA, y es consistente
#   con un efecto levemente positivo.
############################################################
