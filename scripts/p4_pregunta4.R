############################################################
# Tarea 3 - Métodos Cuantitativos I
# Pregunta 4: Supuestos de DID y discusión de tendencias paralelas
# Archivo: scripts/p4_pregunta4.R
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
    post  = time
  )

# 3. Chequeos básicos sobre los periodos ----------------------------------
cat("Valores distintos de la variable time:\n")
print(sort(unique(panel$time)))
cat("\n")

# 4. Resumen por grupo (tratamiento/control) y periodo --------------------
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

# 5. Crear carpeta output si no existe ------------------------------------
if (!dir.exists("output")) {
  dir.create("output")
}

# 6. Guardar resumen en un archivo CSV ------------------------------------
write_csv(
  resumen_grupos,
  file = file.path("output", "resumen_p4_grupos_periodo.csv")
)

############################################################
# Comentario (para el informe, no para ejecutar):
#
# 1) Supuesto clave para que el estimador DID sea causal:
#
#    El supuesto central es el de tendencias paralelas. En ausencia
#    del aumento del salario mínimo, el cambio esperado en el empleo
#    full time equivalente entre el periodo pre (time = 0) y el
#    periodo post (time = 1) debería haber sido el mismo en
#    Nueva Jersey (tratamiento) y en Pennsylvania (control).
#
#    En notación:
#
#    E[ Y_1(0) - Y_0(0) | NJ ] = E[ Y_1(0) - Y_0(0) | PA ].
#
#    Donde Y_t(0) es el resultado potencial sin tratamiento.
#
#    Otros requisitos que suelen asumirse:
#      - No hay efectos anticipados del tratamiento en el periodo pre.
#      - La composición de los grupos no cambia endógenamente por
#        el tratamiento (no hay entrada o salida selectiva de locales).
#      - No hay efectos indirectos fuertes del tratamiento en el
#        grupo de control.
#
# 2) Que necesitarías para testear el supuesto de tendencias paralelas:
#
#    Para evaluar empíricamente este supuesto, necesitarías varios
#    periodos pretratamiento. Con al menos dos o tres años antes de
#    la reforma, podrías estimar un modelo con interacciones entre el
#    indicador de tratamiento y dummies de año pre, y testear si esos
#    coeficientes son estadísticamente iguales a cero.
#
#    Si antes de la reforma el empleo en NJ y PA sigue trayectorias
#    similares, eso daría soporte al supuesto de tendencias paralelas.
#    Si se observan tendencias distintas en los años pre, el supuesto
#    sería cuestionable.
#
# 3) Que puedes hacer con la base actual:
#
#    En esta base solo hay dos periodos:
#      - time = 0: encuesta antes del aumento del salario mínimo.
#      - time = 1: encuesta después del aumento.
#
#    No hay varios años pretratamiento, solo un periodo pre y uno
#    post. Por lo tanto, con esta base de datos no puedes implementar
#    un test estándar de tendencias paralelas sobre el empleo, porque
#    no observas la trayectoria de Y_t antes del tratamiento, solo un
#    nivel pre.
#
#    Lo único que puedes hacer es:
#      - Ver si los grupos parecen comparables en el periodo pre en
#        términos de variables observables.
#      - Argumentar, con base en el contexto económico, si es
#        razonable pensar que las tendencias habrían sido similares
#        en ausencia del cambio en el salario mínimo.
#
#    En tu informe debes dejar claro que el supuesto de tendencias
#    paralelas no puede ser testeado directamente con esta base, solo
#    discutido como una hipótesis.
############################################################
