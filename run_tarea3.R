############################################################
# Script maestro: ejecuta en orden las 8 preguntas
# Proyecto: tarea3_metodos
# Archivo: run_tarea3.R
############################################################

cat("Iniciando ejecución de todos los scripts de la tarea 3.\n\n")

# 1. Definir rutas de los scripts en el orden correcto --------------------
# Ajusta los nombres si tus archivos tienen nombres distintos.
rutas_scripts <- file.path(
  "scripts",
  c(
    "p1_pregunta1.R",
    "p2_pregunta2.R",
    "p3_pregunta3.R",
    "p4_pregunta4.R",
    "p5_pregunta5.R",
    "p6_pregunta6.R",
    "p7_pregunta7.R",
    "p8_pregunta8.R"
  )
)

# 2. Verificar que todos los archivos existen -----------------------------
for (ruta in rutas_scripts) {
  if (!file.exists(ruta)) {
    stop(paste("No se encontró el archivo de script:", ruta))
  }
}

cat("Se van a ejecutar, en este orden:\n")
print(rutas_scripts)
cat("\n")

# 3. Ejecutar los scripts en orden ----------------------------------------
for (ruta in rutas_scripts) {
  cat("============================================================\n")
  cat("Ejecutando script:", ruta, "\n")
  cat("============================================================\n\n")
  
  # Ejecutar cada script en un entorno local para que
  # el rm(list = ls()) interno NO borre los objetos del maestro
  entorno_local <- new.env(parent = globalenv())
  source(ruta,
         local = entorno_local,
         echo  = TRUE,
         max.deparse.length = Inf)
  
  cat("\n------------------------------------------------------------\n")
  cat("Script terminado correctamente:", ruta, "\n")
  cat("------------------------------------------------------------\n\n")
}

cat("============================================================\n")
cat("Ejecución completa: se han corrido los 8 scripts en orden.\n")
cat("============================================================\n")
