cat("Iniciando ejecución de todos los scripts de la tarea 3.\n\n")

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

for (ruta in rutas_scripts) {
  if (!file.exists(ruta)) {
    stop(paste("No se encontró el archivo de script:", ruta))
  }
}

cat("Se van a ejecutar, en este orden:\n")
print(rutas_scripts)
cat("\n")

for (ruta in rutas_scripts) {
  cat("============================================================\n")
  cat("Ejecutando script:", ruta, "\n")
  cat("============================================================\n\n")
  
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
