# Tarea 3 – Métodos Cuantitativos I

Repositorio con el código y los datos usados para replicar el análisis de Card y Krueger (1994) sobre el efecto del salario mínimo en el empleo, utilizando R.

## Estructura

* `data/`
  Archivos de datos. Incluye la base original (`CK1994.csv`) y los archivos intermedios generados por los scripts.

* `scripts/`
  Scripts en R, uno por pregunta (`p1_pregunta1.R` … `p8_pregunta8.R`). Todos asumen que el directorio de trabajo es la raíz del proyecto.

* `docs/`
  Documentos auxiliares, como el diccionario de variables (`CK1994_description.pdf`) y el informe final (`Informe.pdf`).

* `run_tarea3.R`
  Script maestro que ejecuta en orden los ocho scripts de la carpeta `scripts/`.

* `HAZ CLICK EN MI.bat` (solo Windows, opcional)
  Lanza `run_tarea3.R` usando `Rscript.exe`.

## Requisitos

* R 4.5.1 (o versión cercana).
* Paquetes: `tidyverse`, `broom`, `car`.
  Se instalan automáticamente si no están presentes.

## Cómo reproducir los resultados

Desde la raíz del proyecto:

1. En R o RStudio:

   ```r
   source("run_tarea3.R")
   ```

   Esto genera todas las tablas y regresiones en la carpeta `output/`, que son las que se usan en `docs/Informe.pdf`.

2. Para ejecutar pregunta por pregunta:

   ```r
   source("scripts/p1_pregunta1.R")
   source("scripts/p2_pregunta2.R")
   source("scripts/p3_pregunta3.R")
   source("scripts/p4_pregunta4.R")
   source("scripts/p5_pregunta5.R")
   source("scripts/p6_pregunta6.R")
   source("scripts/p7_pregunta7.R")
   source("scripts/p8_pregunta8.R")
   ```

3. En Windows también es posible usar el archivo `HAZ CLICK EN MI.bat`, ajustando la ruta de `Rscript.exe` en el propio `.bat` si fuese necesario.

## Salidas

Todos los scripts escriben archivos `.csv` en la carpeta `output/`. Cada archivo corresponde a las tablas de estadística descriptiva, regresiones y cálculos intermedios citados en el informe.
