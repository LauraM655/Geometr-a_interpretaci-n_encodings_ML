# Mismo plano, otras coordenadas — codificación de factores (app Shiny)

Estructura
```
app.R
README.md
R/
  core.R             núcleo: codificaciones, ajuste, M^-1, datos de ejemplo/simulados
  geometry.R         matemática geométrica y de "muchos niveles" (sin gráficos)
  geometry_plots.R   gráficos (ggplot2, plotly) y textos didácticos
tests/testthat/
  helper-source.R    carga los archivos de R/
  test-core.R
  test-geometry.R
```
Pestañas: Inicio · Datos · Coeficientes · Equivalencia · Geometría (A plano 2D, B 3D) ·
Muchos niveles (1 mapa de M^-1, 2 un cambio/muchos coeficientes, 3 mismas datos/distintas conclusiones) ·
Con covariable (pendiente) · Simulación (pendiente).

Ejecutar (desde R, en la carpeta del proyecto)
```r
install.packages(c("shiny", "bslib", "DT", "ggplot2", "plotly", "testthat"))  # una vez
testthat::test_dir("tests/testthat")
shiny::runApp()
```
Todos los archivos se guardan en UTF-8.