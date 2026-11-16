# Codificación de factores: geometría e interpretación (Paso 1)

Estructura
- `app.R` — app Shiny (pestañas Datos, Coeficientes, Equivalencia; Geometría, Con covariable y Simulación son marcadores para pasos 2–5).
- `R/core.R` — núcleo: `fit_encoding()`, `compare_encodings()`, `describe_contrasts()`, `prepare_data()`, `diagnose_factor()`.
- `tests/testthat/` — pruebas automáticas (equivalencia de predicciones, β = M⁻¹μ̂, coincidencia con `lm()`, Var(β), Helmert balanceado, centrado).

Ejecutar (desde R, en la carpeta del proyecto)
```r
install.packages(c("shiny", "bslib", "DT", "testthat"))   # una sola vez
testthat::test_dir("tests/testthat")                       # pruebas
shiny::runApp()                                            # app
```
