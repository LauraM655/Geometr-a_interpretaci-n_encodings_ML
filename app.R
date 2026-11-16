# =====================================================================
# app.R -- "Mismo plano, otras coordenadas"
# Codificación de factores: geometría e interpretación
# Ejecutar:  shiny::runApp()   (desde la carpeta del proyecto)
# Paquetes:  install.packages(c("shiny", "bslib", "DT", "ggplot2", "plotly"))
# =====================================================================
library(shiny)
library(bslib)
library(DT)

source("R/core.R", local = TRUE, encoding = "UTF-8")
source("R/geometry.R", local = TRUE, encoding = "UTF-8")
source("R/geometry_plots.R", local = TRUE, encoding = "UTF-8")

has_gg <- requireNamespace("ggplot2", quietly = TRUE)
has_plotly <- requireNamespace("plotly", quietly = TRUE)
missing_pkg <- function(pkg) {
  div(class = "alert alert-warning",
      "Falta el paquete ", tags$code(pkg), ". Instálalo con ",
      tags$code(sprintf('install.packages("%s")', pkg)), " y reinicia la app.")
}
plot_or_msg <- function(id, height = "auto") {
  if (has_gg) plotOutput(id, height = height) else missing_pkg("ggplot2")
}
como_leer <- function(...) {
  accordion(open = FALSE, accordion_panel("¿Cómo se lee esta figura?", tags$ol(...)))
}

# ---- identidad visual ------------------------------------------------
ENC_COL_BY_NAME <- setNames(unname(ENC_COL), unname(ENCODING_NAMES[names(ENC_COL)]))

css_rules <- "
.navbar { background: linear-gradient(120deg, #14213d 0%, #1f3a5f 100%) !important;
          border-bottom: 3px solid #f59e0b; }
.navbar .navbar-brand { color: #fff !important; font-family: var(--bs-heading-font-family);
          font-weight: 600; line-height: 1.1; }
.navbar .navbar-brand small { display: block; font-family: var(--bs-body-font-family);
          font-size: .62rem; font-weight: 500; opacity: .7; text-transform: uppercase;
          letter-spacing: .14em; margin-top: 2px; }
.navbar .nav-link { color: #cbd5e1 !important; font-weight: 500; }
.navbar .nav-link:hover { color: #fff !important; }
.navbar .nav-link.active { color: #fff !important; box-shadow: inset 0 -3px 0 #f59e0b; }
.bslib-sidebar-layout > .sidebar { background: #f3efe6; }
.sidebar label, .sidebar .control-label { font-size: .78rem; font-weight: 600;
          text-transform: uppercase; letter-spacing: .06em; color: #475569; }
.card { background: #fff; border: 0; border-radius: 14px;
        box-shadow: 0 1px 3px rgba(20,33,61,.08), 0 10px 28px rgba(20,33,61,.06); }
.card-header { background: transparent; border-bottom: 1px solid #ece7da;
        font-family: var(--bs-heading-font-family); font-weight: 600; font-size: 1.02rem; }
.idea { border-left: 4px solid #f59e0b; background: #fff8e6; border-radius: 8px;
        padding: .8rem 1.1rem; margin: 0 0 .5rem 0; }
.story { border-left: 4px solid #0f766e; background: #ecf7f5; border-radius: 8px;
        padding: .7rem 1rem; margin: .6rem 0; font-size: .97rem; }
.warnbox { border-left: 4px solid #e4572e; background: #fdeeea; border-radius: 8px;
        padding: .7rem 1rem; margin: .6rem 0; font-size: .97rem; }
.enc-chip { display: inline-block; border-radius: 999px; padding: .1rem .65rem;
        color: #fff; font-size: .78rem; font-weight: 600; margin-right: .3rem; }
.step-num { display: inline-block; width: 1.7rem; height: 1.7rem; line-height: 1.7rem;
        text-align: center; border-radius: 50%; background: #f59e0b; color: #14213d;
        font-weight: 700; margin-right: .5rem; }
table.dataTable thead th { font-size: .72rem; text-transform: uppercase;
        letter-spacing: .05em; color: #1f2a44; }
table.dataTable td { font-variant-numeric: tabular-nums; }
.placeholder-card { text-align: center; padding: 3.5rem 1rem; color: #64748b; }
.placeholder-card .step { font-family: var(--bs-heading-font-family); font-size: 3.4rem;
        font-weight: 700; color: #f59e0b; line-height: 1; }
.placeholder-card h4 { color: #1f2a44; margin-top: .6rem; }
"

tema <- bs_theme(
  version = 5,
  bg = "#fbfaf6", fg = "#1f2a44",
  primary = "#0f766e", secondary = "#64748b", success = "#0f766e", danger = "#e4572e",
  base_font = font_collection(font_google("Inter", local = FALSE), "system-ui", "sans-serif"),
  heading_font = font_collection(font_google("Fraunces", local = FALSE), "Georgia", "serif"),
  code_font = font_collection(font_google("JetBrains Mono", local = FALSE), "monospace")
)
tema <- bs_add_rules(tema, css_rules)

dt_es <- list(
  search = "Buscar:", zeroRecords = "Sin resultados",
  info = "_START_–_END_ de _TOTAL_", infoEmpty = "",
  paginate = list(`next` = "Siguiente", previous = "Anterior")
)
dt_compact <- list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE,
                   scrollY = "300px", scrollCollapse = TRUE, language = dt_es)
dt_small <- list(dom = "t", paging = FALSE, ordering = FALSE, language = dt_es)

chip <- function(tp, txt) {
  span(class = "enc-chip", style = paste0("background:", ENC_COL[[tp]]), txt)
}

placeholder <- function(paso, titulo, texto) {
  card(div(class = "placeholder-card",
           div(class = "step", paso), h4(titulo), p(texto),
           span(class = "badge text-bg-secondary", "Próximamente")))
}

enc_choices <- setNames(ENCODING_TYPES, ENCODING_NAMES[ENCODING_TYPES])

# ---- interfaz --------------------------------------------------------
ui <- page_navbar(
  title = span(class = "brand", "Mismo plano, otras coordenadas",
               tags$small("Codificación de factores · modelos lineales")),
  theme = tema,
  header = withMathJax(),
  sidebar = sidebar(
    width = 330,
    selectInput("dataset", "Datos",
                c("iris · 3 especies" = "iris", "mtcars · 3 cilindrajes" = "mtcars",
                  "ToothGrowth · 3 dosis" = "ToothGrowth",
                  "CO2 · 12 plantas" = "CO2", "ChickWeight · 50 pollitos" = "ChickWeight",
                  "Simulado · K ajustable" = "Simulado", "Subir CSV" = "Subir CSV")),
    conditionalPanel(
      "input.dataset == 'Subir CSV'",
      fileInput("file", "Archivo CSV", accept = c(".csv", ".txt"),
                buttonLabel = "Elegir…", placeholder = "Ningún archivo"),
      checkboxInput("csv_semicolon", "Separador ';' y decimal ','", FALSE)
    ),
    conditionalPanel(
      "input.dataset == 'Simulado'",
      sliderInput("sim_K", "Número de niveles K", min = 3, max = 40, value = 12, step = 1),
      sliderInput("sim_n", "Observaciones por nivel (promedio)", min = 2, max = 30, value = 8, step = 1),
      checkboxInput("sim_unb", "Niveles desbalanceados", FALSE),
      numericInput("sim_seed", "Semilla", value = 1, min = 1, step = 1)
    ),
    uiOutput("ui_vars"),
    hr(),
    selectInput("encoding", "Codificación", enc_choices),
    uiOutput("ui_ref"),
    conditionalPanel(
      "input.xvar != ''",
      radioButtons("center", "Covariable",
                   c("Sin centrar (x = 0)" = "zero", "Centrada en su media" = "mean"))
    )
  ),
  
  # ---------------- Inicio ----------------
  nav_panel("Inicio",
            div(class = "idea",
                tags$b("Idea central. "),
                "Para meter un factor (por ejemplo, plantas, colegios, marcas) en un modelo lineal hay que ",
                "convertirlo en columnas numéricas: eso es una ", tags$b("codificación"), ". ",
                "Hay muchas, y todas ", tags$b("predicen exactamente lo mismo"), " con un solo factor, ",
                "pero cada una le da un ", tags$b("significado distinto"), " a los coeficientes. ",
                "Cuando el factor tiene muchos niveles, leer mal un coeficiente es fácil: ",
                "eso es lo que esta app te enseña a ver."),
            layout_columns(
              col_widths = c(7, 5),
              card(card_header("Ruta sugerida (5 minutos)"),
                   tags$p(span(class = "step-num", "1"), tags$b("Datos. "),
                          "Elige un ejemplo en la barra lateral y mira los niveles del factor."),
                   tags$p(span(class = "step-num", "2"), tags$b("Coeficientes. "),
                          "Cambia la codificación y observa qué compara cada coeficiente."),
                   tags$p(span(class = "step-num", "3"), tags$b("Equivalencia. "),
                          "Comprueba que las predicciones son idénticas aunque los coeficientes cambien."),
                   tags$p(span(class = "step-num", "4"), tags$b("Geometría. "),
                          "Ve por qué: es el mismo plano con otros ejes."),
                   tags$p(span(class = "step-num", "5"), tags$b("Muchos niveles. "),
                          "Elige «Simulado» o «ChickWeight» y descubre cómo se enreda la interpretación.")),
              card(card_header("Las tres codificaciones"),
                   tags$p(chip("dummy", "Dummy"), "Cada nivel se compara con uno de referencia que tú eliges."),
                   tags$p(chip("effects", "Efectos"), "Cada nivel se compara con el promedio de todos los niveles."),
                   tags$p(chip("helmert", "Helmert"), "Cada nivel se compara con el promedio de los niveles anteriores."))
            ),
            accordion(open = FALSE,
                      accordion_panel("Mini glosario",
                                      tags$ul(
                                        tags$li(tags$b("Factor / nivel:"), " variable categórica y cada una de sus categorías."),
                                        tags$li(tags$b("Media de nivel μ:"), " promedio de la respuesta dentro de un nivel."),
                                        tags$li(tags$b("Matriz de codificación M:"), " tabla K×K que dice cómo se construyen las columnas; ",
                                                "\\(\\boldsymbol\\mu = M\\boldsymbol\\beta\\)."),
                                        tags$li(tags$b("M⁻¹:"), " su inversa. Cada fila dice qué comparación de medias estima un coeficiente: ",
                                                "\\(\\hat{\\boldsymbol\\beta} = M^{-1}\\hat{\\boldsymbol\\mu}\\)."),
                                        tags$li(tags$b("Confusión de efectos:"), " un único cambio real en un nivel aparece repartido en ",
                                                "varios coeficientes, así que un coeficiente no siempre es «el efecto de ese nivel»."))))
  ),
  
  # ---------------- Datos ----------------
  nav_panel("Datos",
            div(class = "idea",
                "Con un solo factor, las codificaciones ",
                chip("dummy", "dummy"), chip("effects", "efectos"), chip("helmert", "Helmert"),
                " generan el ", tags$b("mismo subespacio"), " \\(\\mathcal{C}(X)\\): mismas predicciones ",
                "\\(\\hat y\\), mismos residuos y mismo \\(\\hat\\sigma^2\\). ",
                "Solo cambian las ", tags$b("coordenadas"), " \\(\\hat{\\boldsymbol\\beta}\\), ",
                "y con ellas lo que cada coeficiente significa."),
            layout_columns(
              col_widths = c(7, 5),
              card(card_header("Vista previa de los datos"), DTOutput("tbl_data")),
              card(card_header("Diagnóstico del factor"),
                   verbatimTextOutput("diag"),
                   plotOutput("plot_groups", height = 270))
            )
  ),
  
  # ---------------- Coeficientes ----------------
  nav_panel("Coeficientes",
            div(class = "idea",
                withMathJax(
                  div(
                    "$$\\boldsymbol{\\mu}=M\\boldsymbol{\\beta}\\;\\Longleftrightarrow\\;",
                    "\\hat{\\boldsymbol{\\beta}}=M^{-1}\\hat{\\boldsymbol{\\mu}},\\qquad",
                    "\\operatorname{Var}(\\hat{\\boldsymbol{\\beta}})=\\sigma^2\\,M^{-1}",
                    "\\operatorname{diag}(1/n_g)\\,M^{-\\top}$$"),
                  "Cada fila de \\(M^{-1}\\) dice qué comparación de medias estima ese coeficiente.")),
            card(card_header("Estimaciones con la codificación elegida"), DTOutput("tbl_coef")),
            layout_columns(
              col_widths = c(7, 5),
              card(card_header(HTML("¿Qué estima cada coeficiente? (filas de <i>M</i><sup>−1</sup>)")),
                   DTOutput("tbl_interp")),
              card(card_header(HTML("Matriz <i>M</i><sup>−1</sup>:  <b>β</b> = <i>M</i><sup>−1</sup> <b>μ</b>")),
                   DTOutput("tbl_minv"))
            )
  ),
  
  # ---------------- Equivalencia ----------------
  nav_panel("Equivalencia",
            uiOutput("vb"),
            card(card_header("Resumen del ajuste por codificación"), DTOutput("tbl_equiv")),
            card(card_header("Coeficientes lado a lado: mismo ajuste, distintas coordenadas"),
                 DTOutput("tbl_side"))
  ),
  
  # ---------------- Geometría ----------------
  nav_panel("Geometría",
            div(class = "idea",
                "Una codificación es un ", tags$b("sistema de coordenadas"), ". Con ", tags$b("K = 2"),
                " niveles el plano de medias \\((\\mu_1,\\mu_2)\\) es el mismo en las tres; lo que cambia es ",
                "la cuadrícula: los ejes son las ", tags$b("columnas de \\(M\\)"),
                " y las rectas de nivel de cada \\(\\beta_i\\) salen de la ", tags$b("fila \\(i\\) de \\(M^{-1}\\)"), "."),
            navset_card_tab(
              nav_panel("A · Plano de medias",
                        layout_columns(
                          col_widths = c(3, 9),
                          card(card_header("Controles"),
                               radioButtons("plane_view", "Codificación a mostrar",
                                            choices = c(enc_choices, "Las tres (comparar)" = "all"),
                                            selected = "dummy"),
                               uiOutput("ui_levs"),
                               sliderInput("mu1", "μ nivel 1", min = 0, max = 1, value = 0.5),
                               sliderInput("mu2", "μ nivel 2", min = 0, max = 1, value = 0.5),
                               actionButton("reset_mu", "Volver a las medias de los datos",
                                            class = "btn-outline-secondary btn-sm"),
                               p(class = "text-muted small mt-3",
                                 "Los deslizadores parten de las medias reales de los dos niveles elegidos ",
                                 "(sin covariable). Muévelos y mira cómo cambian β₀ y β₁.")),
                          card(card_header("Mismo plano, otras coordenadas"),
                               plot_or_msg("plot_plane"),
                               uiOutput("plane_story_ui"),
                               como_leer(
                                 tags$li("Cada punto del plano es un par de medias (μ₁, μ₂). El punto rojo es el tuyo."),
                                 tags$li("Los ejes punteados son los ejes de la codificación (columnas de M); no son perpendiculares."),
                                 tags$li("Las flechas gruesas son las coordenadas: avanza β₀ en el primer eje y luego β₁ en el segundo para llegar al punto."),
                                 tags$li("Elige otra codificación: el punto no se mueve, pero cambian los ejes y por tanto β₀ y β₁."),
                                 tags$li("La recta roja discontinua (μ₁ = μ₂) es β₁ = 0 en las tres: por eso el contraste «los niveles son iguales» no depende de la codificación.")),
                               DTOutput("tbl_plane"))
                        )),
              nav_panel("B · Espacio de observaciones (3D)",
                        layout_columns(
                          col_widths = c(4, 8),
                          card(card_header("Ejemplo de juguete"),
                               p("Tres observaciones, dos niveles: \\(y=(2,4,7)\\), niveles \\((A,A,B)\\). ",
                                 "Las predicciones posibles forman un plano \\(\\mathcal{C}(X)\\). ",
                                 "\\(\\hat y\\) es la proyección ortogonal de \\(y\\) sobre ese plano y el residuo ",
                                 "\\(e=y-\\hat y\\) es perpendicular a él."),
                               radioButtons("toy_enc", "Camino de coordenadas",
                                            choices = c(enc_choices, "Las tres" = "all"), selected = "dummy"),
                               div(class = "story",
                                   "Cada codificación llega al ", tags$b("mismo"), " \\(\\hat y\\): avanza ",
                                   "\\(\\beta_0\\) veces la columna 1 y luego \\(\\beta_1\\) veces la columna 2. ",
                                   "Como la columna 2 es distinta, los \\(\\beta\\) también lo son."),
                               DTOutput("tbl_toy")),
                          card(card_header("y, su proyección y el camino de cada base"),
                               if (has_plotly) plotly::plotlyOutput("plot_toy", height = "560px")
                               else missing_pkg("plotly"),
                               como_leer(
                                 tags$li("El plano verde es el espacio columna: todo lo que el modelo puede predecir."),
                                 tags$li("y (punto oscuro) no está en el plano; ŷ (punto rojo) es su sombra perpendicular."),
                                 tags$li("La línea de color es el camino origen → β₀·col.1 → ŷ. Pasa el ratón sobre la leyenda para ver los valores."),
                                 tags$li("Elige «Las tres»: tres caminos distintos que terminan en el mismo ŷ.")))
                        ))
            )),
  
  # ---------------- Muchos niveles ----------------
  nav_panel("Muchos niveles",
            div(class = "idea",
                tags$b("Aquí está lo importante. "),
                "Con K = 2 todo es sencillo. Con muchos niveles, la codificación decide ",
                tags$b("cuántos niveles se mezclan en cada coeficiente"),
                " y por tanto qué puedes (y qué no puedes) concluir al leerlo. ",
                "Prueba el ejemplo ", tags$b("«Simulado»"), " o ", tags$b("«ChickWeight»"), " y sube K."),
            uiOutput("ui_cov_note"),
            navset_card_tab(
              nav_panel("1 · ¿Qué compara cada coeficiente?",
                        plot_or_msg("plot_heat"),
                        DTOutput("tbl_involved"),
                        como_leer(
                          tags$li("Cada fila es un coeficiente y cada columna un nivel del factor."),
                          tags$li("Una celda coloreada significa que la media de ese nivel entra en la comparación; el color indica el peso (+ verde, − coral)."),
                          tags$li("Dummy: filas casi vacías (cada coeficiente compara solo 2 niveles). Es fácil de leer, pero depende de la referencia."),
                          tags$li("Efectos: filas totalmente llenas: cada coeficiente mezcla los K niveles."),
                          tags$li("Helmert: forma triangular: cada coeficiente mezcla un número creciente de niveles."))),
              nav_panel("2 · Un cambio, muchos coeficientes",
                        layout_columns(
                          col_widths = c(3, 9),
                          card(card_header("Experimento"),
                               uiOutput("ui_pert"),
                               p(class = "text-muted small",
                                 "Imagina que solo la media de UN nivel cambia. ¿Cuántos coeficientes lo reflejan?")),
                          card(plot_or_msg("plot_pert", height = "440px"),
                               uiOutput("pert_text"),
                               como_leer(
                                 tags$li("Elige un nivel y cuánto cambia su media; ningún otro nivel cambia."),
                                 tags$li("Cada barra roja es un coeficiente que se mueve por ese único cambio."),
                                 tags$li("En dummy, cambiar un nivel normal mueve 1 coeficiente, pero cambiar la referencia mueve TODOS."),
                                 tags$li("En efectos, cualquier nivel mueve todos los coeficientes. En Helmert depende del orden de los niveles."),
                                 tags$li("Conclusión: un coeficiente no siempre es «el efecto de ese nivel». Eso es la confusión de efectos.")))
                        )),
              nav_panel("3 · Mismos datos, distintas conclusiones",
                        plot_or_msg("plot_ci", height = "auto"),
                        uiOutput("ci_text"),
                        como_leer(
                          tags$li("Cada panel ajusta exactamente los mismos datos con una codificación distinta."),
                          tags$li("Cada punto es la estimación de un coeficiente con su intervalo de confianza del 95 %."),
                          tags$li("Rojo: el intervalo no incluye 0 («significativo»). Gris: sí lo incluye."),
                          tags$li("Los recuentos cambian porque cada codificación contrasta una hipótesis distinta (β = 0 significa algo diferente en cada una)."),
                          tags$li("Cambia el nivel de referencia en la barra lateral: en dummy cambia la lista de coeficientes «significativos».")))
            )),
  
  nav_panel("Con covariable",
            placeholder("·", "Interacción factor × covariable",
                        "Cómo el centrado de x cambia el significado de los coeficientes de nivel.")),
  nav_panel("Simulación",
            placeholder("·", "Target encoding y fuga de información",
                        "Simulación bajo demanda: sesgo, cobertura y error tipo I.")))

# ---- servidor --------------------------------------------------------
server <- function(input, output, session) {
  
  raw <- reactive({
    if (input$dataset == "Subir CSV") {
      req(input$file)
      if (isTRUE(input$csv_semicolon)) read.csv2(input$file$datapath, stringsAsFactors = FALSE)
      else read.csv(input$file$datapath, stringsAsFactors = FALSE)
    } else if (input$dataset == "Simulado") {
      req(input$sim_K, input$sim_n)
      seed <- if (is.null(input$sim_seed) || is.na(input$sim_seed)) 1 else input$sim_seed
      simulate_factor_data(K = input$sim_K, n_per = input$sim_n,
                           unbalanced = isTRUE(input$sim_unb), seed = seed)
    } else {
      get_example(input$dataset)
    }
  })
  
  output$ui_vars <- renderUI({
    df <- raw()
    nms <- names(df)
    is_num <- vapply(df, is.numeric, logical(1))
    nlev <- vapply(df, function(v) length(unique(v[!is.na(v)])), numeric(1))
    ex <- EXAMPLES[[input$dataset]]
    ysel <- if (!is.null(ex) && ex$y %in% nms[is_num]) ex$y else nms[is_num][1]
    f_ok <- nms[nlev >= 2 & nlev <= 60]
    fsel <- if (!is.null(ex) && ex$f %in% f_ok) ex$f else f_ok[1]
    xsel <- if (!is.null(ex)) ex$x else ""
    tagList(
      selectInput("yvar", "Respuesta (numérica)", nms[is_num], selected = ysel),
      selectInput("fvar", "Factor (2 a 60 niveles)", f_ok, selected = fsel),
      selectInput("xvar", "Covariable continua (opcional)",
                  c("(ninguna)" = "", nms[is_num]), selected = xsel)
    )
  })
  
  dat <- reactive({
    req(input$yvar, input$fvar)
    xname <- if (is.null(input$xvar)) "" else input$xvar
    res <- tryCatch(prepare_data(raw(), input$yvar, input$fvar, xname),
                    error = function(e) conditionMessage(e))
    validate(need(is.list(res), res))
    res
  })
  
  output$ui_ref <- renderUI({
    d <- dat()
    lev <- levels(d$g)
    eff <- identical(input$encoding, "effects")
    selectInput("ref",
                if (eff) "Nivel omitido (su efecto = −suma de los demás)" else "Nivel de referencia",
                lev, selected = if (eff) lev[length(lev)] else lev[1])
  })
  
  cur_ref <- reactive({
    d <- dat(); lev <- levels(d$g)
    if (!is.null(input$ref) && input$ref %in% lev) input$ref
    else if (identical(input$encoding, "effects")) lev[length(lev)] else lev[1]
  })
  
  x0 <- reactive({
    d <- dat()
    if (is.null(d$x) || !identical(input$center, "mean")) 0 else mean(d$x)
  })
  
  fit <- reactive({
    d <- dat()
    res <- tryCatch(
      fit_encoding(d$y, d$g, input$encoding, ref = cur_ref(), x = d$x, x_center = x0()),
      error = function(e) conditionMessage(e))
    validate(need(is.list(res), res))
    res
  })
  
  cmp <- reactive({
    d <- dat()
    res <- tryCatch(
      compare_encodings(d$y, d$g, x = d$x, x_center = x0(),
                        ref = if (input$encoding == "effects") NULL else cur_ref()),
      error = function(e) conditionMessage(e))
    validate(need(is.list(res), res))
    res
  })
  
  # ---- Datos ---------------------------------------------------------
  output$tbl_data <- renderDT(
    datatable(head(raw(), 200), rownames = FALSE, class = "compact stripe hover",
              options = list(pageLength = 8, scrollX = TRUE, language = dt_es, dom = "tp"))
  )
  output$diag <- renderText({
    d <- dat()
    paste(diagnose_factor(d$g, has_x = !is.null(d$x)), collapse = "\n\n")
  })
  output$plot_groups <- renderPlot({
    d <- dat()
    op <- par(mar = c(4, 4, 1, 1), bty = "n", las = 1); on.exit(par(op))
    boxplot(d$y ~ d$g, xlab = d$fname, ylab = d$yname, col = "#f3efe6",
            border = "#64748b", outline = FALSE, las = if (nlevels(d$g) > 8) 2 else 1)
    stripchart(d$y ~ d$g, vertical = TRUE, method = "jitter", pch = 16,
               col = adjustcolor("#0f766e", 0.55), add = TRUE)
    points(seq_len(nlevels(d$g)), tapply(d$y, d$g, mean), pch = 18,
           col = "#e4572e", cex = 2)
    legend("topleft", legend = "media del nivel", pch = 18, col = "#e4572e",
           bty = "n", cex = .9)
  })
  
  # ---- Coeficientes --------------------------------------------------
  output$tbl_coef <- renderDT({
    datatable(coef_table(fit()), rownames = FALSE, class = "compact stripe hover",
              options = dt_compact)
  })
  output$tbl_interp <- renderDT({
    datatable(describe_contrasts(fit(), html = TRUE), rownames = FALSE, escape = FALSE,
              class = "compact stripe hover", options = dt_compact)
  })
  output$tbl_minv <- renderDT({
    f <- fit()
    m <- as.data.frame(round(f$Minv, 3))
    colnames(m) <- paste0("μ[", f$levels, "]")
    datatable(m, class = "compact", options = dt_compact) |>
      formatStyle(seq_len(ncol(m)), fontWeight = "600",
                  backgroundColor = styleInterval(c(-1e-9, 1e-9),
                                                  c("#fbd5cb", "#f1f5f9", "#c8ebe4")))
  })
  
  # ---- Equivalencia --------------------------------------------------
  output$vb <- renderUI({
    cc <- cmp()
    ok <- cc$equal_fitted && cc$equal_sigma2
    layout_column_wrap(
      width = 1 / 3, fill = FALSE,
      value_box(title = "Predicciones ŷ", value = if (ok) "Idénticas" else "Difieren",
                theme = if (ok) "success" else "danger",
                p(if (ok) "Las tres codificaciones generan el mismo subespacio."
                  else "Revisar: no deberían diferir.")),
      value_box(title = "Máxima diferencia en ajustados",
                value = format(max(cc$summary$max_dif_ajustados), digits = 2, scientific = TRUE),
                theme = "secondary", p("error numérico, no estadístico")),
      value_box(title = "σ̂² (común)", value = format(cc$summary$sigma2[1], digits = 4),
                theme = "secondary", p("mismo RSS y mismos grados de libertad"))
    )
  })
  output$tbl_equiv <- renderDT({
    s <- cmp()$summary
    s$RSS <- round(s$RSS, 6); s$sigma2 <- round(s$sigma2, 6)
    s$max_dif_ajustados <- signif(s$max_dif_ajustados, 3)
    datatable(s, rownames = FALSE, class = "compact", options = dt_small) |>
      formatStyle("Codificacion", fontWeight = "700",
                  color = styleEqual(names(ENC_COL_BY_NAME), unname(ENC_COL_BY_NAME)))
  })
  output$tbl_side <- renderDT({
    fits <- cmp()$fits
    K <- fits[[1]]$K
    nm <- if (fits[[1]]$has_x) c(paste0("coef ", seq_len(K)), paste0("pend. ", seq_len(K)))
    else paste0("coef ", seq_len(K))
    side <- data.frame(Parametro = nm, check.names = FALSE)
    for (tp in names(fits)) side[[ENCODING_NAMES[[tp]]]] <- round(unname(fits[[tp]]$coefficients), 4)
    datatable(side, rownames = FALSE, class = "compact stripe hover", options = dt_compact)
  })
  
  # ---- Geometría · Panel A: plano de medias ---------------------------
  output$ui_levs <- renderUI({
    lev <- levels(dat()$g)
    tagList(
      selectInput("lev_a", "Nivel 1 (eje x)", lev, selected = lev[1]),
      selectInput("lev_b", "Nivel 2 (eje y)", lev, selected = lev[2])
    )
  })
  
  lev_ab <- reactive({
    req(input$lev_a, input$lev_b)
    validate(need(input$lev_a != input$lev_b, "Elige dos niveles distintos."))
    c(input$lev_a, input$lev_b)
  })
  
  observeEvent(list(input$lev_a, input$lev_b, input$reset_mu, dat()), {
    ab <- lev_ab()
    d <- dat()
    r <- range(d$y)
    pad <- 0.15 * diff(r); if (pad < 1e-9) pad <- 1
    rng <- c(r[1] - pad, r[2] + pad)
    stp <- signif(diff(rng) / 200, 1); if (stp <= 0) stp <- 0.01
    updateSliderInput(session, "mu1", label = paste0("μ(", ab[1], ")"),
                      min = rng[1], max = rng[2], step = stp,
                      value = mean(d$y[d$g == ab[1]]))
    updateSliderInput(session, "mu2", label = paste0("μ(", ab[2], ")"),
                      min = rng[1], max = rng[2], step = stp,
                      value = mean(d$y[d$g == ab[2]]))
  }, ignoreInit = FALSE)
  
  output$plot_plane <- renderPlot({
    req(input$mu1, input$mu2, input$plane_view)
    types <- if (input$plane_view == "all") ENCODING_TYPES else input$plane_view
    plot_mu_plane(c(input$mu1, input$mu2), lev_ab(), types)
  }, res = 96, height = function() if (identical(input$plane_view, "all")) 480 else 620)
  
  output$plane_story_ui <- renderUI({
    req(input$mu1, input$mu2, input$plane_view)
    mu <- c(input$mu1, input$mu2)
    if (input$plane_view == "all") {
      div(class = "story",
          "Las tres codificaciones ubican ", tags$b("el mismo punto"), " con coordenadas distintas. ",
          "Mira cómo cambian los ejes y los valores de β₀ y β₁ en cada panel; ",
          "la tabla de abajo explica qué comparación hace cada uno.")
    } else {
      div(class = "story", HTML(plane_story(input$plane_view, mu, lev_ab())))
    }
  })
  
  output$tbl_plane <- renderDT({
    req(input$mu1, input$mu2)
    tab <- plane_readout(c(input$mu1, input$mu2), lev_ab())
    datatable(tab, rownames = FALSE, escape = FALSE, class = "compact", options = dt_small) |>
      formatStyle("Codificacion", fontWeight = "700",
                  color = styleEqual(names(ENC_COL_BY_NAME), unname(ENC_COL_BY_NAME)))
  })
  
  # ---- Geometría · Panel B: 3D ---------------------------------------
  if (has_plotly) {
    output$plot_toy <- plotly::renderPlotly({
      req(input$toy_enc)
      show <- if (input$toy_enc == "all") ENCODING_TYPES else input$toy_enc
      plot_toy_3d(show = show)
    })
  }
  output$tbl_toy <- renderDT({
    datatable(toy_coordinates(), rownames = FALSE, class = "compact", options = dt_small) |>
      formatRound(2:3, 3) |>
      formatStyle("Codificacion", fontWeight = "700",
                  color = styleEqual(names(ENC_COL_BY_NAME), unname(ENC_COL_BY_NAME)))
  })
  
  # ---- Muchos niveles ------------------------------------------------
  output$ui_cov_note <- renderUI({
    d <- dat()
    if (is.null(d$x)) return(NULL)
    div(class = "warnbox",
        "Hay una covariable en el modelo: los coeficientes de nivel que ves aquí se evalúan en ",
        tags$b("x = x₀"), " (", if (identical(input$center, "mean")) "la media de x" else "0, sin centrar",
        "). Cambia el centrado en la barra lateral y verás que cambian; la pestaña ",
        "«Con covariable» lo explica con detalle.")
  })
  
  output$plot_heat <- renderPlot({
    plot_minv_heatmap(cmp()$fits, levels(dat()$g))
  }, res = 96, height = 460)
  
  output$tbl_involved <- renderDT({
    s <- involved_summary(cmp()$fits)
    names(s) <- c("Codificacion", "Mín. niveles por coef.", "Máx. niveles por coef.", "Promedio")
    datatable(s, rownames = FALSE, class = "compact", options = dt_small) |>
      formatStyle("Codificacion", fontWeight = "700",
                  color = styleEqual(names(ENC_COL_BY_NAME), unname(ENC_COL_BY_NAME)))
  })
  
  output$ui_pert <- renderUI({
    lev <- levels(dat()$g)
    tagList(
      selectInput("pert_level", "Nivel cuya media cambia", lev, selected = cur_ref()),
      sliderInput("pert_delta", "Cambio en la media (unidades de la respuesta)",
                  min = -5, max = 5, value = 1, step = 0.5)
    )
  })
  
  pert <- reactive({
    req(input$pert_level, input$pert_delta)
    perturbation_data(cmp()$fits, input$pert_level, input$pert_delta)
  })
  
  output$plot_pert <- renderPlot({
    plot_perturbation(pert(), input$pert_level, input$pert_delta)
  }, res = 96, height = 440)
  
  output$pert_text <- renderUI({
    p <- pert()
    K <- max(p$k) + 1
    cnt <- tapply(p$changed, p$tp, sum)[ENCODING_TYPES]
    div(class = "story",
        "Un único cambio real en ", tags$b(input$pert_level), " mueve: ",
        chip("dummy", paste0("Dummy ", cnt[["dummy"]], "/", K)),
        chip("effects", paste0("Efectos ", cnt[["effects"]], "/", K)),
        chip("helmert", paste0("Helmert ", cnt[["helmert"]], "/", K)),
        " coeficientes. Los coeficientes que se mueven son los que «se enteran» del cambio: ",
        "no puedes atribuirlo a un solo coeficiente sin saber qué comparación hace cada uno.")
  })
  
  output$plot_ci <- renderPlot({
    plot_coef_ci(cmp()$fits)
  }, res = 96, height = function() {
    K <- tryCatch(nlevels(dat()$g), error = function(e) 10)
    max(380, min(1100, 24 * K + 140))
  })
  
  output$ci_text <- renderUI({
    sc <- sig_counts(cmp()$fits)
    K <- cmp()$fits[[1]]$K - 1
    div(class = "story",
        "Con exactamente los mismos datos, el número de coeficientes con IC que excluye 0 es ",
        chip("dummy", paste0("Dummy ", sc[["dummy"]], "/", K)),
        chip("effects", paste0("Efectos ", sc[["effects"]], "/", K)),
        chip("helmert", paste0("Helmert ", sc[["helmert"]], "/", K)),
        ". No es contradicción: cada codificación pregunta algo distinto (contra la referencia, ",
        "contra el promedio, contra los niveles anteriores). Se excluye el intercepto.")
  })
}

shinyApp(ui, server)