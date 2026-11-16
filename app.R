# =====================================================================
# app.R -- Codificacion de factores: geometria e interpretacion (Paso 1)
# Ejecutar:  shiny::runApp("ruta/a/encodings_app")
# Paquetes:  install.packages(c("shiny", "bslib", "DT"))
# =====================================================================
library(shiny)
library(bslib)
library(DT)

source("R/core.R", local = TRUE)

placeholder <- function(titulo, texto) {
  card(card_header(titulo),
       div(class = "text-muted p-3", texto))
}

ui <- page_navbar(
  title = "Codificacion de factores: geometria e interpretacion",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  sidebar = sidebar(
    width = 330,
    selectInput("dataset", "Datos",
                c("iris", "mtcars", "ToothGrowth", "Subir CSV")),
    conditionalPanel(
      "input.dataset == 'Subir CSV'",
      fileInput("file", "Archivo CSV", accept = c(".csv", ".txt")),
      checkboxInput("csv_semicolon", "Separador ';' (decimal ',')", FALSE)
    ),
    uiOutput("ui_vars"),
    hr(),
    selectInput("encoding", "Codificacion", setNames(ENCODING_TYPES, ENCODING_NAMES[ENCODING_TYPES])),
    uiOutput("ui_ref"),
    conditionalPanel(
      "input.xvar != ''",
      radioButtons("center", "Covariable",
                   c("Sin centrar (x = 0)" = "zero", "Centrada en su media" = "mean"))
    )
  ),

  nav_panel("Datos",
    layout_columns(
      col_widths = c(7, 5),
      card(card_header("Vista previa"), DTOutput("tbl_data")),
      card(card_header("Diagnostico del factor"), verbatimTextOutput("diag"),
           plotOutput("plot_groups", height = 280))
    )
  ),

  nav_panel("Coeficientes",
    card(card_header("Estimaciones con la codificacion elegida"),
         DTOutput("tbl_coef")),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Que estima cada coeficiente (filas de M^{-1})"),
           DTOutput("tbl_interp")),
      card(card_header("Matriz M^{-1}: beta = M^{-1} mu"),
           DTOutput("tbl_minv"))
    )
  ),

  nav_panel("Equivalencia",
    card(card_header("Mismas predicciones, distintos coeficientes"),
         uiOutput("equiv_badge"),
         DTOutput("tbl_equiv")),
    card(card_header("Coeficientes lado a lado"),
         DTOutput("tbl_side"))
  ),

  nav_panel("Geometria",
    placeholder("Paso 2", "Vista geometrica: proyeccion de y sobre el espacio columna (mismo subespacio, distintas bases).")),
  nav_panel("Con covariable",
    placeholder("Paso 4", "Interaccion factor x covariable: efecto del centrado sobre la interpretacion.")),
  nav_panel("Simulacion",
    placeholder("Paso 5", "Confusion de efectos y target encoding con fuga (simulacion bajo demanda)."))
)

server <- function(input, output, session) {

  # ---- datos --------------------------------------------------------
  raw <- reactive({
    if (input$dataset == "Subir CSV") {
      req(input$file)
      if (isTRUE(input$csv_semicolon)) {
        read.csv2(input$file$datapath, stringsAsFactors = FALSE)
      } else {
        read.csv(input$file$datapath, stringsAsFactors = FALSE)
      }
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
      selectInput("yvar", "Respuesta (numerica)", nms[is_num], selected = ysel),
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
    lab <- if (identical(input$encoding, "effects")) "Nivel omitido (efecto = -suma de los demas)"
           else "Nivel de referencia"
    selectInput("ref", lab, lev,
                selected = if (identical(input$encoding, "effects")) lev[length(lev)] else lev[1])
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

  # ---- pestana Datos -------------------------------------------------
  output$tbl_data <- renderDT(
    datatable(head(raw(), 200), options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  )
  output$diag <- renderText({
    d <- dat()
    paste(diagnose_factor(d$g, has_x = !is.null(d$x)), collapse = "\n\n")
  })
  output$plot_groups <- renderPlot({
    d <- dat()
    op <- par(mar = c(4, 4, 1, 1)); on.exit(par(op))
    boxplot(d$y ~ d$g, xlab = d$fname, ylab = d$yname, col = "grey90", border = "grey40")
    stripchart(d$y ~ d$g, vertical = TRUE, method = "jitter", pch = 16,
               col = adjustcolor("steelblue", 0.6), add = TRUE)
    points(seq_len(nlevels(d$g)), tapply(d$y, d$g, mean), pch = 18, col = "red", cex = 1.8)
  })

  # ---- pestana Coeficientes ------------------------------------------
  output$tbl_coef <- renderDT(
    datatable(coef_table(fit()), rownames = FALSE, options = list(pageLength = 15, dom = "tip"))
  )
  output$tbl_interp <- renderDT(
    datatable(describe_contrasts(fit()), rownames = FALSE,
              options = list(pageLength = 10, dom = "tip", scrollX = TRUE))
  )
  output$tbl_minv <- renderDT({
    f <- fit()
    m <- as.data.frame(round(f$Minv, 3))
    colnames(m) <- paste0("mu[", f$levels, "]")
    datatable(m, options = list(pageLength = 10, dom = "tip", scrollX = TRUE))
  })

  # ---- pestana Equivalencia ------------------------------------------
  output$equiv_badge <- renderUI({
    cc <- cmp()
    if (cc$equal_fitted && cc$equal_sigma2) {
      div(class = "alert alert-success",
          "Los valores ajustados y sigma^2 coinciden en las tres codificaciones (tolerancia numerica ",
          format(cc$tol, digits = 2), "). Solo cambian los coeficientes.")
    } else {
      div(class = "alert alert-danger", "Las predicciones NO coinciden: revisar el ajuste.")
    }
  })
  output$tbl_equiv <- renderDT({
    s <- cmp()$summary
    s$RSS <- round(s$RSS, 6); s$sigma2 <- round(s$sigma2, 6)
    s$max_dif_ajustados <- signif(s$max_dif_ajustados, 3)
    datatable(s, rownames = FALSE, options = list(dom = "t"))
  })
  output$tbl_side <- renderDT({
    fits <- cmp()$fits
    K <- fits[[1]]$K
    nm <- if (fits[[1]]$has_x) c(paste0("coef ", seq_len(K)), paste0("pend. ", seq_len(K)))
          else paste0("coef ", seq_len(K))
    side <- data.frame(Parametro = nm)
    for (tp in names(fits)) side[[ENCODING_NAMES[[tp]]]] <- round(unname(fits[[tp]]$coefficients), 4)
    datatable(side, rownames = FALSE, options = list(pageLength = 15, dom = "tip"))
  })
}

shinyApp(ui, server)
