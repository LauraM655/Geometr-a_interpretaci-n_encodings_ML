# =====================================================================
# core.R  --  Nucleo estadistico de la app (Paso 1)
# Codificacion de un factor en un modelo lineal: dummy, efectos, Helmert.
#
# Teoria (resumen):
#   Un factor con K niveles y medias de nivel mu = (mu_1,...,mu_K)'.
#   Una codificacion es una matriz M = [1 | C] de K x K (invertible):
#       mu = M %*% beta   <=>   beta = M^{-1} %*% mu
#   Cada FILA de M^{-1} dice QUE comparacion de medias estima ese coeficiente.
#   Distintas M generan el MISMO espacio columna => mismos valores ajustados,
#   mismos residuos y mismo sigma^2; solo cambian las coordenadas beta.
#   Var(beta_hat) = sigma^2 * M^{-1} diag(1/n_g) M^{-T}   (sin covariable)
#
# Con covariable continua x e interaccion se usa  X = [Z | Z * (x - x0)],
#   Z = M[g, ].  El primer bloque (K coef.) es M^{-1} alpha  (nivel en x = x0)
#   y el segundo bloque es M^{-1} gamma  (pendientes). Centrar x (cambiar x0)
#   cambia el primer bloque pero NO el segundo ni los valores ajustados.
# =====================================================================

ENCODING_TYPES <- c("dummy", "effects", "helmert")
ENCODING_NAMES <- c(dummy   = "Dummy (nivel de referencia)",
                    effects = "Efectos (suma cero)",
                    helmert = "Helmert")

# ---- 1. Matrices de codificacion -------------------------------------

#' Matriz de contrastes C (K x (K-1)); coincide con contr.treatment,
#' contr.sum y contr.helmert de R cuando los niveles estan en el orden dado.
contrast_matrix <- function(K, type = c("dummy", "effects", "helmert")) {
  type <- match.arg(type)
  if (K < 2) stop("Se necesitan al menos 2 niveles.")
  if (type == "dummy") {
    return(diag(K)[, -1, drop = FALSE])
  }
  if (type == "effects") {
    return(rbind(diag(K - 1), rep(-1, K - 1)))
  }
  cols <- lapply(seq_len(K - 1), function(j) c(rep(-1, j), j, rep(0, K - j - 1)))
  do.call(cbind, cols)
}

#' Matriz de codificacion completa M = [1 | C]  (K x K, invertible)
coding_matrix <- function(K, type) {
  cbind(1, contrast_matrix(K, type))
}

#' Etiquetas de los K coeficientes de un bloque
coef_labels <- function(lev, type) {
  K <- length(lev)
  rest <- switch(type,
    dummy   = paste0(lev[-1], " vs ", lev[1]),
    effects = paste0(lev[-K], " vs media"),
    helmert = vapply(seq_len(K - 1), function(j) {
      prev <- if (j == 1) lev[1]
              else if (j <= 3) paste0("prom(", paste(lev[1:j], collapse = ","), ")")
              else paste0("prom(", lev[1], "..", lev[j], ")")
      paste0(lev[j + 1], " vs ", prev)
    }, character(1))
  )
  c("(Intercepto)", rest)
}

# ---- 2. Ajuste -------------------------------------------------------

#' Ajusta  y ~ g  (o  y ~ g * x ) con la codificacion `type`.
#'
#' @param y numerico. @param g factor/caracter. @param type dummy|effects|helmert
#' @param ref dummy/helmert: nivel de referencia (queda primero).
#'            effects: nivel OMITIDO (su efecto = -suma de los demas).
#'            Por defecto: primer nivel (dummy, helmert) / ultimo (effects).
#' @param x covariable numerica opcional (interaccion completa con g).
#' @param x_center valor x0 en el que se centra x (0 = sin centrar).
#' @return lista con coeficientes, EE, vcov, M, Minv, ajustados, sigma2, etc.
fit_encoding <- function(y, g, type = c("dummy", "effects", "helmert"),
                         ref = NULL, x = NULL, x_center = 0) {
  type <- match.arg(type)
  g <- droplevels(as.factor(g))
  lev0 <- levels(g)
  if (length(lev0) < 2) stop("El factor necesita al menos 2 niveles.")
  if (is.null(ref)) ref <- if (type == "effects") lev0[length(lev0)] else lev0[1]
  if (!ref %in% lev0) stop("El nivel de referencia no esta en el factor.")

  lev <- if (type == "effects") c(setdiff(lev0, ref), ref) else c(ref, setdiff(lev0, ref))
  g <- factor(g, levels = lev)
  n <- length(y)
  K <- length(lev)

  M    <- coding_matrix(K, type)
  Minv <- solve(M)
  Z    <- M[as.integer(g), , drop = FALSE]

  has_x <- !is.null(x)
  X <- if (has_x) cbind(Z, Z * (x - x_center)) else Z
  p <- ncol(X)
  if (n <= p) stop("Hay mas parametros (", p, ") que observaciones (", n, ").")
  if (qr(X)$rank < p) {
    stop("Matriz de diseno singular: hay niveles con muy pocas observaciones ",
         "(con covariable cada nivel necesita al menos 2 con x distinto).")
  }

  XtXi  <- solve(crossprod(X))
  beta  <- drop(XtXi %*% crossprod(X, y))
  fit   <- drop(X %*% beta)
  res   <- y - fit
  dfres <- n - p
  sigma2 <- sum(res^2) / dfres
  V     <- sigma2 * XtXi
  se    <- sqrt(diag(V))
  tval  <- beta / se
  pval  <- 2 * pt(abs(tval), dfres, lower.tail = FALSE)

  labs <- coef_labels(lev, type)
  nm <- if (has_x) c(labs, paste0("pend.: ", labs)) else labs
  names(beta) <- names(se) <- names(tval) <- names(pval) <- nm
  dimnames(V) <- list(nm, nm)
  dimnames(Minv) <- list(labs, lev)
  dimnames(M)    <- list(lev, labs)

  n_g <- as.vector(table(g))
  out <- list(
    type = type, ref = ref, levels = lev, K = K, n = n, p = p, df = dfres,
    M = M, Minv = Minv, n_g = n_g, has_x = has_x, x_center = x_center,
    coefficients = beta, se = se, t = tval, p_value = pval, vcov = V,
    fitted = fit, resid = res, sigma2 = sigma2, rss = sum(res^2),
    labels = labs
  )
  if (!has_x) {
    out$group_means <- setNames(as.numeric(tapply(y, g, mean)), lev)
  } else {
    alpha <- drop(M %*% beta[seq_len(K)])
    gamma <- drop(M %*% beta[K + seq_len(K)])
    out$group_params <- data.frame(nivel = lev, n = n_g,
                                   intercepto_en_x0 = alpha, pendiente = gamma,
                                   row.names = NULL)
  }
  out
}

#' Tabla de coeficientes lista para mostrar
coef_table <- function(fit, digits = 4) {
  data.frame(
    Coeficiente = names(fit$coefficients),
    Estimacion  = round(fit$coefficients, digits),
    EE          = round(fit$se, digits),
    t           = round(fit$t, 3),
    p           = signif(fit$p_value, 3),
    row.names   = NULL, check.names = FALSE
  )
}

# ---- 3. Interpretacion: filas de M^{-1} ------------------------------

#' Para cada coeficiente: la combinacion lineal de medias que estima
#' (fila de M^{-1}) y su lectura en palabras.
describe_contrasts <- function(fit, digits = 3) {
  Minv <- fit$Minv; lev <- fit$levels; K <- fit$K; type <- fit$type
  formula <- vapply(seq_len(K), function(i) {
    w <- Minv[i, ]
    keep <- which(abs(w) > 1e-10)
    paste0(sprintf("%+.*f", digits, w[keep]), "*mu[", lev[keep], "]", collapse = " ")
  }, character(1))

  lectura <- switch(type,
    dummy = c(paste0("Media del nivel de referencia (", lev[1], ")"),
              paste0(lev[-1], " menos ", lev[1])),
    effects = c("Promedio NO ponderado de las K medias de nivel",
                paste0(lev[-K], " menos el promedio de niveles")),
    helmert = c("Promedio NO ponderado de las K medias de nivel",
                vapply(seq_len(K - 1), function(j) {
                  paste0("(", lev[j + 1], " menos el promedio de ",
                         paste(lev[1:j], collapse = ", "), ") / ", j + 1)
                }, character(1)))
  )
  data.frame(Coeficiente = fit$labels, Estima = formula, Lectura = lectura,
             row.names = NULL, check.names = FALSE)
}

# ---- 4. Equivalencia entre codificaciones ----------------------------

#' Ajusta las tres codificaciones y verifica que las predicciones coinciden.
compare_encodings <- function(y, g, x = NULL, x_center = 0,
                              types = ENCODING_TYPES, ref = NULL) {
  fits <- lapply(types, function(tp) {
    r <- if (tp == "effects") NULL else ref   # 'ref' solo aplica a dummy/Helmert
    fit_encoding(y, g, type = tp, ref = r, x = x, x_center = x_center)
  })
  names(fits) <- types
  Fm <- sapply(fits, function(f) f$fitted)
  max_diff <- apply(Fm, 2, function(col) max(abs(col - Fm[, 1])))
  tol <- 1e-8 * max(1, max(abs(y)))
  summ <- data.frame(
    Codificacion = unname(ENCODING_NAMES[types]),
    Parametros   = vapply(fits, function(f) f$p, numeric(1)),
    RSS          = vapply(fits, function(f) f$rss, numeric(1)),
    sigma2       = vapply(fits, function(f) f$sigma2, numeric(1)),
    max_dif_ajustados = max_diff,
    row.names = NULL
  )
  sig <- summ$sigma2
  list(fits = fits, summary = summ, tol = tol,
       equal_fitted = all(max_diff < tol),
       equal_sigma2 = (max(sig) - min(sig)) < 1e-8 * max(1, max(sig)))
}

# ---- 5. Datos --------------------------------------------------------

#' Valores por defecto de los ejemplos incluidos
EXAMPLES <- list(
  iris        = list(y = "Sepal.Length", f = "Species", x = "Petal.Length"),
  mtcars      = list(y = "mpg",          f = "cyl",     x = "wt"),
  ToothGrowth = list(y = "len",          f = "dose",    x = "")
)

get_example <- function(name) {
  switch(name, iris = iris, mtcars = mtcars, ToothGrowth = ToothGrowth,
         stop("Ejemplo desconocido"))
}

#' Limpia y valida: casos completos, y numerico, g factor, x numerico
prepare_data <- function(df, yname, fname, xname = NULL, max_levels = 60) {
  if (is.null(xname) || identical(xname, "")) xname <- NULL
  vars <- c(yname, fname, xname)
  if (!all(vars %in% names(df))) stop("Variables no encontradas en los datos.")
  if (anyDuplicated(vars)) stop("Respuesta, factor y covariable deben ser distintas.")
  d <- df[stats::complete.cases(df[, vars, drop = FALSE]), vars, drop = FALSE]
  if (!is.numeric(d[[yname]])) stop("La respuesta debe ser numerica.")
  g <- droplevels(as.factor(d[[fname]]))
  if (nlevels(g) < 2) stop("El factor tiene menos de 2 niveles.")
  if (nlevels(g) > max_levels) stop("El factor tiene ", nlevels(g), " niveles (maximo ", max_levels, ").")
  x <- NULL
  if (!is.null(xname)) {
    if (!is.numeric(d[[xname]])) stop("La covariable debe ser numerica.")
    x <- d[[xname]]
  }
  list(y = d[[yname]], g = g, x = x, n = nrow(d), yname = yname, fname = fname, xname = xname)
}

#' Diagnostico basico del factor (en pasos siguientes: recomendaciones)
diagnose_factor <- function(g, has_x = FALSE) {
  tab <- table(g); N <- sum(tab); K <- length(tab)
  msgs <- c(sprintf("N = %d, K = %d niveles, N/K = %.1f. n por nivel: min %d, mediana %.0f, max %d.",
                    N, K, N / K, min(tab), stats::median(tab), max(tab)))
  if (min(tab) < 2)
    msgs <- c(msgs, "Hay niveles con una sola observacion: no se puede estimar la varianza de ese nivel y, con covariable, el modelo con interaccion es singular.")
  if (has_x && min(tab) < 3)
    msgs <- c(msgs, "Con interaccion, niveles con menos de 3 observaciones dan pendientes muy inestables.")
  if (max(tab) / min(tab) > 3)
    msgs <- c(msgs, "Diseno desbalanceado: los coeficientes de Helmert/efectos dejan de ser independientes y los promedios 'no ponderados' difieren de la media muestral.")
  if (N / K < 10)
    msgs <- c(msgs, "N/K < 10: con alta cardinalidad conviene considerar contraccion (efectos aleatorios / target encoding regularizado); ver la pestana de simulacion (paso 4).")
  msgs
}
