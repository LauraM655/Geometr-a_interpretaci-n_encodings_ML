# =====================================================================
# geometry.R -- Matematica de la vista geometrica (Paso 2). ASCII puro.
#
# Espacio de medias (K = 2): el punto mu = (mu_1, mu_2).
# Con mu = M beta:
#   * la COLUMNA i de M es la direccion del eje del coeficiente i
#     (mover beta_i una unidad desplaza mu en esa direccion);
#   * la FILA i de M^{-1} es la normal de las rectas de nivel de beta_i:
#     {mu : (M^{-1} mu)_i = c}.
# Cada codificacion es un sistema de coordenadas (oblicuo) distinto
# sobre el MISMO plano de medias.
# =====================================================================

#' Direcciones de los ejes coordenados: columnas de M (K = 2)
basis_vectors <- function(M) {
  stopifnot(nrow(M) == 2, ncol(M) == 2)
  data.frame(coef = if (is.null(colnames(M))) paste0("b", 0:1) else colnames(M),
             mu1 = M[1, ], mu2 = M[2, ], row.names = NULL)
}

#' Rectas de nivel de cada coeficiente dentro de la caja `lim` (K = 2).
#' Devuelve 2 filas (extremos) por recta: coef, c, id, x, y.
level_set_lines <- function(Minv, cs = seq(-2, 2, by = 0.5), lim = c(-5, 5)) {
  stopifnot(nrow(Minv) == 2, ncol(Minv) == 2)
  nm <- if (is.null(rownames(Minv))) paste0("b", 0:1) else rownames(Minv)
  out <- list()
  for (i in 1:2) {
    w <- Minv[i, ]
    for (cc in cs) {
      if (abs(w[2]) > 1e-12) {
        x <- lim
        y <- (cc - w[1] * x) / w[2]
      } else {
        x <- rep(cc / w[1], 2)
        y <- lim
      }
      out[[length(out) + 1]] <- data.frame(
        coef = nm[i], c = cc, id = paste0(i, "_", cc), x = x, y = y)
    }
  }
  do.call(rbind, out)
}

#' Ejemplo de juguete (n = 3, K = 2): coordenadas de y-hat en cada codificacion.
#' y = (2, 4, 7), g = (A, A, B)  =>  y-hat = (3, 3, 7), e = (-1, 1, 0).
toy_coordinates <- function(y = c(2, 4, 7), g = c("A", "A", "B")) {
  g <- factor(g)
  rows <- lapply(ENCODING_TYPES, function(tp) {
    f <- fit_encoding(y, g, tp)
    data.frame(Codificacion = ENCODING_NAMES[[tp]],
               b0 = unname(f$coefficients[1]), b1 = unname(f$coefficients[2]),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  names(out)[2:3] <- c("β₀", "β₁")
  out
}

# ---- Muchos niveles: que tan "enredada" es cada codificacion ----------

#' Numero de niveles que involucra cada coeficiente (no ceros por fila de Minv)
levels_involved <- function(Minv, tol = 1e-10) rowSums(abs(Minv) > tol)

#' Resumen por codificacion de cuantos niveles mezcla cada coeficiente
involved_summary <- function(fits) {
  do.call(rbind, lapply(names(fits), function(tp) {
    n <- levels_involved(fits[[tp]]$Minv)
    data.frame(Codificacion = ENCODING_NAMES[[tp]], Minimo = min(n), Maximo = max(n),
               Promedio = round(mean(n), 1), stringsAsFactors = FALSE)
  }))
}

#' Cambio en cada coeficiente si SOLO la media del nivel `level` cambia en `delta`:
#' delta_beta = delta * (columna de Minv de ese nivel).
perturbation_data <- function(fits, level, delta = 1) {
  do.call(rbind, lapply(names(fits), function(tp) {
    f <- fits[[tp]]
    jj <- match(level, f$levels)
    if (is.na(jj)) stop("Nivel desconocido: ", level)
    w <- drop(f$Minv[, jj]) * delta
    data.frame(tp = tp, enc = ENCODING_SHORT[[tp]], k = seq_along(w) - 1,
               value = unname(w), changed = abs(w) > 1e-10, stringsAsFactors = FALSE)
  }))
}