# =====================================================================
# geometry_plots.R -- Graficos y textos didacticos (Pasos 2 y 3).
# Requiere ggplot2 (2D) y plotly (3D). Depende de core.R y geometry.R.
# Guardar y leer siempre como UTF-8.
# =====================================================================

wrap_txt <- function(x, width = 95) paste(strwrap(x, width), collapse = "\n")

# ---- Panel A: plano de medias (K = 2) --------------------------------

#' Plano (mu1, mu2) para una o varias codificaciones.
#' Elementos: cuadricula (rectas con beta_i constante), ejes oblicuos (columnas
#' de M), camino de coordenadas origen -> beta0 -> mu, y la recta mu1 = mu2.
plot_mu_plane <- function(mu, lev = c("A", "B"), types = ENCODING_TYPES) {
  stopifnot(length(mu) == 2, length(lev) == 2)
  lim <- range(c(0, mu))
  if (diff(lim) < 1e-9) lim <- c(-1, 1)
  pad <- 0.3 * diff(lim)
  lim <- c(lim[1] - pad, lim[2] + pad)
  step <- diff(pretty(lim, n = 6))[1]
  nn <- ceiling(3 * max(abs(lim)) / step)
  cs <- seq(-nn * step, nn * step, by = step)
  
  to_box <- function(d) {                       # t > 0 maximo con t*d dentro de la caja
    t <- numeric(0)
    for (k in 1:2) if (abs(d[k]) > 1e-12) t <- c(t, if (d[k] > 0) lim[2] / d[k] else lim[1] / d[k])
    min(t)
  }
  
  enc_levels <- unname(ENCODING_SHORT[types])
  Ls <- list(); Ax <- list(); Pa <- list(); Pt <- list()
  for (tp in types) {
    short <- ENCODING_SHORT[[tp]]
    M <- coding_matrix(2, tp)
    Minv <- solve(M)
    rownames(Minv) <- c("b0", "b1")
    beta <- drop(Minv %*% mu)
    
    L <- level_set_lines(Minv, cs, lim); L$enc <- short
    Ls[[tp]] <- L
    
    tmax <- 0.96 * c(to_box(M[, 1]), to_box(M[, 2]))
    Ax[[tp]] <- data.frame(enc = short, xe = tmax * c(M[1, 1], M[1, 2]),
                           ye = tmax * c(M[2, 1], M[2, 2]), lab = c("eje β₀", "eje β₁"))
    P1 <- beta[1] * M[, 1]
    Pa[[tp]] <- data.frame(enc = short, coef = c("b0", "b1"),
                           x0 = c(0, P1[1]), y0 = c(0, P1[2]),
                           x1 = c(P1[1], mu[1]), y1 = c(P1[2], mu[2]),
                           lab = c(sprintf("β₀ = %.2f", beta[1]), sprintf("β₁ = %.2f", beta[2])))
    Pt[[tp]] <- data.frame(enc = short, mu1 = mu[1], mu2 = mu[2])
  }
  Lall <- do.call(rbind, Ls); Aall <- do.call(rbind, Ax)
  Pall <- do.call(rbind, Pa); Qall <- do.call(rbind, Pt)
  Pall$mx <- (Pall$x0 + Pall$x1) / 2; Pall$my <- (Pall$y0 + Pall$y1) / 2
  for (nm in c("Lall", "Aall", "Pall", "Qall")) {
    d <- get(nm); d$enc <- factor(d$enc, levels = enc_levels); assign(nm, d)
  }
  
  cap <- wrap_txt(paste(
    "Líneas finas: cuadrícula (cada línea es un valor constante de β₀ o de β₁).",
    "Flechas gruesas: para llegar al punto desde el origen avanza β₀ sobre el eje β₀ y luego β₁ sobre el eje β₁.",
    "Discontinua roja: μ₁ = μ₂, es decir β₁ = 0 en todas las codificaciones."), 110)
  
  ggplot2::ggplot() +
    ggplot2::geom_line(data = Lall,
                       ggplot2::aes(x = x, y = y, group = interaction(enc, id), colour = coef),
                       linewidth = 0.3, alpha = 0.4) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                         colour = "#e4572e", linewidth = 0.7) +
    ggplot2::geom_segment(data = Aall,
                          ggplot2::aes(x = 0, y = 0, xend = xe, yend = ye),
                          colour = "#1f2a44", linewidth = 0.5, linetype = "dotted") +
    ggplot2::geom_text(data = Aall, ggplot2::aes(x = xe, y = ye, label = lab),
                       colour = "#1f2a44", size = 3.8, fontface = "italic", hjust = 1, vjust = -0.5) +
    ggplot2::geom_segment(data = Pall,
                          ggplot2::aes(x = x0, y = y0, xend = x1, yend = y1, colour = coef),
                          linewidth = 1.6, arrow = grid::arrow(length = grid::unit(0.28, "cm"), type = "closed")) +
    ggplot2::geom_label(data = Pall,
                        ggplot2::aes(x = mx, y = my, label = lab, colour = coef),
                        fill = "white", size = 4, fontface = "bold", show.legend = FALSE) +
    ggplot2::geom_point(data = Qall, ggplot2::aes(x = mu1, y = mu2),
                        colour = "#e4572e", size = 4) +
    ggplot2::facet_wrap(~enc, nrow = 1) +
    ggplot2::coord_fixed(xlim = lim, ylim = lim, expand = FALSE) +
    ggplot2::scale_colour_manual(
      values = c(b0 = "#2563eb", b1 = "#d97706"),
      labels = c(b0 = "β₀ (primer paso)", b1 = "β₁ (segundo paso)"), name = NULL) +
    ggplot2::labs(x = paste0("μ(", lev[1], ")"), y = paste0("μ(", lev[2], ")"), caption = cap) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(fill = NA, colour = "#cbd5e1"),
      strip.text = ggplot2::element_text(face = "bold", size = 15),
      legend.position = "bottom",
      plot.caption = ggplot2::element_text(colour = "#64748b", hjust = 0, size = 10))
}

#' Explicacion en palabras (HTML) de lo que significan beta0 y beta1
plane_story <- function(tp, mu, lev) {
  a <- esc_html(lev[1]); b <- esc_html(lev[2])
  beta <- drop(solve(coding_matrix(2, tp)) %*% mu)
  switch(tp,
         dummy = sprintf(paste0(
           "<b>Dummy.</b> β<sub>0</sub> = μ(%s) = %.2f: es la media del nivel de referencia. ",
           "β<sub>1</sub> = μ(%s) − μ(%s) = %.2f: cuánto está %s por encima (+) o por debajo (−) de %s."),
           a, beta[1], b, a, beta[2], b, a),
         effects = sprintf(paste0(
           "<b>Efectos.</b> β<sub>0</sub> = promedio de las dos medias = %.2f. ",
           "β<sub>1</sub> = μ(%s) − promedio = %.2f: cuánto se aparta %s del promedio. ",
           "El efecto de %s es −β<sub>1</sub> = %.2f (la restricción de suma cero lo fija; no es un coeficiente más)."),
           beta[1], a, beta[2], a, b, -beta[2]),
         helmert = sprintf(paste0(
           "<b>Helmert.</b> β<sub>0</sub> = promedio de las dos medias = %.2f. ",
           "β<sub>1</sub> = (μ(%s) − μ(%s)) / 2 = %.2f: la mitad de la diferencia entre %s y el nivel anterior (%s)."),
           beta[1], b, a, beta[2], b, a))
}

#' Coordenadas del punto mu en cada codificacion (K = 2), con su lectura (HTML)
plane_readout <- function(mu, lev) {
  rows <- lapply(ENCODING_TYPES, function(tp) {
    M <- coding_matrix(2, tp)
    Minv <- solve(M)
    levs <- c(lev[1], lev[2])
    fk <- list(Minv = Minv, K = 2, type = tp, levels = levs,
               labels = coef_labels(levs, tp))
    d <- describe_contrasts(fk, html = TRUE)
    data.frame(Codificacion = ENCODING_NAMES[[tp]],
               Coeficiente = c("β₀", "β₁"),
               Estima = d$Estima,
               Valor = round(drop(Minv %*% mu), 3),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# ---- Panel B: espacio de observaciones (n = 3, K = 2) ----------------

#' y en R^3, el plano C(X) y, para cada codificacion, el CAMINO
#' origen -> beta0 * col1 -> beta0 * col1 + beta1 * col2 = y-hat.
plot_toy_3d <- function(y = c(2, 4, 7), g = c("A", "A", "B"), show = ENCODING_TYPES) {
  stopifnot(length(y) == 3, length(g) == 3)
  g <- factor(g)
  stopifnot(nlevels(g) == 2)
  yhat <- fit_encoding(y, g, "dummy")$fitted
  ind_a <- as.numeric(g == levels(g)[1])
  ind_b <- as.numeric(g == levels(g)[2])
  
  lo <- -1; hi <- 9
  pq <- rbind(c(lo, lo), c(hi, lo), c(hi, hi), c(lo, hi))
  V <- t(sapply(seq_len(4), function(r) pq[r, 1] * ind_a + pq[r, 2] * ind_b))
  sub3 <- c("₁", "₂", "₃")
  ax <- paste0("y", sub3, " (", as.character(g), ")")
  
  p <- plotly::plot_ly()
  p <- plotly::add_trace(p, type = "mesh3d", x = V[, 1], y = V[, 2], z = V[, 3],
                         i = c(0L, 0L), j = c(1L, 2L), k = c(2L, 3L), opacity = 0.22,
                         color = I("#0f766e"), name = "Plano C(X): todas las predicciones posibles",
                         showlegend = TRUE, hoverinfo = "name")
  
  # direccion de la columna 1 (intercepto), comun a las tres codificaciones
  p <- plotly::add_trace(p, type = "scatter3d", mode = "lines",
                         x = c(0, 8), y = c(0, 8), z = c(0, 8),
                         line = list(color = "#94a3b8", width = 3, dash = "dot"),
                         name = "dirección de la columna 1 (intercepto)", hoverinfo = "name")
  
  for (tp in show) {
    f <- fit_encoding(y, g, tp)
    Z <- coding_matrix(2, tp)[as.integer(g), , drop = FALSE]
    b <- unname(f$coefficients)
    P1 <- b[1] * Z[, 1]
    col <- ENC_COL[[tp]]
    # direccion de la columna 2 de esta codificacion
    p <- plotly::add_trace(p, type = "scatter3d", mode = "lines",
                           x = c(-2, 2) * Z[1, 2] + 0, y = c(-2, 2) * Z[2, 2], z = c(-2, 2) * Z[3, 2],
                           line = list(color = col, width = 2, dash = "dot"),
                           name = paste0("dirección col. 2 (", ENCODING_SHORT[[tp]], ")"),
                           hoverinfo = "name", showlegend = FALSE)
    # camino de coordenadas
    p <- plotly::add_trace(p, type = "scatter3d", mode = "lines+markers",
                           x = c(0, P1[1], yhat[1]), y = c(0, P1[2], yhat[2]), z = c(0, P1[3], yhat[3]),
                           line = list(color = col, width = 8),
                           marker = list(size = c(0.1, 5, 5), color = col),
                           name = sprintf("%s: ŷ = %.1f·col.1 + %.1f·col.2", ENCODING_SHORT[[tp]], b[1], b[2]),
                           hoverinfo = "name")
  }
  
  p <- plotly::add_trace(p, type = "scatter3d", mode = "lines",
                         x = c(y[1], yhat[1]), y = c(y[2], yhat[2]), z = c(y[3], yhat[3]),
                         line = list(color = "#e4572e", width = 5, dash = "dash"),
                         name = "residuo e = y − ŷ (⊥ al plano)", hoverinfo = "name")
  p <- plotly::add_trace(p, type = "scatter3d", mode = "markers+text",
                         x = c(y[1], yhat[1]), y = c(y[2], yhat[2]), z = c(y[3], yhat[3]),
                         text = c("y", "ŷ"), textposition = "top center", textfont = list(size = 16),
                         marker = list(size = 7, color = c("#1f2a44", "#e4572e")),
                         name = "y y su proyección ŷ", hoverinfo = "text")
  
  plotly::layout(p,
                 scene = list(
                   xaxis = list(title = ax[1], range = c(-2, 9)),
                   yaxis = list(title = ax[2], range = c(-2, 9)),
                   zaxis = list(title = ax[3], range = c(-2, 9)),
                   aspectmode = "cube"),
                 legend = list(orientation = "h", y = -0.05),
                 margin = list(l = 0, r = 0, b = 0, t = 10))
}

# ---- Muchos niveles --------------------------------------------------

lab_frac <- function(w) {
  if (abs(w) < 1e-10) return("")
  s <- frac_str(w); if (s == "") s <- "1"
  paste0(if (w < 0) "−" else "", s)
}

#' Mapa de calor de Minv: que niveles mezcla cada coeficiente.
#' `fits`: lista nombrada de fit_encoding; `orig_levels`: orden de niveles a mostrar.
plot_minv_heatmap <- function(fits, orig_levels) {
  K <- length(orig_levels)
  rows <- lapply(names(fits), function(tp) {
    f <- fits[[tp]]
    Mi <- f$Minv[, match(orig_levels, f$levels), drop = FALSE]
    data.frame(enc = ENCODING_SHORT[[tp]], k = rep(0:(K - 1), times = K),
               lev = factor(rep(orig_levels, each = K), levels = orig_levels),
               w = as.vector(Mi), stringsAsFactors = FALSE)
  })
  d <- do.call(rbind, rows)
  d$enc <- factor(d$enc, levels = unname(ENCODING_SHORT[names(fits)]))
  brk <- if (K <= 20) 0:(K - 1) else seq(0, K - 1, by = 5)
  
  p <- ggplot2::ggplot(d, ggplot2::aes(x = lev, y = k, fill = w)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.3)
  if (K <= 8) {
    d$txt <- vapply(d$w, lab_frac, character(1))
    p <- p + ggplot2::geom_text(data = d, ggplot2::aes(label = txt), size = 3.6)
  }
  p +
    ggplot2::facet_wrap(~enc, nrow = 1) +
    ggplot2::scale_fill_gradient2(low = "#e4572e", mid = "#f1f5f9", high = "#0f766e",
                                  midpoint = 0, limits = c(-1, 1), name = "peso") +
    ggplot2::scale_y_reverse(breaks = brk, labels = paste0("β", brk)) +
    ggplot2::labs(x = "Niveles del factor", y = "Coeficiente",
                  caption = "Cada fila = un coeficiente. Las celdas con color son los niveles cuyas medias entran en la comparación.") +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 14),
      axis.text.x = ggplot2::element_text(angle = if (K > 8) 90 else 0, vjust = 0.5,
                                          hjust = if (K > 8) 1 else 0.5,
                                          size = if (K > 25) 7 else 10),
      plot.caption = ggplot2::element_text(colour = "#64748b", hjust = 0))
}

#' Un solo cambio real, muchos coeficientes que se mueven
plot_perturbation <- function(d, level, delta) {
  tot <- ave(as.numeric(d$changed), d$enc, FUN = sum)
  n <- ave(as.numeric(d$changed), d$enc, FUN = length)
  d$lab <- paste0(d$enc, "\n", tot, " de ", n, " coeficientes cambian")
  d$lab <- factor(d$lab, levels = unique(d$lab))
  K <- max(d$k) + 1
  brk <- if (K <= 20) 0:(K - 1) else seq(0, K - 1, by = 5)
  ggplot2::ggplot(d, ggplot2::aes(x = k, y = value, fill = changed)) +
    ggplot2::geom_col(width = 0.8) +
    ggplot2::geom_hline(yintercept = 0, colour = "#64748b") +
    ggplot2::facet_wrap(~lab, nrow = 1) +
    ggplot2::scale_fill_manual(values = c(`TRUE` = "#e4572e", `FALSE` = "#cbd5e1"), guide = "none") +
    ggplot2::scale_x_continuous(breaks = brk, labels = paste0("β", brk)) +
    ggplot2::labs(x = "Coeficiente", y = "Cambio en el coeficiente",
                  title = sprintf("Si SOLO la media del nivel %s cambia en %+g", level, delta),
                  subtitle = "En rojo: coeficientes que se mueven por ese único cambio") +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 13),
      axis.text.x = ggplot2::element_text(angle = if (K > 12) 90 else 0, vjust = 0.5),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(colour = "#64748b"))
}

#' Mismos datos, tres lecturas: estimaciones con IC (sin el intercepto)
#' y recuento de coeficientes "significativos" por codificacion.
sig_counts <- function(fits, alpha = 0.05) {
  vapply(fits, function(f) sum(f$p_value[2:f$K] < alpha), numeric(1))
}

plot_coef_ci <- function(fits, alpha = 0.05) {
  rows <- lapply(names(fits), function(tp) {
    f <- fits[[tp]]; idx <- 2:f$K
    est <- unname(f$coefficients[idx]); se <- unname(f$se[idx])
    q <- qt(1 - alpha / 2, f$df)
    data.frame(short = ENCODING_SHORT[[tp]], k = idx - 1, est = est,
               lo = est - q * se, hi = est + q * se,
               sig = unname(f$p_value[idx]) < alpha, stringsAsFactors = FALSE)
  })
  d <- do.call(rbind, rows)
  d$lab <- paste0(d$short, "\n", ave(as.numeric(d$sig), d$short, FUN = sum), " de ",
                  ave(as.numeric(d$sig), d$short, FUN = length), " con IC que excluye 0")
  d$lab <- factor(d$lab, levels = unique(d$lab))
  ks <- sort(unique(d$k))
  brk <- if (length(ks) <= 25) ks else ks[seq(1, length(ks), by = 5)]
  ggplot2::ggplot(d, ggplot2::aes(y = k, colour = sig)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "#94a3b8") +
    ggplot2::geom_segment(ggplot2::aes(x = lo, xend = hi, yend = k), linewidth = 0.8) +
    ggplot2::geom_point(ggplot2::aes(x = est), size = 2.2) +
    ggplot2::facet_wrap(~lab, nrow = 1, scales = "free_x") +
    ggplot2::scale_y_reverse(breaks = brk, labels = paste0("β", brk)) +
    ggplot2::scale_colour_manual(values = c(`TRUE` = "#e4572e", `FALSE` = "#94a3b8"),
                                 labels = c(`TRUE` = "IC excluye 0", `FALSE` = "IC incluye 0"), name = NULL) +
    ggplot2::labs(x = "Estimación e IC 95 %", y = "Coeficiente") +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 13),
      legend.position = "bottom")
}