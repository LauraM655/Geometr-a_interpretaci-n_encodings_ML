library(testthat)

test_that("columnas de M son los ejes: Minv %*% M[, i] = e_i", {
  for (tp in ENCODING_TYPES) {
    M <- coding_matrix(2, tp)
    expect_equal(unname(solve(M) %*% M), diag(2), tolerance = 1e-12)
    bv <- basis_vectors(M)
    expect_equal(nrow(bv), 2)
  }
})

test_that("los extremos de cada recta de nivel cumplen (Minv mu)_i = c", {
  for (tp in ENCODING_TYPES) {
    Minv <- solve(coding_matrix(2, tp))
    rownames(Minv) <- c("b0", "b1")
    L <- level_set_lines(Minv, cs = c(-1, 0, 2), lim = c(-4, 4))
    for (r in seq_len(nrow(L))) {
      i <- as.integer(substr(L$id[r], 1, 1))
      val <- sum(Minv[i, ] * c(L$x[r], L$y[r]))
      expect_equal(val, L$c[r], tolerance = 1e-9)
    }
  }
})

test_that("descripcion HTML usa entidades y fracciones", {
  set.seed(1)
  g <- factor(rep(LETTERS[1:5], each = 6)); y <- rnorm(30)
  fe <- fit_encoding(y, g, "effects")
  d_html <- describe_contrasts(fe, html = TRUE)
  expect_true(all(grepl("&mu;<sub>", d_html$Estima, fixed = TRUE)))
  expect_true(grepl("1/5", d_html$Estima[1], fixed = TRUE))
  d_txt <- describe_contrasts(fe)
  expect_true(grepl("mu[A]", d_txt$Estima[1], fixed = TRUE))
  expect_equal(frac_str(1), "")
  expect_equal(frac_str(-1 / 3), "1/3")
})

test_that("ejemplo de juguete: coordenadas conocidas y mismo y-hat", {
  tc <- toy_coordinates()
  expect_equal(unname(unlist(tc[1, 2:3])), c(3, 4))    # dummy: mu_A = 3, mu_B - mu_A = 4
  expect_equal(unname(unlist(tc[2, 2:3])), c(5, -2))   # efectos
  expect_equal(unname(unlist(tc[3, 2:3])), c(5, 2))    # Helmert
  cmp <- compare_encodings(c(2, 4, 7), factor(c("A", "A", "B")))
  expect_true(cmp$equal_fitted)
})

test_that("lectura del plano: dummy da (mu1, mu2 - mu1)", {
  r <- plane_readout(c(5, 8), c("A", "B"))
  expect_equal(nrow(r), 6)
  expect_equal(r$Valor[1:2], c(5, 3))
  expect_equal(r$Valor[3:4], c(6.5, -1.5))   # efectos: media y (mu1 - mu2)/2
  expect_equal(r$Valor[5:6], c(6.5, 1.5))    # Helmert
})

test_that("muchos niveles: cuantos coeficientes se mueven al cambiar un nivel", {
  set.seed(3)
  g <- factor(rep(LETTERS[1:6], each = 5)); y <- rnorm(30)
  fits <- compare_encodings(y, g)$fits
  K <- 6
  cnt <- function(level) tapply(perturbation_data(fits, level, 1)$changed,
                                perturbation_data(fits, level, 1)$tp, sum)
  expect_equal(unname(cnt("A")["dummy"]), K)        # la referencia mueve TODOS
  expect_equal(unname(cnt("C")["dummy"]), 1)        # un nivel normal mueve 1
  expect_equal(unname(cnt("C")["effects"]), K)      # efectos: todos
  expect_equal(unname(cnt("A")["helmert"]), K)      # Helmert: el primer nivel mueve todos
  expect_equal(unname(cnt("F")["helmert"]), 2)      # el ultimo mueve 2
  s <- involved_summary(fits)
  expect_equal(s$Maximo, c(2, K, K))
  expect_equal(s$Minimo, c(1, K, 2))
})

test_that("datos simulados", {
  d <- simulate_factor_data(K = 12, n_per = 8)
  expect_equal(nlevels(d$g), 12)
  expect_true(min(table(d$g)) >= 2)
  du <- simulate_factor_data(K = 12, n_per = 8, unbalanced = TRUE)
  expect_gt(max(table(du$g)) / min(table(du$g)), 3)
  expect_length(attr(d, "mu"), 12)
  expect_true(compare_encodings(d$y, d$g)$equal_fitted)
  for (nm in c("CO2", "ChickWeight")) {
    ex <- EXAMPLES[[nm]]; df <- get_example(nm)
    pd <- prepare_data(df, ex$y, ex$f, ex$x)
    expect_true(compare_encodings(pd$y, pd$g, x = pd$x)$equal_fitted)
  }
})

test_that("los graficos se construyen sin error", {
  skip_if_not_installed("ggplot2")
  p <- plot_mu_plane(c(5, 8), c("A", "B"))
  expect_s3_class(p, "ggplot")
  expect_error(ggplot2::ggplot_build(p), NA)
  p0 <- plot_mu_plane(c(0, 0), c("A", "B"))          # caso degenerado
  expect_error(ggplot2::ggplot_build(p0), NA)
  fits <- compare_encodings(mtcars$mpg, mtcars$cyl)$fits
  expect_error(ggplot2::ggplot_build(plot_minv_heatmap(fits, levels(factor(mtcars$cyl)))), NA)
  expect_error(ggplot2::ggplot_build(plot_perturbation(perturbation_data(fits, "4", 1), "4", 1)), NA)
  expect_error(ggplot2::ggplot_build(plot_coef_ci(fits)), NA)
  expect_error(ggplot2::ggplot_build(plot_mu_plane(c(5, 8), c("A", "B"), "helmert")), NA)
  skip_if_not_installed("plotly")
  expect_s3_class(plot_toy_3d(), "plotly")
  expect_s3_class(plot_toy_3d(show = "helmert"), "plotly")
})