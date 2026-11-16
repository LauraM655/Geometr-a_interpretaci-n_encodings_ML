library(testthat)

make_unbalanced <- function(seed = 1) {
  set.seed(seed)
  n_g <- c(3, 8, 15, 30, 5)
  g <- factor(rep(LETTERS[1:5], n_g))
  mu <- c(10, 12, 9, 15, 11)
  x <- rnorm(sum(n_g), 5, 2)
  y <- mu[as.integer(g)] + 0.8 * x + rnorm(sum(n_g))
  list(y = y, g = g, x = x)
}

test_that("las tres codificaciones dan los mismos ajustados y sigma2 (sin covariable)", {
  for (d in list(make_unbalanced(),
                 list(y = iris$Sepal.Length, g = iris$Species, x = NULL))) {
    cmp <- compare_encodings(d$y, d$g)
    expect_true(cmp$equal_fitted)
    expect_true(cmp$equal_sigma2)
  }
})

test_that("las tres codificaciones coinciden con covariable e interaccion", {
  d <- make_unbalanced()
  for (x0 in c(0, mean(d$x))) {
    cmp <- compare_encodings(d$y, d$g, x = d$x, x_center = x0)
    expect_true(cmp$equal_fitted)
    expect_true(cmp$equal_sigma2)
  }
  cmp <- compare_encodings(mtcars$mpg, mtcars$cyl, x = mtcars$wt)
  expect_true(cmp$equal_fitted)
})

test_that("cambiar el nivel de referencia no cambia las predicciones", {
  d <- make_unbalanced()
  f1 <- fit_encoding(d$y, d$g, "dummy", ref = "A")
  f2 <- fit_encoding(d$y, d$g, "dummy", ref = "D")
  expect_equal(f1$fitted, f2$fitted, tolerance = 1e-9)
  expect_false(isTRUE(all.equal(unname(f1$coefficients), unname(f2$coefficients))))
})

test_that("beta_hat = M^{-1} mu_hat (sin covariable)", {
  d <- make_unbalanced()
  for (tp in ENCODING_TYPES) {
    f <- fit_encoding(d$y, d$g, tp)
    expect_equal(unname(f$coefficients), unname(drop(f$Minv %*% f$group_means)),
                 tolerance = 1e-9)
  }
})

test_that("coeficientes y EE coinciden con lm() y los contrastes de R", {
  d <- make_unbalanced()
  df <- data.frame(y = d$y, g = d$g, x = d$x)
  cmap <- c(dummy = "contr.treatment", effects = "contr.sum", helmert = "contr.helmert")
  for (tp in ENCODING_TYPES) {
    f <- fit_encoding(df$y, df$g, tp)               # ref por defecto = convencion de R
    m <- lm(y ~ g, data = df, contrasts = list(g = cmap[[tp]]))
    expect_equal(unname(f$coefficients), unname(coef(m)), tolerance = 1e-8)
    expect_equal(unname(f$se), unname(summary(m)$coefficients[, 2]), tolerance = 1e-8)
    expect_equal(f$sigma2, summary(m)$sigma^2, tolerance = 1e-8)
  }
  # con covariable: y ~ g * x tiene el mismo orden de columnas [1, C, x, x*C]
  for (tp in ENCODING_TYPES) {
    f <- fit_encoding(df$y, df$g, tp, x = df$x)
    m <- lm(y ~ g * x, data = df, contrasts = list(g = cmap[[tp]]))
    expect_equal(unname(f$coefficients), unname(coef(m)), tolerance = 1e-8)
  }
})

test_that("Var(beta) = sigma2 * Minv diag(1/n_g) Minv'", {
  d <- make_unbalanced()
  for (tp in ENCODING_TYPES) {
    f <- fit_encoding(d$y, d$g, tp)
    V2 <- f$sigma2 * f$Minv %*% diag(1 / f$n_g) %*% t(f$Minv)
    expect_equal(unname(f$vcov), unname(V2), tolerance = 1e-9)
  }
})

test_that("Helmert: coeficientes no correlacionados solo con diseno balanceado", {
  # ToothGrowth: 20 obs por dosis (balanceado)
  fb <- fit_encoding(ToothGrowth$len, factor(ToothGrowth$dose), "helmert")
  Vb <- fb$vcov; offb <- Vb[upper.tri(Vb)]
  expect_lt(max(abs(offb)), 1e-10)
  # desbalanceado: aparece correlacion
  d <- make_unbalanced()
  fu <- fit_encoding(d$y, d$g, "helmert")
  Vu <- fu$vcov
  expect_gt(max(abs(Vu[upper.tri(Vu)])), 1e-3)
})

test_that("interpretacion: filas de Minv = lecturas esperadas", {
  d <- make_unbalanced()
  K <- 5
  fd <- fit_encoding(d$y, d$g, "dummy")
  expect_equal(unname(fd$Minv[1, ]), c(1, 0, 0, 0, 0))
  expect_equal(unname(fd$Minv[3, ]), c(-1, 0, 1, 0, 0))
  fe <- fit_encoding(d$y, d$g, "effects")
  expect_equal(unname(fe$Minv[1, ]), rep(1 / K, K), tolerance = 1e-12)
  expect_equal(unname(rowSums(fe$Minv[-1, ])), rep(0, K - 1), tolerance = 1e-12)
  fh <- fit_encoding(d$y, d$g, "helmert")
  # coef j estima (mu_{j+1} - prom(mu_1..mu_j)) / (j+1)
  j <- 2
  esperado <- c(rep(-1 / j, j), 1, rep(0, K - j - 1)) / (j + 1)
  expect_equal(unname(fh$Minv[j + 1, ]), esperado, tolerance = 1e-12)
  expect_equal(nrow(describe_contrasts(fh)), K)
})

test_that("centrar x cambia el bloque de niveles, no las pendientes ni las predicciones", {
  d <- make_unbalanced(); K <- 5
  a <- fit_encoding(d$y, d$g, "dummy", x = d$x, x_center = 0)
  b <- fit_encoding(d$y, d$g, "dummy", x = d$x, x_center = mean(d$x))
  expect_equal(a$fitted, b$fitted, tolerance = 1e-9)
  expect_equal(unname(a$coefficients[K + 1:K]), unname(b$coefficients[K + 1:K]), tolerance = 1e-9)
  expect_gt(max(abs(a$coefficients[2:K] - b$coefficients[2:K])), 1e-3)
})

test_that("errores informativos", {
  expect_error(fit_encoding(1:5, rep("a", 5)), "2 niveles")
  g <- factor(c("a", "a", "b", "b", "c"))
  expect_error(fit_encoding(c(1, 2, 3, 5, 4), g, x = c(1, 2, 3, 4, 5)))  # singular
  expect_error(prepare_data(iris, "Species", "Sepal.Length"), "numerica")
})
