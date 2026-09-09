# ch_rfa_distseason is generic, with methods for numeric, matrix, data.frame and
# formula input. test_ch_rfa_distseason.R (Martin Durocher's original) checks the
# formula method against a hand-rolled reference; this file covers the other
# three and the properties every method must satisfy.

test_that("ch_rfa_distseason.numeric returns a square symmetric matrix", {
  set.seed(20)
  n <- 25
  radius <- runif(n)
  angle <- runif(n, 0, 2 * pi)

  d <- ch_rfa_distseason(radius, angle)

  expect_true(is.matrix(d))
  expect_equal(dim(d), c(n, n))
  # a distance matrix: symmetric, zero on the diagonal, non-negative
  expect_equal(d, t(d))
  expect_equal(unname(diag(d)), rep(0, n))
  expect_true(all(d >= 0))
})

test_that("all four ch_rfa_distseason methods agree", {
  set.seed(21)
  n <- 20
  coords <- data.frame(radius = runif(n), angle = runif(n, 0, 2 * pi))

  from_numeric <- ch_rfa_distseason(coords$radius, coords$angle)
  from_matrix  <- ch_rfa_distseason(as.matrix(coords))
  from_df      <- ch_rfa_distseason(coords)
  from_formula <- ch_rfa_distseason(radius ~ angle, coords)

  expect_equal(from_matrix, from_numeric)
  expect_equal(from_df, from_numeric)
  expect_equal(from_formula, from_numeric)
})

test_that("ch_rfa_distseason rejects mismatched coordinate lengths", {
  expect_error(
    ch_rfa_distseason(c(0.1, 0.2, 0.3), c(1, 2)),
    "same length"
  )
})

test_that("ch_rfa_distseason treats the season as circular", {
  # Two dates either side of new year are close in seasonal space even though
  # their angles are nearly 2*pi apart. Radius is held equal so only the angular
  # term contributes.
  eps <- 0.05
  d <- ch_rfa_distseason(c(0.5, 0.5), c(eps, 2 * pi - eps))

  # the angular separation is 2 * eps, standardised by the default w = 1 / pi
  expect_equal(d[1, 2], (2 * eps) / pi, tolerance = 1e-8)
})

test_that("ch_rfa_distseason angular distance never exceeds its maximum", {
  set.seed(22)
  n <- 30
  # identical radii, so distance is purely angular
  d <- ch_rfa_distseason(rep(0.5, n), runif(n, 0, 2 * pi))

  # the largest possible standardised angular separation is pi * (1 / pi) = 1
  expect_lte(max(d), 1 + 1e-8)
})

test_that("ch_rfa_distseason reduces to radial distance at equal angles", {
  radius <- c(0.1, 0.4, 0.9)
  d <- ch_rfa_distseason(radius, rep(1.0, 3))

  expect_equal(unname(d), unname(as.matrix(dist(radius, method = "manhattan"))))
})

test_that("ch_rfa_distseason honours the angular weight w", {
  # w scales the angular term, so a larger w must not shrink distances
  set.seed(23)
  n <- 15
  radius <- rep(0.5, n)
  angle <- runif(n, 0, 2 * pi)

  narrow <- ch_rfa_distseason(radius, angle, w = 1 / (2 * pi))
  wide   <- ch_rfa_distseason(radius, angle, w = 1 / pi)

  expect_true(all(wide >= narrow - 1e-12))
})

test_that("ch_rfa_distseason works on the bundled Atlantic data", {
  data(flowAtlantic, envir = environment())
  stat <- ch_rfa_seasonstat(date ~ id, flowAtlantic$ams)

  d <- ch_rfa_distseason(stat[, c("radius", "angle")])

  expect_true(is.matrix(d))
  expect_equal(nrow(d), nrow(stat))
  expect_equal(d, t(d))
  expect_equal(unname(diag(d)), rep(0, nrow(stat)))
})
