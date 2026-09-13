context("Testing Gumbel-transformed axes")

test_that("ch_gumbel_rp_trans returns a fresh transform object", {
  tr <- ch_gumbel_rp_trans()
  expect_s3_class(tr, "transform")
  expect_identical(tr$name, "gumbel_rp")
  # built on demand, not stored: two calls give equal but distinct objects
  expect_equal(ch_gumbel_rp_trans()$transform(10), tr$transform(10))
})

test_that("ch_gumbel_aep_trans returns a fresh transform object", {
  tr <- ch_gumbel_aep_trans()
  expect_s3_class(tr, "transform")
  expect_identical(tr$name, "gumbel_aep")
})

test_that("the return period transform matches the reduced Gumbel variate", {
  tr <- ch_gumbel_rp_trans()
  rp <- c(2, 5, 10, 100)
  expect_equal(tr$transform(rp), -log(-log(1 - 1 / rp)))
  # a 2-year return period is the median: -log(-log(0.5))
  expect_equal(tr$transform(2), -log(-log(0.5)))
})

test_that("the AEP transform matches the reduced Gumbel variate", {
  tr <- ch_gumbel_aep_trans()
  aep <- c(0.5, 0.1, 0.01)
  expect_equal(tr$transform(aep), -log(-log(1 - aep)))
})

test_that("transform and inverse round-trip", {
  rp <- c(1.5, 2, 10, 200)
  tr <- ch_gumbel_rp_trans()
  expect_equal(tr$inverse(tr$transform(rp)), rp)

  aep <- c(0.9, 0.5, 0.02)
  tra <- ch_gumbel_aep_trans()
  expect_equal(tra$inverse(tra$transform(aep)), aep)
})

test_that("return period and AEP are consistent with one another", {
  aep <- c(0.5, 0.2, 0.01)
  expect_equal(ch_gumbel_aep_trans()$transform(aep),
               ch_gumbel_rp_trans()$transform(1 / aep))
})

test_that("the scale functions build a usable ggplot layer", {
  df <- data.frame(aep = 1:20 / 21, y = sort(stats::rexp(20), decreasing = TRUE))
  for (sc in list(scale_x_gumbel_aep(), scale_y_gumbel_aep())) {
    expect_s3_class(sc, "ScaleContinuousPosition")
  }
  p <- ggplot2::ggplot(df, ggplot2::aes(aep, y)) +
    ggplot2::geom_point() +
    scale_x_gumbel_aep()
  expect_silent(built <- ggplot2::ggplot_build(p))
  expect_s3_class(p, "ggplot")
})

test_that("arguments are passed through to the underlying ggplot2 scale", {
  sc <- scale_x_gumbel_rp("Return Period")
  expect_identical(sc$name, "Return Period")
})
