test_that("ch_high_Grubbs_test returns a data frame in the original order", {
  set.seed(1234)
  x <- c(rnorm(10), 15, 35)

  out <- ch_high_Grubbs_test(x)

  expect_s3_class(out, "data.frame")
  expect_named(out, c("tindex", "tmax", "tout"))
  expect_equal(nrow(out), length(x))
  # tindex records the original position, and the rows come back in that order
  expect_equal(out$tindex, seq_along(x))
  expect_equal(out$tmax, x)
})

test_that("ch_high_Grubbs_test flags obvious high outliers", {
  set.seed(1234)
  x <- c(rnorm(10), 15, 35)

  out <- ch_high_Grubbs_test(x)

  expect_equal(out$tout[11:12], c(1, 1))
  expect_true(all(out$tout[1:10] == 0))
})

test_that("ch_high_Grubbs_test flags nothing in a clean sample", {
  set.seed(42)
  x <- rnorm(30)

  out <- ch_high_Grubbs_test(x)

  expect_true(all(out$tout == 0))
})

test_that("ch_high_Grubbs_test only ever returns 0 or 1 in tout", {
  set.seed(7)
  out <- ch_high_Grubbs_test(c(rnorm(20), 25))

  expect_true(all(out$tout %in% c(0, 1)))
})

test_that("ch_high_Grubbs_test flags the largest value, not an arbitrary one", {
  set.seed(99)
  x <- c(rnorm(20), 40)

  out <- ch_high_Grubbs_test(x)

  flagged <- out$tmax[out$tout == 1]
  if (length(flagged) > 0) {
    # anything flagged must be at the top of the sample
    expect_true(all(flagged >= sort(x, decreasing = TRUE)[length(flagged)]))
  }
})
