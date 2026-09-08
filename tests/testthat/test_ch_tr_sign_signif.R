# Tests for the two trend index helpers, ch_tr_sign and ch_tr_signif.

test_that("ch_tr_sign converts slopes to 1, 2, 3", {
  # negative -> 1, zero -> 2, positive -> 3
  expect_equal(ch_tr_sign(c(-0.23, 0.34, 0.0, 0.033, -0.55)),
               c(1, 3, 2, 3, 1))
})

test_that("ch_tr_sign ignores slope magnitude", {
  expect_equal(ch_tr_sign(c(-1e-9, -1e9)), c(1, 1))
  expect_equal(ch_tr_sign(c(1e-9, 1e9)), c(3, 3))
})

test_that("ch_tr_sign returns numeric indices of the same length", {
  out <- ch_tr_sign(c(-1, 0, 1, 2, -3))

  expect_type(out, "double")
  expect_length(out, 5)
  expect_true(all(out %in% c(1, 2, 3)))
})

test_that("ch_tr_sign accepts a list and a data frame column", {
  expect_equal(ch_tr_sign(list(-1, 0, 1)), c(1, 2, 3))

  df <- data.frame(slope = c(-1, 0, 1))
  expect_equal(ch_tr_sign(df$slope), c(1, 2, 3))
})

test_that("ch_tr_sign honours a custom offset", {
  # offset shifts the whole index scale
  expect_equal(ch_tr_sign(c(-1, 0, 1), offset = 0), c(-1, 0, 1))
  expect_equal(ch_tr_sign(c(-1, 0, 1), offset = 10), c(9, 10, 11))
})


test_that("ch_tr_signif converts p-values to 1 and 2", {
  # not significant -> 1, significant -> 2
  expect_equal(ch_tr_signif(c(-0.052, 0.34, 0.012, -0.033, -0.55)),
               c(1, 1, 2, 2, 1))
})

test_that("ch_tr_signif treats the threshold itself as significant", {
  expect_equal(ch_tr_signif(0.05), 2)
  expect_equal(ch_tr_signif(0.050001), 1)
})

test_that("ch_tr_signif honours a custom p threshold", {
  p <- c(0.01, 0.05, 0.09)

  expect_equal(ch_tr_signif(p, pvalue = 0.05), c(2, 2, 1))
  expect_equal(ch_tr_signif(p, pvalue = 0.10), c(2, 2, 2))
  expect_equal(ch_tr_signif(p, pvalue = 0.001), c(1, 1, 1))
})

test_that("ch_tr_signif uses the absolute value of its input", {
  # Signed values arise when the sign carries the trend direction
  expect_equal(ch_tr_signif(c(-0.01, 0.01)), c(2, 2))
})

test_that("ch_tr_signif returns numeric indices of the same length", {
  out <- ch_tr_signif(c(0.001, 0.5, 0.04))

  expect_type(out, "double")
  expect_length(out, 3)
  expect_true(all(out %in% c(1, 2)))
})

test_that("ch_tr_sign and ch_tr_signif index their plotting vectors safely", {
  # These functions exist to index symbol/colour vectors, so the returned values
  # must be valid positions in a length-3 and length-2 vector respectively.
  symbols <- c("down", "none", "up")
  colours <- c("grey", "black")

  expect_equal(symbols[ch_tr_sign(c(-1, 0, 1))], c("down", "none", "up"))
  expect_equal(colours[ch_tr_signif(c(0.5, 0.01))], c("grey", "black"))
})
