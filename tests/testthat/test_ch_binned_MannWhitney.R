test_that("ch_binned_MannWhitney returns the documented list", {
  b <- suppressMessages(
    ch_binned_MannWhitney(CAN05AA008, step = 5,
                          range1 = c(1970, 1979), range2 = c(1990, 1999))
  )

  expect_type(b, "list")
  expect_named(b, c("StationID", "Station_lname", "variable", "bin_width",
                    "range1", "range2", "p_used", "fail", "bin_method",
                    "test_method", "series"))
  expect_equal(b$StationID, "05AA008")
  expect_equal(b$bin_width, 5)
  expect_equal(b$range1, c(1970, 1979))
  expect_equal(b$range2, c(1990, 1999))
  expect_equal(b$p_used, 0.05)
  expect_equal(b$bin_method, "median")
  expect_equal(b$test_method, "Mann-Whitney U")
})

test_that("ch_binned_MannWhitney series has the documented columns", {
  b <- suppressMessages(
    ch_binned_MannWhitney(CAN05AA008, step = 5,
                          range1 = c(1970, 1979), range2 = c(1990, 1999))
  )
  series <- b$series

  expect_s3_class(series, "data.frame")
  expect_named(series, c("period", "median_1", "median_2", "MW_U", "p_value",
                         "s_code"))
  # 365 / 5 = 73 bins
  expect_equal(nrow(series), 73)
  expect_equal(series$period, 1:73)
})

test_that("ch_binned_MannWhitney p-values and codes are in range", {
  b <- suppressMessages(
    ch_binned_MannWhitney(CAN05AA008, step = 5,
                          range1 = c(1970, 1979), range2 = c(1990, 1999))
  )
  series <- b$series

  expect_true(all(series$p_value >= 0 & series$p_value <= 1, na.rm = TRUE))
  expect_true(all(series$s_code %in% c(-1, 0, 1)))
  expect_true(all(series$MW_U >= 0, na.rm = TRUE))
})

test_that("ch_binned_MannWhitney only flags bins below the p threshold", {
  b <- suppressMessages(
    ch_binned_MannWhitney(CAN05AA008, step = 5,
                          range1 = c(1970, 1979), range2 = c(1990, 1999),
                          ptest = 0.05)
  )
  series <- b$series

  # every significant bin must have p <= ptest, and vice versa
  expect_true(all(series$p_value[series$s_code != 0] <= 0.05))
  expect_true(all(series$s_code[series$p_value > 0.05] == 0))
})

test_that("ch_binned_MannWhitney bin count follows the step size", {
  b30 <- suppressMessages(
    ch_binned_MannWhitney(CAN05AA008, step = 30,
                          range1 = c(1970, 1979), range2 = c(1990, 1999))
  )

  expect_equal(nrow(b30$series), round(365 / 30))
  expect_equal(b30$bin_width, 30)
})

test_that("ch_binned_MannWhitney sets fail when a period has missing bins", {
  # The 1960s are incomplete in this record, which is the case the documentation
  # calls out as failing.
  b <- suppressMessages(
    ch_binned_MannWhitney(CAN05AA008, step = 5,
                          range1 = c(1960, 1969), range2 = c(1990, 1999))
  )

  expect_true(b$fail)
})

test_that("ch_binned_MannWhitney reports which range is incomplete", {
  expect_message(
    ch_binned_MannWhitney(CAN05AA008, step = 5,
                          range1 = c(1960, 1969), range2 = c(1990, 1999)),
    "Range_1 contains missing values"
  )
})

test_that("ch_binned_MannWhitney records a custom variable name", {
  b <- suppressMessages(
    ch_binned_MannWhitney(CAN05AA008, step = 5,
                          range1 = c(1970, 1979), range2 = c(1990, 1999),
                          variable = "runoff")
  )

  expect_equal(b$variable, "runoff")
})
