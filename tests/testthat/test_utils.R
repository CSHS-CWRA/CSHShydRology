# Tests for the small utility functions in R/utils.R:
# ch_stack_EC, ch_date_subset, ch_test_url_file and ch_safe_GET.

test_that("ch_stack_EC flattens month x day frames row-wise", {
  # Two rows of three "days"; stacking transposes then unrolls, so the values
  # come out in row order.
  values <- data.frame(d1 = c(1, 4), d2 = c(2, 5), d3 = c(3, 6))
  codes  <- data.frame(d1 = c("A", "D"), d2 = c("B", "E"), d3 = c("C", "F"))

  stacked <- ch_stack_EC(values, codes)

  expect_s3_class(stacked, "data.frame")
  expect_named(stacked, c("data_values", "data_codes"))
  expect_equal(nrow(stacked), 6)
  expect_equal(stacked$data_values, c(1, 2, 3, 4, 5, 6))
  expect_equal(stacked$data_codes, c("A", "B", "C", "D", "E", "F"))
})

test_that("ch_stack_EC returns numeric values and character codes", {
  values <- data.frame(a = 1.5, b = 2.5)
  codes  <- data.frame(a = "", b = "E")

  stacked <- ch_stack_EC(values, codes)

  expect_type(stacked$data_values, "double")
  expect_type(stacked$data_codes, "character")
})

test_that("ch_stack_EC requires both arguments", {
  df <- data.frame(a = 1)
  expect_error(ch_stack_EC(), "No specified data values")
  expect_error(ch_stack_EC(data_values = df), "No specified data codes")
  expect_error(ch_stack_EC(data_codes = df), "No specified data values")
})


test_that("ch_date_subset keeps only the requested date range", {
  dates <- seq.Date(as.Date("2010-10-01"), as.Date("2013-09-30"), by = 1)
  df <- data.frame(Date = dates, x = seq_along(dates))

  sub <- ch_date_subset(df, "2011-10-01/2012-09-30")

  expect_s3_class(sub, "data.frame")
  expect_equal(min(sub$Date), as.Date("2011-10-01"))
  expect_equal(max(sub$Date), as.Date("2012-09-30"))
  # 2012 is a leap year, so this water year is 366 days
  expect_equal(nrow(sub), 366)
  expect_named(sub, c("Date", "x"))
})

test_that("ch_date_subset includes both endpoints", {
  dates <- seq.Date(as.Date("2020-01-01"), as.Date("2020-01-31"), by = 1)
  df <- data.frame(Date = dates, x = seq_along(dates))

  sub <- ch_date_subset(df, "2020-01-05/2020-01-10")

  expect_equal(nrow(sub), 6)
  expect_true(as.Date("2020-01-05") %in% sub$Date)
  expect_true(as.Date("2020-01-10") %in% sub$Date)
})

test_that("ch_date_subset returns no rows for a range outside the data", {
  dates <- seq.Date(as.Date("2020-01-01"), as.Date("2020-01-31"), by = 1)
  df <- data.frame(Date = dates, x = seq_along(dates))

  expect_equal(nrow(ch_date_subset(df, "1990-01-01/1990-12-31")), 0)
})


test_that("ch_test_url_file reports OK for a readable local file", {
  # A file:// URL exercises the success path without touching the network.
  path <- fixture_path("ecde_flows_sample.csv")
  expect_equal(ch_test_url_file(path, quiet = TRUE), "OK")
})

test_that("ch_test_url_file reports a problem for an unreadable location", {
  missing <- file.path(tempdir(), "no-such-file-at-all.csv")
  # readLines() on a missing file warns before it errors, so either string is a
  # legitimate answer; what matters is that it is not "OK".
  expect_true(ch_test_url_file(missing, quiet = TRUE) %in% c("error", "warning"))
})

test_that("ch_test_url_file messages unless quiet", {
  path <- fixture_path("ecde_flows_sample.csv")
  expect_message(ch_test_url_file(path, quiet = FALSE), "Processed URL")
})


test_that("ch_safe_GET returns the error message rather than stopping", {
  # An unroutable host: the point is that a failure comes back as a string, so
  # callers such as ch_get_url_data can decide what to do about it.
  out <- ch_safe_GET("http://localhost:1/definitely-not-served",
                     tempfile(fileext = ".csv"))

  expect_type(out, "character")
  expect_length(out, 1)
  expect_false(out == "OK")
})
