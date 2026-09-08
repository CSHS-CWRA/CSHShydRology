# Tests for the two Adjusted and Homogenized Canadian Climate Data readers.
#
# Both work out what they are reading from the file name and how many header
# lines to skip by counting keywords, so the fixtures deliberately reproduce the
# bilingual header layout of the real ECCC files. See
# data-raw/make_test_fixtures.R.

test_that("ch_read_AHCCD_daily reads a daily maximum temperature file", {
  daily <- ch_read_AHCCD_daily(fixture_path("dx_ahccd_daily_sample.txt"))

  expect_s3_class(daily, "data.frame")
  # The value column is named after the variable inferred from the file name
  expect_named(daily, c("date", "tmax", "code"))
  expect_s3_class(daily$date, "Date")
  expect_type(daily$tmax, "double")
})

test_that("ch_read_AHCCD_daily keeps only real calendar dates", {
  daily <- ch_read_AHCCD_daily(fixture_path("dx_ahccd_daily_sample.txt"))

  # The file stores 31 slots per month; the short months are padded and must be
  # dropped. 2018-2020 inclusive is 1096 days (2020 is a leap year).
  expect_equal(nrow(daily), 1096)
  expect_false(anyNA(daily$date))
  expect_equal(min(daily$date), as.Date("2018-01-01"))
  expect_equal(max(daily$date), as.Date("2020-12-31"))
  # dates must come out in order, one per day
  expect_true(all(diff(daily$date) == 1))
})

test_that("ch_read_AHCCD_daily recodes the -9999.9 missing flag as NA", {
  daily <- ch_read_AHCCD_daily(fixture_path("dx_ahccd_daily_sample.txt"))

  # The fixture has exactly one genuinely missing observation, on 2018-03-10
  expect_equal(sum(is.na(daily$tmax)), 1)
  expect_true(is.na(daily$tmax[daily$date == as.Date("2018-03-10")]))
  # and no -999 sentinel should survive into the data
  expect_true(all(daily$tmax > -999, na.rm = TRUE))
})

test_that("ch_read_AHCCD_daily carries the quality codes through", {
  daily <- ch_read_AHCCD_daily(fixture_path("dx_ahccd_daily_sample.txt"))

  expect_type(daily$code, "character")
  expect_true("E" %in% daily$code)
})

test_that("ch_read_AHCCD_daily infers precipitation from a dt file name", {
  # Precipitation columns are one character wider than temperature columns, so
  # this exercises a different set of field widths.
  precip <- ch_read_AHCCD_daily(fixture_path("dt_ahccd_daily_sample.txt"))

  expect_named(precip, c("date", "precip", "code"))
  expect_equal(nrow(precip), 1096)
  expect_false(anyNA(precip$date))
})

test_that("ch_read_AHCCD_daily rejects bad arguments", {
  expect_error(ch_read_AHCCD_daily(""), "not specified")
  expect_error(
    ch_read_AHCCD_daily(file.path(tempdir(), "dx_nope.txt")),
    "not found"
  )

  # A file name that encodes no recognised variable
  unknown <- tempfile("zz_unknown", fileext = ".txt")
  writeLines("nothing to see here", unknown)
  expect_error(ch_read_AHCCD_daily(unknown), "Unrecognised file type")
})


test_that("ch_read_AHCCD_monthly reads a monthly precipitation file", {
  monthly <- ch_read_AHCCD_monthly(fixture_path("mt_ahccd_monthly_sample.txt"))

  expect_s3_class(monthly, "data.frame")
  expect_named(monthly, c("year", "month", "precip", "code"))
  expect_type(monthly$precip, "double")
})

test_that("ch_read_AHCCD_monthly returns twelve rows for every year", {
  monthly <- ch_read_AHCCD_monthly(fixture_path("mt_ahccd_monthly_sample.txt"))

  # 2011-2020 inclusive
  expect_equal(nrow(monthly), 120)
  expect_equal(range(as.numeric(monthly$year)), c(2011, 2020))
  expect_equal(as.vector(table(monthly$year)), rep(12L, 10))
  expect_equal(sort(unique(monthly$month)), sprintf("%02d", 1:12))
})

test_that("ch_read_AHCCD_monthly recodes the -9999.9 missing flag as NA", {
  monthly <- ch_read_AHCCD_monthly(fixture_path("mt_ahccd_monthly_sample.txt"))

  # The fixture has one missing month, February 2011
  expect_equal(sum(is.na(monthly$precip)), 1)
  missing_row <- monthly[is.na(monthly$precip), ]
  expect_equal(as.numeric(missing_row$year), 2011)
  expect_equal(missing_row$month, "02")
})

test_that("ch_read_AHCCD_monthly reads a minimum temperature file", {
  # Regression test: the "mn" branch assigned to `vals_type` rather than
  # `val_type`, so naming the result failed with
  # "object 'val_type' not found" and tmin files could not be read at all.
  monthly <- ch_read_AHCCD_monthly(fixture_path("mn_ahccd_monthly_sample.txt"))

  expect_s3_class(monthly, "data.frame")
  expect_named(monthly, c("year", "month", "tmin", "code"))
  expect_equal(nrow(monthly), 120)
})

test_that("ch_read_AHCCD_monthly rejects bad arguments", {
  expect_error(ch_read_AHCCD_monthly(""), "not specified")
  expect_error(
    ch_read_AHCCD_monthly(file.path(tempdir(), "mt_nope.txt")),
    "not found"
  )

  unknown <- tempfile("zz_unknown", fileext = ".txt")
  writeLines("nothing to see here", unknown)
  expect_error(ch_read_AHCCD_monthly(unknown), "Unrecognised file type")
})
