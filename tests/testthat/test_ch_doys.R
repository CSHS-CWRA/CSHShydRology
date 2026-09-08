test_that("ch_doys returns the documented columns", {
  dates <- seq.Date(as.Date("2010-01-01"), as.Date("2012-12-31"), by = 1)
  out <- ch_doys(dates)

  expect_s3_class(out, "data.frame")
  expect_named(out, c("Date", "year", "month", "day", "doy", "wyear", "dwy"))
  expect_equal(nrow(out), length(dates))
  expect_s3_class(out$Date, "Date")
})

test_that("ch_doys computes calendar fields correctly", {
  dates <- as.Date(c("2011-01-01", "2011-03-15", "2011-12-31", "2012-02-29"))
  out <- ch_doys(dates)

  expect_equal(out$year, c(2011, 2011, 2011, 2012))
  expect_equal(out$month, c(1, 3, 12, 2))
  expect_equal(out$day, c(1, 15, 31, 29))
  expect_equal(out$doy, c(1, 74, 365, 60))
})

test_that("ch_doys starts the water year on day 1 of the chosen month", {
  # NOTE ON CONVENTION: ch_doys labels a water year by the calendar year it
  # STARTS in, so October 2011 - September 2012 is wyear 2011. This is the
  # opposite of ch_wtr_yr(), which labels the same span 2012. See
  # test_ch_wtr_yr.R, which pins the other convention.
  out <- ch_doys(as.Date(c("2011-09-30", "2011-10-01", "2011-10-02")),
                 water_yr = 10)

  expect_equal(out$wyear, c(2010, 2011, 2011))
  expect_equal(out$dwy[2], 1)
  expect_equal(out$dwy[3], 2)
})

test_that("ch_doys day of water year increases by one per day", {
  dates <- seq.Date(as.Date("2010-10-01"), as.Date("2011-09-30"), by = 1)
  out <- ch_doys(dates, water_yr = 10)

  expect_equal(out$dwy[1], 1)
  expect_true(all(diff(out$dwy) == 1))
  expect_equal(max(out$dwy), 365)
  expect_equal(length(unique(out$wyear)), 1)
})

test_that("ch_doys and ch_wtr_yr use opposite water year labels", {
  # Both functions compute a water year, but they disagree by one year for the
  # same date and start month: ch_doys names it after the starting calendar
  # year and ch_wtr_yr after the ending one. This test exists to make that
  # difference explicit rather than surprising.
  dates <- as.Date(c("2011-10-01", "2012-09-30"))

  expect_equal(ch_doys(dates, water_yr = 10)$wyear, c(2011, 2011))
  expect_equal(ch_wtr_yr(dates, start_month = 10), c(2012, 2012))
  expect_equal(
    ch_wtr_yr(dates, start_month = 10) - ch_doys(dates, water_yr = 10)$wyear,
    c(1, 1)
  )
})

test_that("ch_doys treats water_yr = 1 as an October water year", {
  dates <- seq.Date(as.Date("2011-01-01"), as.Date("2011-12-31"), by = 1)
  # Documented behaviour: a request for a calendar water year is remapped to 10
  expect_equal(ch_doys(dates, water_yr = 1), ch_doys(dates, water_yr = 10))
})

test_that("ch_doys accepts other water year start months", {
  out <- ch_doys(as.Date(c("2011-03-31", "2011-04-01")), water_yr = 4)

  expect_equal(out$wyear, c(2010, 2011))
  expect_equal(out$dwy[2], 1)
})
