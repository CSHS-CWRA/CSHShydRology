test_that("ch_circ_mean_reg returns the documented list", {
  amax <- ch_sh_get_amax(test_flows())

  out <- ch_circ_mean_reg(amax)

  expect_type(out, "list")
  expect_named(out, c("n", "mean", "median", "regularity"))
  expect_equal(out$n, nrow(amax))
})

test_that("ch_circ_mean_reg returns mean and median as days of year", {
  # Regression test. The median used to be returned in degrees rather than days
  # (it was scaled by 365/365 while the mean was scaled by 365/360), and the
  # underlying value came from circular::median.circular(), which for these data
  # returns a direction roughly opposite the true median. The result was a
  # "median" flood date in December for a river whose floods are in June.
  amax <- ch_sh_get_amax(test_flows())

  out <- ch_circ_mean_reg(amax)

  expect_gte(out$mean, 1)
  expect_lte(out$mean, 366)
  expect_gte(out$median, 1)
  expect_lte(out$median, 366)
})

test_that("ch_circ_mean_reg mean and median agree for a concentrated series", {
  # The Crowsnest River is snowmelt-driven, so its annual maxima cluster tightly
  # in late spring. For such a series the circular mean and median must land in
  # the same part of the year; a half-year gap indicates the antimedian problem.
  amax <- ch_sh_get_amax(test_flows())
  amax <- amax[amax$days >= 365, ]

  out <- ch_circ_mean_reg(amax)

  expect_lt(abs(out$mean - out$median), 60)
  # and both should sit near the ordinary median day of year
  expect_lt(abs(out$median - stats::median(amax$doy)), 60)
})

test_that("ch_circ_mean_reg recovers a known mean direction", {
  # All events on the same day: the circular mean and median must be that day,
  # and the regularity must be 1 (no spread at all).
  same_day <- data.frame(doy = rep(100, 20), days = rep(365, 20))

  out <- ch_circ_mean_reg(same_day)

  expect_equal(out$mean, 100 * 365 / 365, tolerance = 1e-6)
  expect_equal(out$median, 100 * 365 / 365, tolerance = 1e-6)
  expect_equal(as.numeric(out$regularity), 1, tolerance = 1e-6)
})

test_that("ch_circ_mean_reg regularity falls between 0 and 1", {
  amax <- ch_sh_get_amax(test_flows())

  out <- ch_circ_mean_reg(amax)

  expect_gte(as.numeric(out$regularity), 0)
  expect_lte(as.numeric(out$regularity), 1)
})

test_that("ch_circ_mean_reg regularity is lower for scattered events", {
  # Events spread right around the year are far less regular than clustered ones
  clustered <- data.frame(doy = c(148, 150, 152, 149, 151), days = rep(365, 5))
  scattered <- data.frame(doy = c(10, 100, 190, 280, 350), days = rep(365, 5))

  expect_gt(
    as.numeric(ch_circ_mean_reg(clustered)$regularity),
    as.numeric(ch_circ_mean_reg(scattered)$regularity)
  )
})

test_that("ch_circ_mean_reg handles events either side of new year", {
  # Two events on 31 December and 1 January are one day apart on the circle,
  # so the circular mean must sit at the turn of the year, not in midsummer as a
  # plain arithmetic mean would.
  turn_of_year <- data.frame(doy = c(364, 365, 1, 2), days = rep(365, 4))

  out <- ch_circ_mean_reg(turn_of_year)

  expect_true(out$mean > 350 || out$mean < 15)
})
