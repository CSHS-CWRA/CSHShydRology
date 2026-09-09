test_that("ch_sh_get_amax returns one row per year with the documented columns", {
  flows <- test_flows()

  amax <- ch_sh_get_amax(flows)

  expect_s3_class(amax, "data.frame")
  expect_named(amax, c("Year", "amax", "maxdate", "doy", "days"))
  expect_equal(nrow(amax), length(unique(format(flows$Date, "%Y"))))
  expect_s3_class(amax$maxdate, "Date")
})

test_that("ch_sh_get_amax finds the true annual maximum", {
  flows <- test_flows()
  amax <- ch_sh_get_amax(flows)

  for (i in seq_len(nrow(amax))) {
    year_rows <- flows[format(flows$Date, "%Y") == as.character(amax$Year[i]), ]
    expect_equal(amax$amax[i], max(year_rows$Flow, na.rm = TRUE))
  }
})

test_that("ch_sh_get_amax reports the date on which the maximum occurred", {
  flows <- test_flows()
  amax <- ch_sh_get_amax(flows)

  # The flow recorded on maxdate must equal the reported annual maximum
  for (i in seq_len(nrow(amax))) {
    on_that_day <- flows$Flow[flows$Date == amax$maxdate[i]]
    expect_equal(on_that_day, amax$amax[i])
  }
  # and the year of maxdate must be the year it is filed under
  expect_equal(as.numeric(format(amax$maxdate, "%Y")), amax$Year)
})

test_that("ch_sh_get_amax day of year agrees with the date", {
  amax <- ch_sh_get_amax(test_flows())

  expect_true(all(amax$doy >= 1 & amax$doy <= 366))
  expect_equal(amax$doy, as.numeric(format(amax$maxdate, "%j")))
})

test_that("ch_sh_get_amax counts the observations in each year", {
  flows <- test_flows()
  amax <- ch_sh_get_amax(flows)

  # A decade of complete years: 365 days, or 366 in leap years
  expect_true(all(amax$days %in% c(365, 366)))
  expect_equal(sum(amax$days), nrow(flows))
})

test_that("ch_sh_get_amax flags an incomplete year through `days`", {
  # Trim the last year so it is partial; `days` is what callers use to filter
  # partial years out before circular statistics.
  flows <- test_flows("1990-01-01", "1999-06-30")
  amax <- ch_sh_get_amax(flows)

  expect_lt(amax$days[amax$Year == 1999], 365)
})
