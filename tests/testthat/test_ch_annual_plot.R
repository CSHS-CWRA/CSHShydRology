context("Testing ch_annual_plot")

make_ams <- function() {
  data.frame(
    year = c(1991:1993, 1995, 1997:2000),  # gaps at 1994 and 1996
    flow = c(10, 12, 9, 15, 11, 13, 8, 14)
  )
}

test_that("a ggplot object is returned", {
  ams <- make_ams()
  expect_s3_class(ch_annual_plot(ams$year, ams$flow), "ggplot")
})

test_that("columns of `data` can be referred to by name", {
  ams <- make_ams()
  p_cols <- ch_annual_plot(year, flow, data = ams)
  p_vecs <- ch_annual_plot(ams$year, ams$flow)
  expect_equal(p_cols$data, p_vecs$data)
})

test_that("missing years are filled with NA so the line breaks", {
  ams <- make_ams()
  d <- ch_annual_plot(year, flow, data = ams)$data
  # every year in the range is present, including the two gaps
  expect_equal(d$year, 1991:2000)
  expect_true(is.na(d$flow[d$year == 1994]))
  expect_true(is.na(d$flow[d$year == 1996]))
  expect_equal(d$flow[d$year == 1995], 15)
})

test_that("dates are reduced to their year", {
  dates <- as.Date(c("1991-06-02", "1992-05-14", "1993-07-30"))
  d <- ch_annual_plot(dates, c(1, 2, 3))$data
  expect_equal(d$year, 1991:1993)
})

test_that("expressions are evaluated within `data`", {
  ams <- make_ams()
  d <- ch_annual_plot(year, 2 * flow, data = ams)$data
  expect_equal(d$flow[d$year == 1991], 20)
})

test_that("inputs of incompatible length are rejected", {
  expect_error(ch_annual_plot(1991:1995, c(1, 2)))
})

test_that("it works on a real annual maximum series", {
  ams <- ch_rfa_extractamax(Flow ~ Date, CAN05AA008, tol = 350)
  p <- ch_annual_plot(Date, Flow, data = ams)
  expect_s3_class(p, "ggplot")
  expect_true(nrow(p$data) >= nrow(ams))
})
