# ch_rfa_julianplot draws an empty circular "rose" plot by day of year, which
# the other FloodNet seasonality functions then add points to.

test_that("ch_rfa_julianplot draws without error", {
  expect_draws(ch_rfa_julianplot())
})

test_that("ch_rfa_julianplot accepts custom rose styling", {
  expect_draws(ch_rfa_julianplot(rose.col = "blue", rose.lwd = 2,
                                 rose.cex = 1.0,
                                 rose.radius = seq(0.2, 1, 0.2)))
})

test_that("ch_rfa_julianplot accepts a single radius", {
  expect_draws(ch_rfa_julianplot(rose.radius = 1))
})

test_that("ch_rfa_julianplot can be overplotted with seasonal statistics", {
  # The plot exists so that seasonality statistics can be added to it, so this
  # checks the two work together.
  data(flowAtlantic, envir = environment())
  stat <- ch_rfa_seasonstat(date ~ id, flowAtlantic$ams)

  expect_draws({
    ch_rfa_julianplot()
    points(stat[, "x"], stat[, "y"], pch = 16)
  })
})
