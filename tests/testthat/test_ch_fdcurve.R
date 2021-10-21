context("Testing ch_fdcurve")

test_that("ch_fdcurve correctly returns values", {
  flow <- CAN05AA008
  test <- ch_fdcurve(flow, normal = FALSE, gust = TRUE)
  expect_true(test$flow[1] <= 9.78 & test$flow[25] >= 11.9)
})
