context("Testing fdcurve")

test_that("fdcurve correctly returns values", {
  flow <- W05AA008$Flow
  test <- fdcurve(flow, title="Station", normal=FALSE, gust=TRUE)
  expect_true(test$flow[1] <= 9.78 & test$flow[25] >= 11.9)
})
