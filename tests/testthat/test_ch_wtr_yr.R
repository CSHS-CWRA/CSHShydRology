context("Testing ch_wtr_yr")

test_that("ch_wtr_yr correctly categorizes water year", {
  month <- 10
  date <- seq(as.Date("1910/1/1"), as.Date("1912/1/1"), "days")
  wtr_yr <- ch_wtr_yr(dates = date, start_month = month)
  comp_df <- data.frame(wtr_yr,date)
  expect_true(comp_df[comp_df$date == as.Date("1911-10-01"),]$wtr_yr == 1912)
})
