context("Testing ch_get_AHCCD_monthly")

test_that("ch_get_AHCCD_monthly correctly returns values", {
  prov <- "SK"             # Sask
  station <- "4057120"     # Saskatoon
  var <- "PCP"             # precip
  stoon_monthly_precip <- ch_get_AHCCD_monthly(station, prov, var)
  returned_ID <- stoon_monthly_precip[1, 1]
  measure_type <- stoon_monthly_precip[1, 5]
  expect_true(returned_ID == station & measure_type == "total_precip")
})
