#--------------------------------------------
# Author: Margot Pelto, Dec 2025
# Purpose: To check for errors within the function get_eccc_single_station
#--------------------------------------------
station_id = 1026271

#Testing DateTime format
test_that("Date should be a character with format YYYY-MM-DDThh:mm:ss", {
  expect_error(get_eccc_single_station(station_ClimateID =station_id, dt_start=1990), class = "not_character")
  expect_error(get_eccc_single_station(station_ClimateID =station_id, dt_start=c("1990-01-01T00:00:00","1992-01-01T00:00:00")), class = "invalid_formatting")
  expect_error(get_eccc_single_station(station_ClimateID =station_id,dt_start="19900101"), class = "invalid_formatting")
  expect_error(get_eccc_single_station(station_ClimateID =station_id,dt_start="1990-01-01"), class = "invalid_formatting")
  expect_error(get_eccc_single_station(station_ClimateID =station_id, dt_start="1990-01-01T00:00:00",dt_end="19900202T00:00:00"), class = "invalid_formatting")
  expect_error(get_eccc_single_station(station_ClimateID =station_id, dt_start="19900202T00:00:00",dt_end="1990-01-01T00:00:00"), class = "invalid_formatting")
})

#Testing DateTime range
test_that("Date Range Check", {
  expect_error(get_eccc_single_station(station_ClimateID =station_id, dt_start="1990-01-01T00:00:00",dt_end="1989-01-01T00:00:00"), class = "invalid_range")
  expect_error(get_eccc_single_station(station_ClimateID =station_id, dt_start="..",dt_end=".."), class = "invalid_formatting")
})

# Check Verbose value
test_that("Check Verbose argument", {
  expect_error(get_eccc_single_station(station_ClimateID =station_id, dt_start="1990-01-01T00:00:00",dt_end="..",verbose='true'), class = "not_logical")
})

# Check res value
test_that("Check Res Argument", {
  expect_error(get_eccc_single_station(station_ClimateID =station_id, res = 'month'), class = "invalid_argument")
  expect_error(get_eccc_single_station(station_ClimateID =station_id, res = c('month','day')), class = "invalid_argument")
  expect_error(get_eccc_single_station(station_ClimateID =station_id, res = 1), class = "invalid_argument")
})

# Check API Limit value
test_that("Check API Limit", {
  expect_error(get_eccc_single_station(station_ClimateID =station_id, API_limit = 0), class = "invalid_range")
  expect_error(get_eccc_single_station(station_ClimateID =station_id, API_limit = -1), class = "invalid_range")
  expect_error(get_eccc_single_station(station_ClimateID =station_id, API_limit = 11456), class = "invalid_range")
  expect_error(get_eccc_single_station(station_ClimateID =station_id, API_limit = TRUE), class = "invalid_range")
})
